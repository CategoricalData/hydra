-- | Coq code generator: transforms Hydra kernel modules into .v files.
-- Produces dist/coq/hydra-kernel/src/main/coq/hydra/*.v

module Hydra.Coq.Generate where

import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Pkg
import qualified Hydra.Coq.Syntax as Coq
import qualified Hydra.Coq.Coder as Coder
import qualified Hydra.Coq.Print as Print
import qualified Hydra.Context as Context
import qualified Hydra.Graph as Graph
import qualified Hydra.Errors as Errors
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util

import Data.List (intercalate, isPrefixOf, nub, isInfixOf)
import Data.Char (toUpper, toLower, isAlphaNum, isDigit)
import Data.Maybe (fromMaybe)
import Data.Graph (stronglyConnComp, SCC(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

-- | Standard coder interface: takes an adapted Module and Definitions,
-- returns a map from file paths to file contents.
-- This function is designed to be used with generateSources from Hydra.Generation.
-- | Global field mapping, built once from all kernel modules.
-- Must be initialized before calling moduleToCoq.
globalFieldMapping :: [Pkg.Module] -> Map.Map (String, String) String
globalFieldMapping = buildFieldMapping

-- | Global constructor count map, built once from all kernel modules.
globalConstructorCounts :: [Pkg.Module] -> Map.Map String Int
globalConstructorCounts modules =
  let allTypeDefs = concatMap (\m ->
        [(localName (Core.unName $ Pkg.typeDefinitionName td),
          Core.typeSchemeType (Pkg.typeDefinitionType td))
        | Pkg.DefinitionType td <- Pkg.moduleDefinitions m]) modules
  in buildConstructorCounts allTypeDefs

-- | Global set of sanitized field accessor names across all modules.
-- These are accessors for record fields that had to be sanitized to 'unit' due to
-- Coq's strict positivity requirement. At call sites, we replace applications of
-- these accessors with hydra_unreachable.
globalSanitizedAccessors :: [Pkg.Module] -> Set.Set String
globalSanitizedAccessors modules =
  let allTypeGroups = concatMap (\m ->
        let ns = Pkg.unNamespace (Pkg.moduleNamespace m)
            typeDefs = [(localName (Core.unName $ Pkg.typeDefinitionName td),
                         Core.typeSchemeType (Pkg.typeDefinitionType td))
                       | Pkg.DefinitionType td <- Pkg.moduleDefinitions m]
        in sortTypeDefsSCC ns typeDefs) modules
  in collectSanitizedAccessors allTypeGroups

-- | Global set of all definition names (local parts), grouped by name.
-- When a name appears in multiple modules, references to it should be kept qualified.
globalAmbiguousNames :: [Pkg.Module] -> Set.Set String
globalAmbiguousNames modules =
  let allNames = concatMap (\m ->
        let ns = Pkg.unNamespace (Pkg.moduleNamespace m)
        in [(localName (Core.unName $ Pkg.typeDefinitionName td), ns)
           | Pkg.DefinitionType td <- Pkg.moduleDefinitions m]
        ++ [(localName (Core.unName $ Pkg.termDefinitionName td), ns)
           | Pkg.DefinitionTerm td <- Pkg.moduleDefinitions m]) modules
      -- Group by name, find names that appear in multiple namespaces
      nameMap = Map.fromListWith (++) [(n, [ns]) | (n, ns) <- allNames]
  in Set.fromList [n | (n, nss) <- Map.toList nameMap, length (nub nss) > 1]

moduleToCoq :: Map.Map (String, String) String -> Map.Map String Int -> Set.Set String -> Set.Set String -> Pkg.Module -> [Pkg.Definition] -> Context.Context -> Graph.Graph
  -> Either Errors.Error (Map.Map FilePath String)
moduleToCoq fieldMap constrCounts ambiguousNames globalSanitizedAcc mod defs _cx _g =
  let ns = Pkg.unNamespace (Pkg.moduleNamespace mod)
      path = namespaceToPath ns
      desc = maybe "" (\d -> "(* " ++ d ++ " *)\n\n") (Pkg.moduleDescription mod)

      -- Modules that exceed Coq's practical typechecking limits: emit axiom stubs
      -- instead of full term definitions. These files have deeply nested polymorphic
      -- code that causes coqc to consume 10GB+ memory during type inference.
      axiomOnlyModules = ["hydra.hoisting", "hydra.inference"]
      isAxiomOnly = ns `elem` axiomOnlyModules

      -- Separate type and term definitions from the adapted defs
      typeDefs = [(localName (Core.unName $ Pkg.typeDefinitionName td),
                   Core.typeSchemeType (Pkg.typeDefinitionType td))
                 | Pkg.DefinitionType td <- defs]
      termDefs = [(localName (Core.unName $ Pkg.termDefinitionName td),
                   Pkg.termDefinitionTerm td,
                   maybe [] Core.typeSchemeVariables (Pkg.termDefinitionType td),
                   fmap Core.typeSchemeType (Pkg.termDefinitionType td))
                 | Pkg.DefinitionTerm td <- defs]

      fm = fieldMap

      -- Sort term defs into SCC groups and rewrite field names
      termDefsForSort = [(n, t) | (n, t, _, _) <- termDefs]
      termDefMap = Map.fromList [(n, (t, vs, mty)) | (n, t, vs, mty) <- termDefs]
      termGroups = sortTermDefsSCC ns termDefsForSort
      -- Enrich each group with type vars and types, and rewrite fields
      termGroups' = map (\(cyc, group) ->
        (cyc, [(n, normalizeInnerTypeLambdas (rewriteTermFields fm t), vs, mty)
              | (n, t) <- group
              , Just (_, vs, mty) <- [Map.lookup n termDefMap]])) termGroups

      -- Build type sentences with SCC-based mutual recursion support
      typeGroups = sortTypeDefsSCC ns typeDefs
      typeSentences = concatMap generateTypeGroup typeGroups

      -- Build term output: mix of AST sentences and raw text for mutual groups.
      -- Each group produces rendered text (either from AST or directly).
      termRenderedParts = map (\(cyc, grp) ->
        if cyc
        then encodeMutualGroupText grp
        else let sentences = concatMap (\td -> encodeTermGroup (False, [td])) grp
             in Print.printDocument (Coq.Document sentences) ++ "\n") termGroups'

      -- For namespace discovery, we still need AST sentences for non-mutual terms
      termSentences = concatMap (\(cyc, grp) ->
        if cyc then [] else concatMap (\td -> encodeTermGroup (False, [td])) grp) termGroups'

      -- Helper: compute type binders for a term definition.
      -- Only hoist inner type variables when the type scheme already has type params.
      -- For monomorphic functions (empty typeVars), inner type vars are artifacts of
      -- Hydra's generic type inference and should not become Coq definition parameters.
      mkTypeBinders body typeVars =
        let schemeVarNames = Set.fromList (map Core.unName typeVars)
            innerTypeVars = if null typeVars then Set.empty else collectFreeTypeVars body
            allTypeVarNames = nub (map Core.unName typeVars ++ filter (`Set.notMember` schemeVarNames) (Set.toList innerTypeVars))
        in (allTypeVarNames, map (\v -> Coq.BinderType (Coq.TypeBinders
              [Coder.coqName v]
              (Coq.Type (Coder.coqTermQualid "Type")))) allTypeVarNames)

      -- Helper: compute return type from TypeScheme
      mkReturnType mType = fmap (\ty ->
        let (_, bodyTy) = extractTypeParams ty
        in Coq.Type (Coder.encodeType bodyTy)) mType

      -- Encode a term group: singleton → Definition, cycle → Fixpoint ... with ...
      encodeTermGroup (False, [(name, body, typeVars, mType)]) =
        -- Acyclic singleton: Definition
        -- Erase inner lambda domains that reference type variables not in the type scheme.
        -- These are artifacts of Hydra's generic type inference.
        let schemeVarNames = Set.fromList (map Core.unName typeVars)
            body' = reorderLetBindings $ eraseUnboundTypeVarDomains schemeVarNames body
            showLets' tm = case tm of
              Core.TermLambda lam -> "fun " ++ Core.unName (Core.lambdaParameter lam) ++ " => " ++ showLets' (Core.lambdaBody lam)
              Core.TermLet v ->
                let bs = Core.letBindings v
                    nms = map (Core.unName . Core.bindingName) bs
                in "let [" ++ unwords nms ++ "] in " ++ showLets' (Core.letBody v)
              _ -> "..."
            coqBody = Coder.encodeTerm body'
            showFirstBindingVars tm = case tm of
              Core.TermLambda lam -> showFirstBindingVars (Core.lambdaBody lam)
              Core.TermLet v ->
                let b = head (Core.letBindings v)
                    bname = Core.unName (Core.bindingName b)
                in bname ++ " refs: " ++ showVarRefs (Core.bindingTerm b)
              _ -> "not a let"
            showVarRefs tm = case tm of
              Core.TermVariable v -> Core.unName v ++ " "
              Core.TermApplication v -> showVarRefs (Core.applicationFunction v) ++ showVarRefs (Core.applicationArgument v)
              Core.TermLambda lam -> showVarRefs (Core.lambdaBody lam)
              _ -> ""
            (_, typeBinders) = mkTypeBinders body' typeVars
            returnType = mkReturnType mType
        in [Coq.Sentence Nothing $ Coq.SentenceContentDefinition $ Coq.Definition
              Nothing (Coder.coqIdent name) typeBinders returnType coqBody]
      encodeTermGroup (True, group) =
        -- Mutually recursive group: use hydra_fix with product encoding.
        -- Returns empty AST sentences — the actual text is generated separately
        -- and injected into mutualGroupTexts.
        []
      encodeTermGroup _ = []

      -- Generate text for all mutual groups (bypassing the AST)
      mutualGroupTexts = concatMap (\(cyc, grp) ->
        if cyc then [encodeMutualGroupText grp] else []) termGroups'

      -- Encode a mutually recursive group as raw Coq text using hydra_fix on nested pairs.
      encodeMutualGroupText group =
        let groupSchemeVars = Set.fromList $ concatMap (\(_, _, tv, _) -> map Core.unName tv) group
            funInfos = map (\(name, body, _typeVars, mType) ->
              let body' = reorderLetBindings $ eraseUnboundTypeVarDomains groupSchemeVars body
                  coqBody = Coder.encodeTerm body'
                  bodyText = Print.printTerm coqBody
                  typeText = case mType of
                    Just ty -> let (_, bodyTy) = extractTypeParams ty
                               in Print.printType (Coq.Type (Coder.encodeType bodyTy))
                    Nothing -> "_"
              in (name, typeText, bodyText)) group

            -- Collect type variable binders across the group
            allTypeVarNames = nub $ concatMap (\(_, b, tv, _) -> fst (mkTypeBinders b tv)) group

            names = map (\(n, _, _) -> n) funInfos
            bundleName = intercalate "_" (take 2 names) ++ "_bundle"
            n = length funInfos

            -- Nested product type: T1 * (T2 * ... * Tn)
            types = map (\(_, t, _) -> t) funInfos
            productType = mkProdType types

            -- Projection expressions
            projExprs = mkProjectionExprs n "bundle_"

            -- Let bindings inside hydra_fix
            lets = concat [name ++ " := " ++ proj ++ " in\n    let "
                          | ((name, _, _), proj) <- zip funInfos projExprs]
            -- Remove trailing " in\n    let "
            letBlock = "let " ++ take (length lets - length " in\n    let ") lets ++ " in\n    "

            -- Product value: pair body1 (pair body2 (... bodyn))
            bodies = map (\(_, _, b) -> b) funInfos
            prodVal = mkProdVal bodies

            -- Type binder text
            typBindText = if null allTypeVarNames then ""
              else " " ++ unwords ["(" ++ v ++ " : Type)" | v <- allTypeVarNames]
            implicitArgs = if null allTypeVarNames then ""
              else "Arguments " ++ bundleName ++ " "
                   ++ unwords ["{" ++ v ++ "}" | v <- allTypeVarNames] ++ ".\n"

            -- Bundle definition
            bundleDef = "Definition " ++ bundleName ++ typBindText ++ " :=\n"
              ++ "  hydra_fix (fun (bundle_ : " ++ productType ++ ") =>\n"
              ++ "    " ++ letBlock ++ prodVal ++ ").\n"
              ++ implicitArgs

            -- Projection definitions for each function
            projDefs = concat [
              let projText = projExprs !! i
                  typeText = t
                  argsDef = if null allTypeVarNames then ""
                    else "Arguments " ++ name ++ " "
                         ++ unwords ["{" ++ v ++ "}" | v <- allTypeVarNames] ++ ".\n"
              in "Definition " ++ name ++ typBindText ++ " : " ++ typeText
                 ++ " :=\n  " ++ replaceBundle projText bundleName ++ ".\n"
                 ++ argsDef
              | (i, (name, t, _)) <- zip [0..] funInfos]

        in bundleDef ++ "\n" ++ projDefs

      -- Build nested product type text
      mkProdType [t] = t
      mkProdType (t:ts) = "prod (" ++ t ++ ") (" ++ mkProdType ts ++ ")"
      mkProdType [] = "unit"

      -- Build nested pair value text
      mkProdVal [b] = b
      mkProdVal (b:bs) = "(pair (" ++ b ++ ") (" ++ mkProdVal bs ++ "))"
      mkProdVal [] = "tt"

      -- Build projection expressions for n elements
      mkProjectionExprs n var
        | n <= 0 = []
        | n == 1 = [var]
        | otherwise = [mkProj i n var | i <- [0..n-1]]
        where
          mkProj 0 _ v = "(fst " ++ v ++ ")"
          mkProj i total v
            | i == total - 1 = snds i v
            | otherwise = "(fst " ++ snds i v ++ ")"
          snds 0 v = v
          snds k v = snds (k-1) ("(snd " ++ v ++ ")")

      -- Replace "bundle_" with the actual bundle name in projection text
      replaceBundle [] _ = []
      replaceBundle s bname
        | "bundle_" `isPrefixOf` s = bname ++ replaceBundle (drop (length "bundle_") s) bname
        | otherwise = head s : replaceBundle (tail s) bname

      -- Render type sentences and all term parts for namespace discovery
      typeSentencesText = Print.printDocument (Coq.Document typeSentences)
      allTermText = concat termRenderedParts
      rawBody = typeSentencesText ++ "\n" ++ allTermText

      -- Discover all referenced hydra.xxx namespaces from the body
      referencedNs = extractReferencedNamespaces ns rawBody
      depSentences = dependencyImports referencedNs

      -- Assemble: imports + type sentences + term parts (interleaved in SCC order)
      importText = Print.printDocument (Coq.Document (Coder.standardImports : depSentences))
      bodyWithImports = importText ++ "\n" ++ typeSentencesText ++ "\n" ++ allTermText
      -- Collect all names that could cause collisions: local definitions + ambiguous names
      localDefNames = Set.union ambiguousNames $ Set.fromList $
        [n | (n, _) <- typeDefs] ++
        concatMap (\(_, grp) -> map (\(n, _, _, _) -> n) grp) termGroups'
      body0 = stripHydraQualifications ns localDefNames bodyWithImports
      -- Remove redundant default branches from exhaustive matches
      body1 = removeRedundantDefaults constrCounts body0
      -- Add catch-all branches for non-exhaustive matches
      body2 = addPartialMatchCatchAll constrCounts body1
      -- Extract polymorphic helpers to reduce Coq type inference overhead
      body3 = extractPolymorphicHelpers body2
      -- Replace applications of sanitized field accessors with hydra_unreachable.
      -- Use global set (cross-module) since accessor definitions may be in other modules.
      body4 = replaceSanitizedAccessors globalSanitizedAcc body3
      -- For modules that are too complex for coqc, convert Definition to Axiom
      body = if isAxiomOnly then convertDefinitionsToAxioms body4 else body4

      -- Generate Arguments declarations for parameterized types (appended after types)
      typeArgsDecls = generateArgumentsDecls typeDefs

      -- Insert Arguments declarations for non-mutual term definitions inline
      nonMutualTermDefs = concatMap snd (filter (not . fst) termGroups')
      body' = insertTermArguments nonMutualTermDefs body

      content = desc ++ body' ++ typeArgsDecls ++ "\n"

  in Right (Map.singleton path content)

-- | Convert a Hydra namespace to a file path relative to the output directory.
-- e.g., "hydra.core" -> "hydra/core.v"
-- e.g., "hydra.show.core" -> "hydra/show/core.v"
-- File names are NOT capitalized so that Coq's module path resolution
-- matches Hydra's namespace convention (e.g., "hydra.core.Term" resolves
-- to module hydra.core in file hydra/core.v).
namespaceToPath :: String -> FilePath
namespaceToPath ns =
  let parts = splitOn '.' ns
      dirParts = init parts
      fileName = last parts ++ ".v"
  in foldr (</>) fileName dirParts

-- | Convert a Hydra namespace to a Coq logical module path.
-- These match the namespace exactly since file names are not capitalized.
-- e.g., "hydra.core" -> "hydra.core"
namespaceToCoqModule :: String -> String
namespaceToCoqModule = id

-- | Split a string on a character
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | x == c = "" : splitOn c xs
  | otherwise = let (first:rest) = splitOn c xs in (x:first) : rest

-- | Capitalize first letter
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

-- | Decapitalize first letter
decapitalize :: String -> String
decapitalize [] = []
decapitalize (c:cs) = toLower c : cs

-- | Extract type definitions from a module
extractTypeDefs :: Pkg.Module -> [(String, Core.Type)]
extractTypeDefs m = concatMap go (Pkg.moduleDefinitions m)
  where
    go (Pkg.DefinitionType td) =
      let name = localName (Core.unName $ Pkg.typeDefinitionName td)
          typ = Core.typeSchemeType (Pkg.typeDefinitionType td)
      in [(name, typ)]
    go _ = []

-- | Check if a mutual group has strict positivity issues.
-- A positivity issue occurs when a type in the group appears as a function
-- domain in a constructor argument of another type in the group.
hasPositivityIssue :: Set.Set String -> [(String, Core.Type)] -> Bool
hasPositivityIssue groupNames = any checkDef
  where
    checkDef (_, ty) =
      let (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeRecord fields -> any (checkFieldNeg . Core.fieldTypeType) fields
        Core.TypeUnion fields -> any (checkFieldNeg . Core.fieldTypeType) fields
        _ -> False
    -- Check if a type has a group-member in a negative (function domain) position
    checkFieldNeg ty = case ty of
      Core.TypeFunction ft ->
        hasGroupRef (Core.functionTypeDomain ft) || checkFieldNeg (Core.functionTypeCodomain ft)
      Core.TypeAnnotated at -> checkFieldNeg (Core.annotatedTypeBody at)
      Core.TypeForall ft -> checkFieldNeg (Core.forallTypeBody ft)
      Core.TypeWrap wt -> checkFieldNeg wt
      _ -> False
    -- Check if a type mentions any group member
    hasGroupRef ty = case ty of
      Core.TypeVariable v -> localName (Core.unName v) `Set.member` groupNames
      Core.TypeApplication v -> hasGroupRef (Core.applicationTypeFunction v) || hasGroupRef (Core.applicationTypeArgument v)
      Core.TypeFunction v -> hasGroupRef (Core.functionTypeDomain v) || hasGroupRef (Core.functionTypeCodomain v)
      Core.TypeList v -> hasGroupRef v
      Core.TypeMap v -> hasGroupRef (Core.mapTypeKeys v) || hasGroupRef (Core.mapTypeValues v)
      Core.TypeMaybe v -> hasGroupRef v
      Core.TypePair v -> hasGroupRef (Core.pairTypeFirst v) || hasGroupRef (Core.pairTypeSecond v)
      Core.TypeSet v -> hasGroupRef v
      Core.TypeAnnotated v -> hasGroupRef (Core.annotatedTypeBody v)
      Core.TypeForall v -> hasGroupRef (Core.forallTypeBody v)
      Core.TypeWrap v -> hasGroupRef v
      Core.TypeEither v -> hasGroupRef (Core.eitherTypeLeft v) || hasGroupRef (Core.eitherTypeRight v)
      _ -> False

-- | Replace function-typed fields that cause strict positivity violations with unit.
-- Only affects fields where a group member appears in a function domain.
sanitizePositivity :: Set.Set String -> Core.Type -> Core.Type
sanitizePositivity groupNames ty =
  let (params, bodyTy) = extractTypeParams ty
      sanitized = case bodyTy of
        Core.TypeRecord fields -> Core.TypeRecord (map sanitizeField fields)
        Core.TypeUnion fields -> Core.TypeUnion (map sanitizeField fields)
        _ -> bodyTy
      -- Re-wrap with forall parameters
  in foldr (\p t -> Core.TypeForall (Core.ForallType (Core.Name p) t)) sanitized params
  where
    sanitizeField ft =
      if checkFieldNeg (Core.fieldTypeType ft)
      then ft { Core.fieldTypeType = Core.TypeUnit }
      else ft
    checkFieldNeg fty = case fty of
      Core.TypeFunction ft ->
        hasGroupRef (Core.functionTypeDomain ft) || checkFieldNeg (Core.functionTypeCodomain ft)
      Core.TypeAnnotated at -> checkFieldNeg (Core.annotatedTypeBody at)
      Core.TypeForall ft -> checkFieldNeg (Core.forallTypeBody ft)
      Core.TypeWrap wt -> checkFieldNeg wt
      _ -> False
    hasGroupRef hty = case hty of
      Core.TypeVariable v -> localName (Core.unName v) `Set.member` groupNames
      Core.TypeApplication v -> hasGroupRef (Core.applicationTypeFunction v) || hasGroupRef (Core.applicationTypeArgument v)
      Core.TypeFunction v -> hasGroupRef (Core.functionTypeDomain v) || hasGroupRef (Core.functionTypeCodomain v)
      Core.TypeList v -> hasGroupRef v
      Core.TypeMaybe v -> hasGroupRef v
      Core.TypeSet v -> hasGroupRef v
      Core.TypeAnnotated v -> hasGroupRef (Core.annotatedTypeBody v)
      Core.TypeForall v -> hasGroupRef (Core.forallTypeBody v)
      Core.TypeWrap v -> hasGroupRef v
      _ -> False

-- | Generate Coq sentences for a group of type definitions.
-- Non-cyclic singletons → individual Inductive/Definition/Record.
-- Cyclic groups → single Inductive ... with ... block.
generateTypeGroup :: (Bool, [(String, Core.Type)]) -> [Coq.Sentence]
generateTypeGroup (False, [(name, ty)]) = generateTypeSentence name ty
generateTypeGroup (_, group) =
  let groupNames = Set.fromList (map fst group)
      hasPositivity = hasPositivityIssue groupNames group
      -- For groups with positivity issues, sanitize problematic fields to unit
      sanitizedGroup = if hasPositivity
        then map (\(n, ty) -> (n, sanitizePositivity groupNames ty)) group
        else group
  in
       -- Mutually recursive group: generate as Inductive ... with ...
       let bodies = concatMap (\(name, ty) -> makeInductiveBody name ty) sanitizedGroup
           -- Generate field accessor functions for record-like types in the group
           accessors = concatMap makeAccessorDefs sanitizedGroup
       in (if null bodies then []
           else [Coq.Sentence Nothing $ Coq.SentenceContentInductive $ Coq.InductiveDefinition
                  Nothing False bodies])
          ++ accessors
  where
    makeInductiveBody name ty =
      let (params, bodyTy) = extractTypeParams ty
          paramBinders = map (\p -> Coq.BinderType (Coq.TypeBinders
            [Coder.coqName p] (Coq.Type (Coder.coqTermQualid "Type")))) params
      in case bodyTy of
        Core.TypeUnion fields ->
          [Coq.InductiveBody
            (Coder.coqIdent name) paramBinders
            (Just (Coq.Type (Coder.coqTermQualid "Type")))
            (map (makeConstructor name params) fields)]
        Core.TypeRecord fields ->
          if null fields
          then [Coq.InductiveBody
                  (Coder.coqIdent name) paramBinders
                  (Just (Coq.Type (Coder.coqTermQualid "Type")))
                  [Coq.Constructor (Coder.coqIdent ("Build_" ++ name)) [] (Just (Coq.Type (makeReturnType name params)))]]
          else
            let constrType = foldr (\ft acc ->
                  let ftCoq = Coder.encodeType (Core.fieldTypeType ft)
                  in Coder.coqArrow ftCoq acc)
                  (makeReturnType name params)
                  fields
            in [Coq.InductiveBody
                  (Coder.coqIdent name) paramBinders
                  (Just (Coq.Type (Coder.coqTermQualid "Type")))
                  [Coq.Constructor (Coder.coqIdent ("Build_" ++ name)) [] (Just (Coq.Type constrType))]]
        _ -> []  -- non-record/union types in a cycle are unusual; skip

    makeConstructor typeName params ft =
      let fn = Core.unName (Core.fieldTypeName ft)
          constrName = typeName ++ "_" ++ capitalize fn
          fieldTy = Core.fieldTypeType ft
          argType = Coder.encodeType fieldTy
      in Coq.Constructor
          (Coder.coqIdent constrName) []
          (Just (Coq.Type (if isUnitType fieldTy
            then makeReturnType typeName params
            else Coder.coqArrow argType (makeReturnType typeName params))))

    makeReturnType typeName params =
      if null params
      then Coder.coqTermQualid typeName
      else Coder.coqTermApp (Coder.coqTermQualid typeName) (map Coder.coqTermQualid params)

    -- Always include unit -> for unit-typed constructors.
    -- This ensures consistency with the Coder, which always generates
    -- a pattern variable when the lambda body references the parameter.
    isUnitType _ = False

    -- Generate field accessor Definition's for record-like types in mutual groups.
    -- E.g., for TypeLambda with fields (parameter, body):
    --   Definition typeLambda_parameter (r_ : TypeLambda) : Name :=
    --     match r_ with Build_TypeLambda f0 f1 => f0 end.
    makeAccessorDefs (name, ty) =
      let (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeRecord fields | not (null fields) ->
          let nFields = length fields
              fieldVars = ["f" ++ show i | i <- [0 .. nFields - 1]]
              constrPat = Coq.Pattern10Qualiid (Coq.Pattern10_Qualid
                { Coq.pattern10_QualidQualid = Coder.coqQualid ("Build_" ++ name)
                , Coq.pattern10_QualidPatterns = map (\v -> Coq.Pattern1
                    { Coq.pattern1Pattern = Coq.Pattern0Qualid (Coder.coqQualid v)
                    , Coq.pattern1Scope = Nothing }) fieldVars
                })
          in [makeOneAccessor name constrPat fieldVars i ft False
              | (i, ft) <- zip [0..] fields]
        _ -> []

    makeOneAccessor typeName constrPat fieldVars idx ft sanitized =
      let fn = sanitize (localName (Core.unName (Core.fieldTypeName ft)))
          prefixedFn = decapitalize typeName ++ "_" ++ fn
          -- For sanitized fields, return the field variable (will have type unit).
          -- The call site will need hydra_unreachable to coerce it.
          returnExpr = Coder.coqTermQualid (fieldVars !! idx)
          matchExpr = Coq.TermTerm100 (Coq.Term100Term10 (Coq.Term10OneTerm
            (Coq.OneTermTerm1 (Coq.Term1Term0 (Coq.Term0Match (Coq.Match
              { Coq.matchCaseItems = [Coq.CaseItem
                  { Coq.caseItemTerm = Coq.Term100Term10 (Coq.Term10OneTerm
                      (Coq.OneTermExplicit (Coq.QualidAnnotated
                        { Coq.qualidAnnotatedQualid = Coder.coqQualid "r_"
                        , Coq.qualidAnnotatedUnivAnnot = Nothing })))
                  , Coq.caseItemAs = Nothing
                  , Coq.caseItemIn = Nothing }]
              , Coq.matchReturn = Nothing
              , Coq.matchPipe = False
              , Coq.matchEquations = [Coq.Equation
                  { Coq.equationPattern = [[Coq.PatternPattern constrPat]]
                  , Coq.equationTerm = returnExpr }]
              }))))))
      in Coq.Sentence Nothing $ Coq.SentenceContentDefinition $ Coq.Definition
          Nothing (Coder.coqIdent prefixedFn)
          [Coq.BinderType (Coq.TypeBinders [Coder.coqName "r_"]
            (Coq.Type (Coder.coqTermQualid typeName)))]
          Nothing
          matchExpr

-- | Generate a type definition as Inductive (used when Record field names collide).
generateTypeSentenceAsInductive :: String -> Core.Type -> [Coq.Sentence]
generateTypeSentenceAsInductive name ty =
  let (params, bodyTy) = extractTypeParams ty
      paramBinders = map (\p -> Coq.BinderType (Coq.TypeBinders
        [Coder.coqName p] (Coq.Type (Coder.coqTermQualid "Type")))) params
      makeRet = if null params then Coder.coqTermQualid name
                else Coder.coqTermApp (Coder.coqTermQualid name) (map Coder.coqTermQualid params)
  in case bodyTy of
    Core.TypeRecord fields | not (null fields) ->
      let constrType = foldr (\ft acc ->
            Coder.coqArrow (Coder.encodeType (Core.fieldTypeType ft)) acc) makeRet fields
      in [Coq.Sentence Nothing $ Coq.SentenceContentInductive $ Coq.InductiveDefinition
            Nothing False
            [Coq.InductiveBody (Coder.coqIdent name) paramBinders
              (Just (Coq.Type (Coder.coqTermQualid "Type")))
              [Coq.Constructor (Coder.coqIdent ("Build_" ++ name)) [] (Just (Coq.Type constrType))]]]
    _ -> generateTypeSentence name ty  -- fallback for non-record types

-- | Generate a Coq type definition directly, producing proper Inductive types
-- for unions and Definition aliases for other types.
-- Returns a list of sentences (may produce multiple for a union + its synonyms).
generateTypeSentence :: String -> Core.Type -> [Coq.Sentence]
generateTypeSentence name ty =
  let (params, bodyTy) = extractTypeParams ty
      paramBinders = map (\p -> Coq.BinderType (Coq.TypeBinders
        [Coder.coqName p] (Coq.Type (Coder.coqTermQualid "Type")))) params
  in case bodyTy of
    Core.TypeUnion fields ->
      [Coq.Sentence Nothing $ Coq.SentenceContentInductive $ Coq.InductiveDefinition
        Nothing False
        [Coq.InductiveBody
          (Coder.coqIdent name)
          paramBinders
          (Just (Coq.Type (Coder.coqTermQualid "Type")))
          (map (makeConstructor name params) fields)]]
    Core.TypeRecord fields ->
      if null fields
      then [mkDefSentence name paramBinders (Coder.coqTermQualid "unit")]
      else
        -- Generate as proper Record with prefixed field names to avoid collisions.
        -- E.g., type Binding with field "name" -> "binding_name"
        [Coq.Sentence Nothing $ Coq.SentenceContentRecord $ Coq.RecordDefinition
          Nothing
          (Coder.coqIdent name)
          paramBinders
          (Just Coq.SortType)
          (Coq.RecordBody
            (Just (Coder.coqIdent ("Build_" ++ name)))
            (map (\ft ->
              let fn = sanitize (localName (Core.unName (Core.fieldTypeName ft)))
                  prefixedFn = decapitalize name ++ "_" ++ fn
                  ftCoq = Coder.encodeType (Core.fieldTypeType ft)
              in Coq.RecordField (Coder.coqIdent prefixedFn) (Coq.Type ftCoq)
            ) fields))]
    _ ->
      [mkDefSentence name paramBinders (Coder.encodeType bodyTy)]
  where
    mkDefSentence n binders body = Coq.Sentence Nothing $
      Coq.SentenceContentDefinition $ Coq.Definition
        Nothing (Coder.coqIdent n) binders
        (Just (Coq.Type (Coder.coqTermQualid "Type")))
        body

    makeConstructor typeName params ft =
      let fn = Core.unName (Core.fieldTypeName ft)
          constrName = typeName ++ "_" ++ capitalize fn
          fieldTy = Core.fieldTypeType ft
          argType = Coder.encodeType fieldTy
      in Coq.Constructor
          (Coder.coqIdent constrName)
          []
          (Just (Coq.Type (if isUnitType fieldTy
            then makeReturnType typeName params
            else Coder.coqArrow argType (makeReturnType typeName params))))

    makeReturnType typeName params =
      if null params
      then Coder.coqTermQualid typeName
      else Coder.coqTermApp (Coder.coqTermQualid typeName)
             (map Coder.coqTermQualid params)

    -- Always include unit -> for unit-typed constructors.
    -- This ensures consistency with the Coder, which always generates
    -- a pattern variable when the lambda body references the parameter.
    isUnitType _ = False

-- | Generate Arguments declarations for parameterized types.
-- Only makes a parameter implicit ({p}) for a constructor if the parameter
-- appears in at least one of the constructor's argument types (so Coq can infer it).
-- Parameters that don't appear in argument types are left explicit (p).
generateArgumentsDecls :: [(String, Core.Type)] -> String
generateArgumentsDecls typeDefs =
  let allParamTypes = concatMap collectParamType typeDefs
      lines' = concatMap makeArgsLines allParamTypes
  in if null lines' then "" else "\n" ++ unlines lines'
  where
    collectParamType (name, ty) =
      let (params, bodyTy) = extractTypeParams ty
      in if null params then [] else [(name, params, bodyTy)]

    makeArgsLines (name, params, bodyTy) =
      case bodyTy of
        Core.TypeUnion fields ->
          concatMap (makeConstrArgs name params) fields
        Core.TypeRecord fields
          | null fields -> []
          | otherwise ->
            let implicitAll = unwords ["{" ++ p ++ "}" | p <- params]
                constrLine = "Arguments Build_" ++ name ++ " " ++ implicitAll ++ "."
                fieldLines = [("Arguments " ++ decapitalize name ++ "_" ++ sanitize (localName (Core.unName (Core.fieldTypeName ft)))
                              ++ " " ++ implicitAll ++ ".") | ft <- fields]
            in constrLine : fieldLines
        _ -> []

    -- For a union constructor, make all type params implicit.
    -- Coq can usually infer them from context (return type, surrounding expressions).
    -- When it can't, the definition will fail — but this is better than requiring
    -- the Coder to pass type arguments it doesn't have access to.
    makeConstrArgs typeName params ft =
      let constrName = typeName ++ "_" ++ capitalize (Core.unName (Core.fieldTypeName ft))
          implicitAll = unwords ["{" ++ p ++ "}" | p <- params]
      in ["Arguments " ++ constrName ++ " " ++ implicitAll ++ "."]


-- | Insert Arguments declarations for term definitions inline in the rendered text.
-- For each term definition with type variable binders, inserts an Arguments line
-- right after the Definition's closing period.
insertTermArguments :: [(String, Core.Term, [Core.Name], Maybe Core.Type)] -> String -> String
insertTermArguments termDefs text =
  let argsMap = Map.fromList $ concatMap makeArgs termDefs
  in unlines $ processLines argsMap [] (lines text)
  where
    makeArgs (name, body, typeVars, _mType) =
      let schemeVarNames = Set.fromList (map Core.unName typeVars)
          innerTypeVars = if null typeVars then Set.empty else collectFreeTypeVars body
          allTypeVarNames = nub (map Core.unName typeVars ++ filter (`Set.notMember` schemeVarNames) (Set.toList innerTypeVars))
      in if null allTypeVarNames then []
         else let implicitParams = unwords ["{" ++ v ++ "}" | v <- allTypeVarNames]
              in [(name, "Arguments " ++ name ++ " " ++ implicitParams ++ ".")]

    -- Process lines with state: tracking names in the current Definition/Fixpoint block
    processLines _ _ [] = []
    processLines aMap currentNames (l:ls) =
      let newName = extractDefName l
          activeNames = case newName of
            Just n -> currentNames ++ [n]  -- accumulate names in block
            Nothing -> currentNames
          endsWithDot = lineEndsSentence l
      in if endsWithDot && not (null activeNames)
         then let argsLines = concatMap (\n -> maybe [] (:[]) (Map.lookup n aMap)) activeNames
              in l : argsLines ++ processLines aMap [] ls
         else l : processLines aMap activeNames ls

    lineEndsSentence l =
      let trimmed = reverse (dropWhile (== ' ') (reverse l))
      in not (null trimmed) && last trimmed == '.'

    extractDefName line =
      let trimmed = dropWhile (== ' ') line
      in extractAfterKeyword "Definition " trimmed
         `orElse` extractAfterKeyword "Fixpoint " trimmed
         `orElse` extractAfterKeyword "with " trimmed

    extractAfterKeyword kw str =
      if kw `isPrefixOf` str
      then let rest = drop (length kw) str
               name = takeWhile (\c -> isAlphaNum c || c == '_') rest
           in Just name
      else Nothing

    orElse Nothing b = b
    orElse a _ = a

-- | Normalize inner type lambdas: rename their parameters to match the outermost
-- type lambda chain. The Coq coder erases TermTypeLambda nodes, so inner type lambdas
-- that rebind the same polymorphic variables under different names (e.g. t1 instead of t0)
-- leave dangling references. This pass substitutes inner type lambda parameters to match
-- the outer chain, then erases the inner type lambda wrapper.
normalizeInnerTypeLambdas :: Core.Term -> Core.Term
normalizeInnerTypeLambdas term =
  let (outerParams, body) = stripTypeLambdas term
  in if null outerParams then term
     else rebuildTypeLambdas outerParams (goTerm outerParams body)
  where
    -- Strip leading TermTypeLambda chain, returning param names and body
    stripTypeLambdas (Core.TermTypeLambda tl) =
      let (more, b) = stripTypeLambdas (Core.typeLambdaBody tl)
      in (Core.unName (Core.typeLambdaParameter tl) : more, b)
    stripTypeLambdas t = ([], t)

    -- Rebuild TermTypeLambda chain
    rebuildTypeLambdas [] body = body
    rebuildTypeLambdas (p:ps) body =
      Core.TermTypeLambda (Core.TypeLambda (Core.Name p) (rebuildTypeLambdas ps body))

    -- Walk a term, converting inner type lambdas to regular lambdas.
    -- polyNames: set of let-bound names whose terms were converted from type lambdas.
    -- Only type applications targeting these names are converted to term applications.
    goTerm outerPs = go Set.empty
      where
        go polyNames tm = case tm of
          Core.TermAnnotated v -> Core.TermAnnotated v { Core.annotatedTermBody = go polyNames (Core.annotatedTermBody v) }
          Core.TermApplication v -> Core.TermApplication v
            { Core.applicationFunction = go polyNames (Core.applicationFunction v)
            , Core.applicationArgument = go polyNames (Core.applicationArgument v) }
          Core.TermEither v -> Core.TermEither $ case v of
            Left l -> Left (go polyNames l)
            Right r -> Right (go polyNames r)
          Core.TermLambda lam -> Core.TermLambda lam
            { Core.lambdaBody = go polyNames (Core.lambdaBody lam) }
          Core.TermCases cs -> Core.TermCases cs
            { Core.caseStatementCases = map (\f -> f { Core.fieldTerm = go polyNames (Core.fieldTerm f) }) (Core.caseStatementCases cs)
            , Core.caseStatementDefault = fmap (go polyNames) (Core.caseStatementDefault cs) }
          Core.TermLet v ->
            -- Identify bindings whose terms start with TypeLambda — these get converted
            let newPolyNames = Set.fromList [Core.unName (Core.bindingName b)
                  | b <- Core.letBindings v
                  , isTypeLambda (Core.bindingTerm b)]
                polyNames' = Set.union polyNames newPolyNames
            in Core.TermLet v
              { Core.letBindings = map (\b -> b
                  { Core.bindingTerm = go polyNames' (Core.bindingTerm b)
                  -- Clear bindingType for converted bindings so stale type vars aren't hoisted
                  , Core.bindingType = if isTypeLambda (Core.bindingTerm b) then Nothing
                                       else Core.bindingType b }) (Core.letBindings v)
              , Core.letBody = go polyNames' (Core.letBody v) }
          Core.TermList v -> Core.TermList (map (go polyNames) v)
          Core.TermMap v -> Core.TermMap v
          Core.TermMaybe v -> Core.TermMaybe (fmap (go polyNames) v)
          Core.TermPair v -> Core.TermPair (go polyNames (fst v), go polyNames (snd v))
          Core.TermRecord v -> Core.TermRecord v { Core.recordFields = map (\f -> f { Core.fieldTerm = go polyNames (Core.fieldTerm f) }) (Core.recordFields v) }
          Core.TermSet s -> Core.TermSet (Set.map (go polyNames) s)
          Core.TermInject v -> Core.TermInject v { Core.injectionField = (Core.injectionField v) { Core.fieldTerm = go polyNames (Core.fieldTerm (Core.injectionField v)) } }
          Core.TermTypeLambda tl ->
            -- Inner type lambda: convert to regular lambda with Type domain
            let param = Core.typeLambdaParameter tl
                body = Core.typeLambdaBody tl
            in go polyNames $ Core.TermLambda $ Core.Lambda
                 param (Just (Core.TypeVariable (Core.Name "Type"))) body
          Core.TermTypeApplication v ->
            let body = Core.typeApplicationTermBody v
                ttype = Core.typeApplicationTermType v
            in if targetsPolyName polyNames body
               then go polyNames $ Core.TermApplication $ Core.Application body (typeToTerm ttype)
               else -- Erase type application for non-local targets (library/imported functions)
                    go polyNames body
          Core.TermWrap v -> Core.TermWrap v { Core.wrappedTermBody = go polyNames (Core.wrappedTermBody v) }
          _ -> tm

    isTypeLambda (Core.TermTypeLambda _) = True
    isTypeLambda (Core.TermAnnotated v) = isTypeLambda (Core.annotatedTermBody v)
    isTypeLambda _ = False

    -- Check if a type application's body ultimately targets a poly-converted local name
    targetsPolyName polyNames tm = case tm of
      Core.TermVariable v -> Set.member (Core.unName v) polyNames
      Core.TermTypeApplication v -> targetsPolyName polyNames (Core.typeApplicationTermBody v)
      Core.TermAnnotated v -> targetsPolyName polyNames (Core.annotatedTermBody v)
      _ -> False

    -- Convert a type to a term for use as an explicit type argument
    typeToTerm ty = case ty of
      Core.TypeVariable v -> Core.TermVariable v
      Core.TypeList t -> Core.TermApplication $ Core.Application
        (Core.TermVariable (Core.Name "list")) (typeToTerm t)
      Core.TypeMaybe t -> Core.TermApplication $ Core.Application
        (Core.TermVariable (Core.Name "option")) (typeToTerm t)
      Core.TypeSet t -> Core.TermApplication $ Core.Application
        (Core.TermVariable (Core.Name "list")) (typeToTerm t)
      Core.TypeApplication at -> Core.TermApplication $ Core.Application
        (typeToTerm (Core.applicationTypeFunction at))
        (typeToTerm (Core.applicationTypeArgument at))
      Core.TypeFunction ft -> Core.TermVariable (Core.Name "unit")  -- Fallback for function types used as type args
      Core.TypePair pt -> Core.TermApplication $ Core.Application
        (Core.TermApplication $ Core.Application
          (Core.TermVariable (Core.Name "prod"))
          (typeToTerm (Core.pairTypeFirst pt)))
        (typeToTerm (Core.pairTypeSecond pt))
      Core.TypeMap mt -> Core.TermApplication $ Core.Application
        (Core.TermVariable (Core.Name "list"))
        (Core.TermApplication $ Core.Application
          (Core.TermVariable (Core.Name "prod"))
          (typeToTerm (Core.mapTypeKeys mt)))
      Core.TypeUnit -> Core.TermVariable (Core.Name "unit")
      Core.TypeLiteral _ -> Core.TermVariable (Core.Name "unit")  -- Fallback
      Core.TypeEither et -> Core.TermApplication $ Core.Application
        (Core.TermApplication $ Core.Application
          (Core.TermVariable (Core.Name "sum"))
          (typeToTerm (Core.eitherTypeLeft et)))
        (typeToTerm (Core.eitherTypeRight et))
      Core.TypeRecord _ -> Core.TermVariable (Core.Name "unit")
      Core.TypeUnion _ -> Core.TermVariable (Core.Name "unit")
      Core.TypeWrap t -> typeToTerm t
      Core.TypeAnnotated at -> typeToTerm (Core.annotatedTypeBody at)
      Core.TypeForall ft -> typeToTerm (Core.forallTypeBody ft)  -- Strip forall for type args
      Core.TypeVoid -> Core.TermVariable (Core.Name "Empty_set")


-- | Reorder let bindings so that each binding appears after all bindings it references.
-- In Haskell, let is mutually recursive. In Coq, let ... in is sequential.
-- This topological sort ensures dependencies are defined before use.
reorderLetBindings :: Core.Term -> Core.Term
reorderLetBindings = go
  where
    go tm = case tm of
      Core.TermAnnotated v -> Core.TermAnnotated v { Core.annotatedTermBody = go (Core.annotatedTermBody v) }
      Core.TermApplication v -> Core.TermApplication v
        { Core.applicationFunction = go (Core.applicationFunction v)
        , Core.applicationArgument = go (Core.applicationArgument v) }
      Core.TermLambda lam -> Core.TermLambda lam { Core.lambdaBody = go (Core.lambdaBody lam) }
      Core.TermCases cs -> Core.TermCases cs
        { Core.caseStatementCases = map (\f -> f { Core.fieldTerm = go (Core.fieldTerm f) }) (Core.caseStatementCases cs)
        , Core.caseStatementDefault = fmap go (Core.caseStatementDefault cs) }
      Core.TermLet v ->
        -- Collect consecutive single-binding lets into one group for sorting.
        -- Haskell's mutual let becomes nested single-binding lets in Hydra.
        let (allBindings, innerBody) = collectLetBindings tm
            -- Process SCCs: acyclic bindings stay as-is, cyclic ones get pair-encoded.
            processedGroups = processSCCs allBindings
            -- Apply 'go' recursively to each binding's term
            processedGroups' = map (\grp -> map (\b -> b { Core.bindingTerm = go (Core.bindingTerm b) }) grp) processedGroups
            -- Rebuild lets, handling cyclic groups with pair encoding
        in rebuildMutualLets processedGroups' (go innerBody)
      Core.TermList ts -> Core.TermList (map go ts)
      Core.TermEither v -> Core.TermEither $ case v of
        Left l -> Left (go l); Right r -> Right (go r)
      Core.TermMaybe m -> Core.TermMaybe (fmap go m)
      Core.TermPair p -> Core.TermPair (go (fst p), go (snd p))
      Core.TermRecord r -> Core.TermRecord r { Core.recordFields = map (\f -> f { Core.fieldTerm = go (Core.fieldTerm f) }) (Core.recordFields r) }
      Core.TermInject inj -> Core.TermInject inj { Core.injectionField = (Core.injectionField inj) { Core.fieldTerm = go (Core.fieldTerm (Core.injectionField inj)) } }
      Core.TermTypeLambda tl -> Core.TermTypeLambda tl { Core.typeLambdaBody = go (Core.typeLambdaBody tl) }
      Core.TermWrap w -> Core.TermWrap w { Core.wrappedTermBody = go (Core.wrappedTermBody w) }
      _ -> tm

    -- Rename all free occurrences of a variable name within a term
    renameVar oldName newName tm = case tm of
      Core.TermVariable v | v == oldName -> Core.TermVariable newName
      Core.TermVariable v -> Core.TermVariable v
      Core.TermAnnotated a -> Core.TermAnnotated a { Core.annotatedTermBody = renameVar oldName newName (Core.annotatedTermBody a) }
      Core.TermApplication a -> Core.TermApplication a
        { Core.applicationFunction = renameVar oldName newName (Core.applicationFunction a)
        , Core.applicationArgument = renameVar oldName newName (Core.applicationArgument a) }
      Core.TermLambda lam
        | Core.lambdaParameter lam == oldName -> tm  -- shadowed
        | otherwise -> Core.TermLambda lam
            { Core.lambdaBody = renameVar oldName newName (Core.lambdaBody lam) }
      Core.TermCases cs -> Core.TermCases cs
        { Core.caseStatementCases = map (\f -> f { Core.fieldTerm = renameVar oldName newName (Core.fieldTerm f) }) (Core.caseStatementCases cs)
        , Core.caseStatementDefault = fmap (renameVar oldName newName) (Core.caseStatementDefault cs) }
      Core.TermLet lt ->
        let isBound = any (\b -> Core.bindingName b == oldName) (Core.letBindings lt)
        in if isBound then tm
           else Core.TermLet lt
             { Core.letBindings = map (\b -> b { Core.bindingTerm = renameVar oldName newName (Core.bindingTerm b) }) (Core.letBindings lt)
             , Core.letBody = renameVar oldName newName (Core.letBody lt) }
      Core.TermList ts -> Core.TermList (map (renameVar oldName newName) ts)
      Core.TermMaybe m -> Core.TermMaybe (fmap (renameVar oldName newName) m)
      Core.TermPair p -> Core.TermPair (renameVar oldName newName (fst p), renameVar oldName newName (snd p))
      Core.TermRecord r -> Core.TermRecord r { Core.recordFields = map (\f -> f { Core.fieldTerm = renameVar oldName newName (Core.fieldTerm f) }) (Core.recordFields r) }
      Core.TermInject inj -> Core.TermInject inj { Core.injectionField = (Core.injectionField inj) { Core.fieldTerm = renameVar oldName newName (Core.fieldTerm (Core.injectionField inj)) } }
      Core.TermTypeLambda tl -> Core.TermTypeLambda tl { Core.typeLambdaBody = renameVar oldName newName (Core.typeLambdaBody tl) }
      Core.TermTypeApplication ta -> Core.TermTypeApplication ta { Core.typeApplicationTermBody = renameVar oldName newName (Core.typeApplicationTermBody ta) }
      Core.TermWrap w -> Core.TermWrap w { Core.wrappedTermBody = renameVar oldName newName (Core.wrappedTermBody w) }
      Core.TermEither e -> Core.TermEither (case e of Left l -> Left (renameVar oldName newName l); Right r -> Right (renameVar oldName newName r))
      _ -> tm

    -- Collect consecutive single-binding lets into a flat list
    collectLetBindings (Core.TermLet v) =
      let bindings = Core.letBindings v
          body = Core.letBody v
          (moreBindings, innerBody) = collectLetBindings body
      in (bindings ++ moreBindings, innerBody)
    collectLetBindings tm = ([], tm)

    -- Rebuild nested single-binding lets from a flat list
    rebuildLets [] body = body
    rebuildLets (b:bs) body =
      Core.TermLet (Core.Let [b] (rebuildLets bs body))

    -- Group bindings by SCC, returning a topologically-ordered list of groups.
    -- Each group is either a single acyclic binding or a cycle of mutually recursive ones.
    processSCCs bindings =
      let localPart s = case splitOn '.' s of { [] -> s; parts -> last parts }
          names = Set.fromList [Core.unName (Core.bindingName b) | b <- bindings]
          localNames = Set.fromList [localPart (Core.unName (Core.bindingName b)) | b <- bindings]
          allNames = Set.union names localNames
          depVars b = let vars = freeVars (Core.bindingTerm b)
                          localVars = Set.fromList [localPart v | v <- Set.toList vars]
                      in Set.intersection allNames (Set.union vars localVars)
          graph = [(b, Core.unName (Core.bindingName b), Set.toList (depVars b)) | b <- bindings]
          sccs = stronglyConnComp graph
      in map sccToList sccs

    sccToList (AcyclicSCC b) = [b]
    sccToList (CyclicSCC bs) = bs

    -- Rebuild lets from a list of SCC groups. Acyclic groups become single lets.
    -- Cyclic groups get encoded using hydra_fix on a pair of functions.
    rebuildMutualLets [] body = body
    rebuildMutualLets (grp:rest) body =
      if length grp == 1
      then Core.TermLet (Core.Let grp (rebuildMutualLets rest body))
      else encodeMutualLetGroup grp (rebuildMutualLets rest body)

    -- Encode a mutually recursive group of let bindings using hydra_fix + pairs.
    -- Given [b1, b2], produces:
    --   let _bundle := hydra_fix (fun _bundle =>
    --     let b1_name := fst _bundle in
    --     let b2_name := snd _bundle in
    --     pair b1_term b2_term) in
    --   let b1_name := fst _bundle in
    --   let b2_name := snd _bundle in
    --   body
    encodeMutualLetGroup grp body =
      let n = length grp
          bundleName = Core.Name "hydra_mutual_bundle_"
          -- Use a different name for the inner lambda parameter so the Coder
          -- doesn't detect self-reference and wrap with another hydra_fix
          bundleInner = Core.Name "hydra_mutual_b_"
          innerBundleVar = Core.TermVariable bundleInner
          outerBundleVar = Core.TermVariable bundleName
          appVar fname v = Core.TermApplication (Core.Application (Core.TermVariable (Core.Name fname)) v)
          -- Projection expressions for n-element nested pair encoding
          nestedSecond 0 v = v
          nestedSecond k v = appVar "pairs.second" (nestedSecond (k-1) v)
          mkProj bvar i
            | i == n - 1 = nestedSecond i bvar
            | otherwise = appVar "pairs.first" (nestedSecond i bvar)
          -- Projections inside the hydra_fix body (use bundleInner)
          innerProjBindings = [Core.Binding (Core.bindingName b) (mkProj innerBundleVar i) (Core.bindingType b)
                              | (i, b) <- zip [0..] grp]
          -- Projections outside the hydra_fix (use bundleName after the let)
          outerProjBindings = [Core.Binding (Core.bindingName b) (mkProj outerBundleVar i) (Core.bindingType b)
                              | (i, b) <- zip [0..] grp]
          -- Strip outer hydra_fix wrapper if present — the bundle provides the fixpoint.
          -- Pattern: hydra_fix (fun NAME => body) → body with NAME replaced by corresponding projection
          stripHydraFix bName bTerm = case bTerm of
            Core.TermApplication (Core.Application
              (Core.TermVariable (Core.Name "hydra_fix"))
              (Core.TermLambda lam)) ->
                -- Replace lambda parameter (self-reference) with the bundle projection
                let innerName = Core.lambdaParameter lam
                    innerBody = Core.lambdaBody lam
                    -- Rename occurrences of innerName to bName in the body
                in renameVar innerName bName innerBody
            _ -> bTerm
          strippedBindings = [b { Core.bindingTerm = stripHydraFix (Core.bindingName b) (Core.bindingTerm b) }
                             | b <- grp]
          -- Inside hydra_fix body: let-bindings projecting from bundleVar, then pair of terms
          pairExpr = mkPair [Core.bindingTerm b | b <- strippedBindings]
          mkPair [] = Core.TermVariable (Core.Name "tt")
          mkPair [t] = t
          mkPair [t1, t2] = Core.TermPair (t1, t2)
          mkPair (t:ts) = Core.TermPair (t, mkPair ts)
          fixBody = rebuildLets innerProjBindings pairExpr
          -- hydra_fix call: hydra_fix (fun bundleInner => fixBody)
          -- Note: bundleInner is different from bundleName so the Coder doesn't
          -- wrap the binding with another hydra_fix (it'd detect self-reference otherwise)
          fixTerm = Core.TermApplication (Core.Application
            (Core.TermVariable (Core.Name "hydra_fix"))
            (Core.TermLambda (Core.Lambda bundleInner Nothing fixBody)))
          bundleBinding = Core.Binding bundleName fixTerm Nothing
      in Core.TermLet (Core.Let [bundleBinding] (rebuildLets outerProjBindings body))

    -- Topological sort of let bindings: move a binding before all bindings that reference it
    topoSortBindings bindings =
      let localPart s = case splitOn '.' s of { [] -> s; parts -> last parts }
          names = Set.fromList [Core.unName (Core.bindingName b) | b <- bindings]
          -- Also include local parts of qualified names (e.g., "isExtra" from "hydra.substitution.isExtra")
          localNames = Set.fromList [localPart (Core.unName (Core.bindingName b)) | b <- bindings]
          allNames = Set.union names localNames
          -- Collect free vars and also extract local parts of qualified names
          depVars b = let vars = freeVars (Core.bindingTerm b)
                          localVars = Set.fromList [localPart v | v <- Set.toList vars]
                      in Set.intersection allNames (Set.union vars localVars)
          deps = depVars
          graph = [(b, Core.unName (Core.bindingName b), Set.toList (deps b)) | b <- bindings]
          sccs = stronglyConnComp graph
      in concatMap flattenSCC sccs

    flattenSCC (AcyclicSCC b) = [b]
    flattenSCC (CyclicSCC bs) = bs  -- fall through; handled separately below

    freeVars tm = case tm of
      Core.TermVariable v -> Set.singleton (Core.unName v)
      Core.TermApplication v -> Set.union (freeVars (Core.applicationFunction v)) (freeVars (Core.applicationArgument v))
      Core.TermLambda lam ->
        Set.delete (Core.unName (Core.lambdaParameter lam)) (freeVars (Core.lambdaBody lam))
      Core.TermCases cs ->
        Set.unions $ maybe Set.empty freeVars (Core.caseStatementDefault cs) :
                     map (\f -> freeVars (Core.fieldTerm f)) (Core.caseStatementCases cs)
      Core.TermLet v ->
        let boundNames = Set.fromList [Core.unName (Core.bindingName b) | b <- Core.letBindings v]
        in Set.difference
             (Set.unions (freeVars (Core.letBody v) : map (\b -> freeVars (Core.bindingTerm b)) (Core.letBindings v)))
             boundNames
      Core.TermAnnotated v -> freeVars (Core.annotatedTermBody v)
      Core.TermList ts -> Set.unions (map freeVars ts)
      Core.TermMaybe m -> maybe Set.empty freeVars m
      Core.TermPair p -> Set.union (freeVars (fst p)) (freeVars (snd p))
      Core.TermRecord r -> Set.unions (map (\f -> freeVars (Core.fieldTerm f)) (Core.recordFields r))
      Core.TermInject inj -> freeVars (Core.fieldTerm (Core.injectionField inj))
      Core.TermTypeLambda tl -> freeVars (Core.typeLambdaBody tl)
      Core.TermTypeApplication ta -> freeVars (Core.typeApplicationTermBody ta)
      Core.TermWrap w -> freeVars (Core.wrappedTermBody w)
      Core.TermEither v -> case v of
        Left l -> freeVars l
        Right r -> freeVars r
      Core.TermSet s -> Set.unions (map freeVars (Set.toList s))
      _ -> Set.empty

-- | Erase lambda domain types that reference type variables not in the given set.
-- These are artifacts of Hydra's generic type inference — inner lambdas may get
-- type variable domains (e.g., TypeVariable "t0") that should be concrete types.
-- Erasing them lets Coq infer the correct types.
eraseUnboundTypeVarDomains :: Set.Set String -> Core.Term -> Core.Term
eraseUnboundTypeVarDomains initialBoundVars = go initialBoundVars
  where
    go boundVars tm = case tm of
      Core.TermAnnotated v -> Core.TermAnnotated v { Core.annotatedTermBody = go boundVars (Core.annotatedTermBody v) }
      Core.TermApplication v -> Core.TermApplication v
        { Core.applicationFunction = go boundVars (Core.applicationFunction v)
        , Core.applicationArgument = go boundVars (Core.applicationArgument v) }
      Core.TermEither v -> Core.TermEither $ case v of
        Left l -> Left (go boundVars l)
        Right r -> Right (go boundVars r)
      Core.TermLambda lam ->
        -- If this lambda introduces a type variable (param with domain Type),
        -- add it to the bound set so inner references aren't erased
        let paramName = Core.unName (Core.lambdaParameter lam)
            isTypeParam = case Core.lambdaDomain lam of
              Just (Core.TypeVariable v) -> Core.unName v == "Type"
              _ -> False
            boundVars' = if isTypeParam && isTypeVarLike paramName
                         then Set.insert paramName boundVars
                         else boundVars
        in Core.TermLambda lam
        { Core.lambdaDomain = eraseIfUnbound boundVars (Core.lambdaDomain lam)
        , Core.lambdaBody = go boundVars' (Core.lambdaBody lam) }
      Core.TermCases cs -> Core.TermCases cs
        { Core.caseStatementCases = map (\f -> f { Core.fieldTerm = go boundVars (Core.fieldTerm f) }) (Core.caseStatementCases cs)
        , Core.caseStatementDefault = fmap (go boundVars) (Core.caseStatementDefault cs) }
      Core.TermLet v -> Core.TermLet v
        { Core.letBindings = map (\b -> b { Core.bindingTerm = go boundVars (Core.bindingTerm b) }) (Core.letBindings v)
        , Core.letBody = go boundVars (Core.letBody v) }
      Core.TermList ts -> Core.TermList (map (go boundVars) ts)
      Core.TermMaybe m -> Core.TermMaybe (fmap (go boundVars) m)
      Core.TermPair p -> Core.TermPair (go boundVars (fst p), go boundVars (snd p))
      Core.TermRecord r -> Core.TermRecord r { Core.recordFields = map (\f -> f { Core.fieldTerm = go boundVars (Core.fieldTerm f) }) (Core.recordFields r) }
      Core.TermInject inj -> Core.TermInject inj { Core.injectionField = (Core.injectionField inj) { Core.fieldTerm = go boundVars (Core.fieldTerm (Core.injectionField inj)) } }
      Core.TermTypeLambda tl ->
        -- Type lambda: the Coder strips it, so the parameter becomes unbound in Coq.
        -- Don't add it to boundVars.
        Core.TermTypeLambda tl { Core.typeLambdaBody = go boundVars (Core.typeLambdaBody tl) }
      Core.TermTypeApplication ta ->
        Core.TermTypeApplication ta { Core.typeApplicationTermBody = go boundVars (Core.typeApplicationTermBody ta) }
      Core.TermWrap w -> Core.TermWrap w { Core.wrappedTermBody = go boundVars (Core.wrappedTermBody w) }
      _ -> tm

    eraseIfUnbound bv Nothing = Nothing
    eraseIfUnbound bv (Just ty) =
      if hasUnboundTypeVar bv ty then Nothing else Just ty

    hasUnboundTypeVar bv (Core.TypeVariable v) =
      let name = Core.unName v
      in isTypeVarLike name && not (Set.member name bv)
    hasUnboundTypeVar bv (Core.TypeFunction ft) =
      hasUnboundTypeVar bv (Core.functionTypeDomain ft) || hasUnboundTypeVar bv (Core.functionTypeCodomain ft)
    hasUnboundTypeVar bv (Core.TypeList t) = hasUnboundTypeVar bv t
    hasUnboundTypeVar bv (Core.TypeMaybe t) = hasUnboundTypeVar bv t
    hasUnboundTypeVar bv (Core.TypeSet t) = hasUnboundTypeVar bv t
    hasUnboundTypeVar bv (Core.TypePair pt) = hasUnboundTypeVar bv (Core.pairTypeFirst pt) || hasUnboundTypeVar bv (Core.pairTypeSecond pt)
    hasUnboundTypeVar bv (Core.TypeMap mt) = hasUnboundTypeVar bv (Core.mapTypeKeys mt) || hasUnboundTypeVar bv (Core.mapTypeValues mt)
    hasUnboundTypeVar bv (Core.TypeApplication at) = hasUnboundTypeVar bv (Core.applicationTypeFunction at) || hasUnboundTypeVar bv (Core.applicationTypeArgument at)
    hasUnboundTypeVar bv (Core.TypeAnnotated at) = hasUnboundTypeVar bv (Core.annotatedTypeBody at)
    hasUnboundTypeVar _ _ = False

    isTypeVarLike ('t':rest) = all isDigit rest && not (null rest)
    isTypeVarLike _ = False

-- | Collect all free type variables from a term.
-- These are type variable references in lambda domain annotations, type applications, etc.
-- that look like generated variable names (t0, t1, etc.)
collectFreeTypeVars :: Core.Term -> Set.Set String
collectFreeTypeVars = collectFromTerm
  where
    collectFromTerm tm = case tm of
      Core.TermAnnotated v -> collectFromTerm (Core.annotatedTermBody v)
      Core.TermApplication v ->
        Set.union (collectFromTerm (Core.applicationFunction v))
                  (collectFromTerm (Core.applicationArgument v))
      Core.TermLambda lam ->
        let paramName = Core.unName (Core.lambdaParameter lam)
            domVars = maybe Set.empty collectFromType (Core.lambdaDomain lam)
            bodyVars = collectFromTerm (Core.lambdaBody lam)
            allVars = Set.union domVars bodyVars
        in if isTypeVarName paramName then Set.delete paramName allVars else allVars
      Core.TermCases cs ->
        let defVars = maybe Set.empty collectFromTerm (Core.caseStatementDefault cs)
            caseVars = Set.unions (map (\f -> collectFromTerm (Core.fieldTerm f)) (Core.caseStatementCases cs))
        in Set.union defVars caseVars
      Core.TermLet lt ->
        let bindVars = Set.unions (map (\b ->
              Set.union (collectFromTerm (Core.bindingTerm b))
                        (maybe Set.empty collectFromTypeScheme (Core.bindingType b)))
              (Core.letBindings lt))
        in Set.union bindVars (collectFromTerm (Core.letBody lt))
      Core.TermList ts -> Set.unions (map collectFromTerm ts)
      Core.TermPair p -> Set.union (collectFromTerm (fst p)) (collectFromTerm (snd p))
      Core.TermRecord r -> Set.unions (map (\f -> collectFromTerm (Core.fieldTerm f)) (Core.recordFields r))
      Core.TermInject inj -> collectFromTerm (Core.fieldTerm (Core.injectionField inj))
      Core.TermTypeLambda tl ->
        Set.delete (Core.unName (Core.typeLambdaParameter tl))
                   (collectFromTerm (Core.typeLambdaBody tl))
      Core.TermTypeApplication ta -> collectFromTerm (Core.typeApplicationTermBody ta)
      Core.TermWrap w -> collectFromTerm (Core.wrappedTermBody w)
      Core.TermMaybe m -> maybe Set.empty collectFromTerm m
      Core.TermEither e -> case e of
        Left t -> collectFromTerm t
        Right t -> collectFromTerm t
      Core.TermSet s -> Set.unions (map collectFromTerm (Set.toList s))
      _ -> Set.empty

    collectFromType ty = case ty of
      Core.TypeVariable v ->
        let name = Core.unName v
        in if isTypeVarName name then Set.singleton name else Set.empty
      Core.TypeFunction ft ->
        Set.union (collectFromType (Core.functionTypeDomain ft))
                  (collectFromType (Core.functionTypeCodomain ft))
      Core.TypeList t -> collectFromType t
      Core.TypeMaybe t -> collectFromType t
      Core.TypeSet t -> collectFromType t
      Core.TypePair pt ->
        Set.union (collectFromType (Core.pairTypeFirst pt))
                  (collectFromType (Core.pairTypeSecond pt))
      Core.TypeMap mt ->
        Set.union (collectFromType (Core.mapTypeKeys mt))
                  (collectFromType (Core.mapTypeValues mt))
      Core.TypeApplication at ->
        Set.union (collectFromType (Core.applicationTypeFunction at))
                  (collectFromType (Core.applicationTypeArgument at))
      Core.TypeAnnotated at -> collectFromType (Core.annotatedTypeBody at)
      Core.TypeForall ft -> collectFromType (Core.forallTypeBody ft)
      Core.TypeRecord fields -> Set.unions (map (\f -> collectFromType (Core.fieldTypeType f)) fields)
      Core.TypeUnion fields -> Set.unions (map (\f -> collectFromType (Core.fieldTypeType f)) fields)
      Core.TypeWrap t -> collectFromType t
      _ -> Set.empty

    collectFromTypeScheme ts =
      Set.union (Set.fromList [Core.unName v | v <- Core.typeSchemeVariables ts, isTypeVarName (Core.unName v)])
                (collectFromType (Core.typeSchemeType ts))

    -- Check if a name looks like a generated type variable (t0, t1, ...)
    isTypeVarName name = case name of
      ('t':rest) -> all isDigit rest && not (null rest)
      _ -> False

-- | Build a map from constructor name prefix (type name) to constructor count
-- for all union types in the module.
buildConstructorCounts :: [(String, Core.Type)] -> Map.Map String Int
buildConstructorCounts = Map.fromList . concatMap extractCounts
  where
    extractCounts (name, ty) =
      let (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeUnion fields -> [(name, length fields)]
        _ -> []

-- | Remove redundant default branches (| _ => ...) from exhaustive matches.
-- Uses a stack of pattern accumulators to track nesting depth.
-- Only removes a default when patterns at the current depth exhaust the type.
removeRedundantDefaults :: Map.Map String Int -> String -> String
removeRedundantDefaults constrCounts = unlines . processStack [[]] . lines
  where
    -- Stack: list of pattern accumulators, one per match nesting level.
    -- stack !! 0 is the current (innermost) level.
    processStack _ [] = []
    processStack stack (l:ls) =
      let trimmed = dropWhile (== ' ') l
          matchCount = countOccurrences "match " l
          endCount = countOccurrences "end)" l + (if trimmed == "end." then 1 else 0)
      in case () of
        _ | isDefaultPattern trimmed ->
              let currentPatterns = head stack
                  mPrefix = case currentPatterns of
                    (p:_) -> extractTypePrefix p
                    _ -> Nothing
                  -- Count only patterns with the same type prefix (prevents cross-match leakage)
                  samePrefix = case mPrefix of
                    Just pfx -> length [p | p <- currentPatterns, extractTypePrefix p == Just pfx]
                    Nothing -> 0
                  expectedCount = mPrefix >>= (`Map.lookup` constrCounts)
                  redundant = case expectedCount of
                    Just n -> samePrefix >= n
                    Nothing -> False
              in if redundant
                 then processStack (adjustStack matchCount endCount stack) ls
                 else l : processStack (adjustStack matchCount endCount stack) ls
          | isConstructorPattern trimmed ->
              let stack' = case stack of
                    (cur:rest) -> (trimmed:cur) : rest
                    [] -> [[trimmed]]
              in l : processStack (adjustStack matchCount endCount stack') ls
          | otherwise ->
              l : processStack (adjustStack matchCount endCount stack) ls

    adjustStack opens closes stack =
      let popped = drop closes stack
          safePopped = if null popped then [[]] else popped
          pushed = iterate ([] :) safePopped !! opens
      in pushed

    countOccurrences needle haystack = countKw Nothing haystack
      where
        countKw _ [] = 0
        countKw prev s@(_:cs)
          | needle `isPrefixOf` s && not (isWordChar prev) =
              1 + countKw Nothing (drop (length needle) s)
          | otherwise = countKw (Just (head s)) cs
        isWordChar Nothing = False
        isWordChar (Just c) = isAlphaNum c || c == '_'

    isDefaultPattern s = "| _ =>" `isPrefixOf` s
    isConstructorPattern s = "|" `isPrefixOf` s && not ("| _" `isPrefixOf` s)

    extractTypePrefix line =
      let t = dropWhile (\c -> c == '|' || c == ' ') line
          constrName = takeWhile (\c -> isAlphaNum c || c == '_') t
      in if null constrName then Nothing
         else Just $ typeNameFromConstructor constrName

    typeNameFromConstructor name =
      case break (== '_') (reverse name) of
        (_, _:rest) -> reverse rest
        _ -> name

-- | Add catch-all branches for non-exhaustive pattern matches.
-- Uses a stack of pattern accumulators (like removeRedundantDefaults) to handle nesting.
-- When closing a match, checks if the accumulated patterns are fewer than the type's
-- constructor count. If so, inserts a catch-all before the end.
addPartialMatchCatchAll :: Map.Map String Int -> String -> String
addPartialMatchCatchAll constrCounts = unlines . processStack [[]] . lines
  where
    processStack _ [] = []
    processStack stack (l:ls) =
      let trimmed = dropWhile (== ' ') l
          matchCount = countOccurrences "match " l
          endCount = countOccurrences "end)" l + (if trimmed == "end." then 1 else 0)
      in if endCount > 0
         then -- For each end, check if we need a catch-all at the current level
              let (output, stack') = handleEnds endCount matchCount stack
              in output ++ (l : processStack stack' ls)
         else if isConstructorPattern trimmed
              then let stack' = case stack of
                         (cur:rest) -> (trimmed:cur) : rest
                         [] -> [[trimmed]]
                       stack'' = pushN matchCount stack'
                   in l : processStack stack'' ls
              else l : processStack (pushN matchCount (popN endCount stack)) ls

    handleEnds 0 opens stack = ([], pushN opens stack)
    handleEnds n opens stack =
      let currentPatterns = head stack
          catchAll = needsCatchAll currentPatterns
          popped = if length stack > 1 then tail stack else [[]]
          (restOutput, restStack) = handleEnds (n - 1) opens popped
      in (catchAll ++ restOutput, restStack)

    pushN 0 stack = stack
    pushN n stack = pushN (n - 1) ([] : stack)

    popN 0 stack = stack
    popN n stack = popN (n - 1) (if length stack > 1 then tail stack else [[]])

    needsCatchAll patterns =
      let constrPatterns = filter (not . isDefaultPattern) patterns
          mPrefix = case constrPatterns of
            (p:_) -> extractConstrPrefix p
            _ -> Nothing
          isRecordMatch = case mPrefix of
            Just prefix -> "Build_" `isPrefixOf` prefix || prefix == "Build"
            Nothing -> False
          expectedCount = mPrefix >>= (`Map.lookup` constrCounts)
          patCount = length constrPatterns
          hasDefault = any isDefaultPattern patterns
      in case expectedCount of
           Just n | not isRecordMatch && not hasDefault && patCount > 0 && patCount < n ->
             ["| _ => hydra_unreachable"]
           _ -> []

    isDefaultPattern s = "| _ =>" `isPrefixOf` s
    isConstructorPattern s = "|" `isPrefixOf` s

    extractConstrPrefix pat =
      let t = dropWhile (\c -> c == '|' || c == ' ') pat
          constrName = takeWhile (\c -> isAlphaNum c || c == '_') t
      in case constrName of
        [] -> Nothing
        "_" -> Nothing  -- wildcard pattern, not a constructor
        _ -> Just (typeNameFromConstructor constrName)

    typeNameFromConstructor name =
      case break (== '_') (reverse name) of
        (_, _:rest) -> reverse rest
        _ -> name

    countOccurrences needle haystack = countKw Nothing haystack
      where
        countKw _ [] = 0
        countKw prev s@(_:cs)
          | needle `isPrefixOf` s && not (isWordChar prev) =
              1 + countKw Nothing (drop (length needle) s)
          | otherwise = countKw (Just (head s)) cs
        isWordChar Nothing = False
        isWordChar (Just c) = isAlphaNum c || c == '_'

-- | Convert all Definition/Fixpoint blocks to Axiom declarations.
-- Parses line-by-line, tracking whether we're inside a definition body.
-- A definition body starts with 'Definition X ... :=' and ends at a line
-- whose top-level (non-paren-wrapped) trailing char is '.', followed by
-- another top-level declaration line.
convertDefinitionsToAxioms :: String -> String
convertDefinitionsToAxioms text = unlines (processLines (lines text))
  where
    processLines [] = []
    processLines (l:rest)
      | "Definition " `isPrefixOf` l || "Fixpoint " `isPrefixOf` l =
          -- Collect full definition lines until we see a new top-level declaration
          let (defnLines, afterDef) = collectDefinition (l:rest)
              fullDefn = unwords (map (dropWhile (== ' ')) defnLines)
              sig = takeBeforeAssign fullDefn
              axiomLine = convertSigToAxiom sig
          in axiomLine : processLines afterDef
      | "Arguments " `isPrefixOf` l = l : processLines rest
      | otherwise = l : processLines rest

    -- Collect lines that belong to the current Definition/Fixpoint block
    -- Stop when we encounter a new top-level declaration or blank line after a '.'
    collectDefinition [] = ([], [])
    collectDefinition (l:rest) = go [l] rest
      where
        go acc [] = (reverse acc, [])
        go acc (next:more)
          | "Definition " `isPrefixOf` next
          || "Fixpoint " `isPrefixOf` next
          || "Arguments " `isPrefixOf` next
          || "Axiom " `isPrefixOf` next
          || "(*" `isPrefixOf` next
          || "Require " `isPrefixOf` next
          || next == "" =
              (reverse acc, next:more)
          | otherwise = go (next:acc) more

    -- Take everything before ':=' in the full (potentially multi-line) definition
    takeBeforeAssign s = case findAssign s of
      Just idx -> take idx s
      Nothing -> s

    findAssign s = go 0 s
      where
        go _ [] = Nothing
        go i (':':'=':_) = Just i
        go i (_:cs) = go (i+1) cs

    convertSigToAxiom sig =
      let sig' = trimRight sig
          rest = if "Definition " `isPrefixOf` sig'
                 then drop (length "Definition ") sig'
                 else if "Fixpoint " `isPrefixOf` sig'
                      then drop (length "Fixpoint ") sig'
                      else sig'
      in case parseNameAndType rest of
           Just (name, typ) -> "Axiom " ++ name ++ " : " ++ typ ++ "."
           Nothing -> "(* Failed to convert: " ++ sig' ++ " *)"

    trimRight = reverse . dropWhile (\c -> c == ' ' || c == ':') . reverse

    parseNameAndType s =
      case break (== ' ') s of
        (name, []) -> Just (name, "unit")
        (name, ' ':rest) ->
          let rest' = dropWhile (== ' ') rest
          in if null rest' then Just (name, "unit")
             else if head rest' == ':'
                  then Just (name, dropWhile (== ' ') (tail rest'))
                  else case findTopLevelColon rest' of
                    Just idx ->
                      let binders = reverse (dropWhile (== ' ') (reverse (take idx rest')))
                          typ = dropWhile (== ' ') (drop (idx + 1) rest')
                      in Just (name, "forall " ++ binders ++ ", " ++ typ)
                    Nothing -> Just (name, rest')
        _ -> Nothing

    findTopLevelColon s = go 0 0 s
      where
        go _ _ [] = Nothing
        go depth idx (c:cs)
          | c == '(' = go (depth + 1) (idx + 1) cs
          | c == ')' = go (depth - 1) (idx + 1) cs
          | c == ':' && depth == 0 = Just idx
          | otherwise = go depth (idx + 1) cs

-- | Collect accessor names for fields that were sanitized due to positivity issues.
-- These accessors have type 'Record -> unit' but the Hydra code treats them as functions.
-- Returns a set of accessor names like "primitive_implementation".
collectSanitizedAccessors :: [(Bool, [(String, Core.Type)])] -> Set.Set String
collectSanitizedAccessors typeGroups =
  Set.fromList $ concatMap processGroup typeGroups
  where
    processGroup (_, group) =
      let groupNames = Set.fromList (map fst group)
          hasPositivity = hasPositivityIssueList groupNames group
      in if hasPositivity
         then concatMap (findSanitizedFields groupNames) group
         else []

    hasPositivityIssueList groupNames = any (\(_, ty) ->
      let (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeRecord fields -> any (checkFieldNegList groupNames) fields
        _ -> False)

    checkFieldNegList groupNames ft = checkNeg (Core.fieldTypeType ft)
      where
        checkNeg fty = case fty of
          Core.TypeFunction ftx ->
            hasGroupRefList groupNames (Core.functionTypeDomain ftx) || checkNeg (Core.functionTypeCodomain ftx)
          Core.TypeAnnotated at -> checkNeg (Core.annotatedTypeBody at)
          Core.TypeForall ftx -> checkNeg (Core.forallTypeBody ftx)
          Core.TypeWrap wt -> checkNeg wt
          _ -> False

    hasGroupRefList groupNames hty = case hty of
      Core.TypeVariable v -> localName (Core.unName v) `Set.member` groupNames
      Core.TypeApplication v -> hasGroupRefList groupNames (Core.applicationTypeFunction v) || hasGroupRefList groupNames (Core.applicationTypeArgument v)
      Core.TypeFunction v -> hasGroupRefList groupNames (Core.functionTypeDomain v) || hasGroupRefList groupNames (Core.functionTypeCodomain v)
      Core.TypeList v -> hasGroupRefList groupNames v
      Core.TypeMaybe v -> hasGroupRefList groupNames v
      Core.TypeSet v -> hasGroupRefList groupNames v
      Core.TypeAnnotated v -> hasGroupRefList groupNames (Core.annotatedTypeBody v)
      Core.TypeForall v -> hasGroupRefList groupNames (Core.forallTypeBody v)
      Core.TypeWrap v -> hasGroupRefList groupNames v
      _ -> False

    findSanitizedFields groupNames (typeName, ty) =
      let (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeRecord fields ->
          [decapitalize typeName ++ "_" ++ sanitize (localName (Core.unName (Core.fieldTypeName ft)))
           | ft <- fields, checkFieldNegList groupNames ft]
        _ -> []

-- | Replace applications of sanitized field accessors with hydra_unreachable.
-- The accessor itself (e.g., "primitive_implementation") has type Record -> unit
-- in the generated code, but the Hydra source treats it as a function. At call sites,
-- we replace the entire application chain with hydra_unreachable.
--
-- Target pattern: "(fun r_ => (ACCESSOR) (r_)) (REC)" where ACCESSOR is sanitized.
-- Replace with: "hydra_unreachable".
replaceSanitizedAccessors :: Set.Set String -> String -> String
replaceSanitizedAccessors sanitized body =
  if Set.null sanitized then body
  else foldl replaceOne body (Set.toList sanitized)
  where
    replaceOne text accessorName =
      -- Replace "(fun r_ => (accessorName) (r_))" with "(fun _ => hydra_unreachable)"
      let pattern1 = "(fun r_ => (" ++ accessorName ++ ") (r_))"
          replacement1 = "(fun _ : _ => hydra_unreachable)"
      in replaceAll pattern1 replacement1 text

    replaceAll needle replacement [] = []
    replaceAll needle replacement s@(c:cs)
      | needle `isPrefixOf` s = replacement ++ replaceAll needle replacement (drop (length needle) s)
      | otherwise = c : replaceAll needle replacement cs

-- | Extract common polymorphic helper patterns (forSingle, forMany) as top-level
-- definitions and replace inline let bindings with references. This dramatically
-- reduces Coq type inference overhead for modules with deeply nested rewriting
-- combinators (e.g., rewriting.v), preventing memory exhaustion during compilation.
extractPolymorphicHelpers :: String -> String
extractPolymorphicHelpers body =
  let -- Pattern 1: forSingle (no path dependency)
      forSingleInline = "let forSingle := fun (t2 : Type) => fun (t3 : Type) => fun (t4 : Type) => fun (t5 : Type) => fun (t6 : Type) => fun (rec : t2 -> t3 -> (prod) (t4) (t5)) => fun (cons_ : t5 -> t6) => fun (val : t2) => fun (term_ : t3) => let r := ((rec) (val)) (term_) in (pair) ((pairs.first) (r)) ((cons_) ((pairs.second) (r))) in "
      forSingleRef = "let forSingle := hydra_mapSingle in "
      -- Pattern 2: forMany (no path dependency)
      forManyInline = "let forMany := fun (t2 : Type) => fun (t3 : Type) => fun (t4 : Type) => fun (t5 : Type) => fun (rec : t2 -> t3 -> (prod) (t2) (t4)) => fun (cons_ : (list) (t4) -> t5) => fun (val : t2) => fun (els : (list) (t3)) => let rr := (((lists.foldl) (fun (r : (prod) (t2) ((list) (t4))) => fun (el : t3) => let r2 := ((rec) ((pairs.first) (r))) (el) in (pair) ((pairs.first) (r2)) (((lists.cons) ((pairs.second) (r2))) ((pairs.second) (r))))) ((pair) (val) (nil))) (els) in (pair) ((pairs.first) (rr)) ((cons_) ((lists.reverse) ((pairs.second) (rr)))) in "
      forManyRef = "let forMany := hydra_mapMany in "
      -- Pattern 3: forSingleWithAccessor (depends on 'path' from enclosing scope)
      forSingleWAInline = "let forSingleWithAccessor := fun (t2 : Type) => fun (t3 : Type) => fun (t4 : Type) => fun (t5 : Type) => fun (t6 : Type) => fun (rec : (list) (SubtermStep) -> t2 -> t3 -> (prod) (t4) (t5)) => fun (cons_ : t5 -> t6) => fun (accessor : SubtermStep) => fun (val : t2) => fun (term_ : t3) => let r := (((rec) (((lists.concat2) (path)) ((cons) (accessor) (nil)))) (val)) (term_) in (pair) ((pairs.first) (r)) ((cons_) ((pairs.second) (r))) in "
      forSingleWARef = "let forSingleWithAccessor := hydra_mapSingleWithAccessor path in "
      -- Pattern 4: forManyWithAccessors (depends on 'path' from enclosing scope)
      forManyWAInline = "let forManyWithAccessors := fun (t2 : Type) => fun (t3 : Type) => fun (t4 : Type) => fun (t5 : Type) => fun (rec : (list) (SubtermStep) -> t2 -> t3 -> (prod) (t2) (t4)) => fun (cons_ : (list) (t4) -> t5) => fun (val : t2) => fun (accessorTermPairs : (list) ((prod) (SubtermStep) (t3))) => let rr := (((lists.foldl) (fun (r : (prod) (t2) ((list) (t4))) => fun (atp : (prod) (SubtermStep) (t3)) => let r2 := (((rec) (((lists.concat2) (path)) ((cons) ((pairs.first) (atp)) (nil)))) ((pairs.first) (r))) ((pairs.second) (atp)) in (pair) ((pairs.first) (r2)) (((lists.cons) ((pairs.second) (r2))) ((pairs.second) (r))))) ((pair) (val) (nil))) (accessorTermPairs) in (pair) ((pairs.first) (rr)) ((cons_) ((lists.reverse) ((pairs.second) (rr)))) in "
      forManyWARef = "let forManyWithAccessors := hydra_mapManyWithAccessors path in "

      replacements = [(forSingleInline, forSingleRef), (forManyInline, forManyRef),
                      (forSingleWAInline, forSingleWARef), (forManyWAInline, forManyWARef)]
      hits = [(old, new) | (old, new) <- replacements, old `isInfixOf` body]
      body' = foldl (\b (old, new) -> replace old new b) body hits
  in if null hits then body
     else let helpers = unlines $ concat
               [["(* Extracted polymorphic helpers to reduce type inference overhead *)"]
               ,if forSingleInline `isInfixOf` body then
                 ["Definition hydra_mapSingle (t2 t3 t4 t5 t6 : Type)"
                 ,"  (rec : t2 -> t3 -> (prod) (t4) (t5)) (cons_ : t5 -> t6) (val : t2) (term : t3)"
                 ,"  : (prod) (t4) (t6) :="
                 ,"  let r := ((rec) (val)) (term) in (pair) ((pairs.first) (r)) ((cons_) ((pairs.second) (r)))."]
                else []
               ,if forManyInline `isInfixOf` body then
                 ["Definition hydra_mapMany (t2 t3 t4 t5 : Type)"
                 ,"  (rec : t2 -> t3 -> (prod) (t2) (t4)) (cons_ : (list) (t4) -> t5) (val : t2) (els : (list) (t3))"
                 ,"  : (prod) (t2) (t5) :="
                 ,"  let rr := (((lists.foldl) (fun (r : (prod) (t2) ((list) (t4))) => fun (el : t3) => let r2 := ((rec) ((pairs.first) (r))) (el) in (pair) ((pairs.first) (r2)) (((lists.cons) ((pairs.second) (r2))) ((pairs.second) (r))))) ((pair) (val) (nil))) (els) in (pair) ((pairs.first) (rr)) ((cons_) ((lists.reverse) ((pairs.second) (rr))))."]
                else []
               ,if forSingleWAInline `isInfixOf` body then
                 ["Definition hydra_mapSingleWithAccessor (path : (list) (SubtermStep)) (t2 t3 t4 t5 t6 : Type)"
                 ,"  (rec : (list) (SubtermStep) -> t2 -> t3 -> (prod) (t4) (t5)) (cons_ : t5 -> t6)"
                 ,"  (accessor : SubtermStep) (val : t2) (term_ : t3)"
                 ,"  : (prod) (t4) (t6) :="
                 ,"  let r := (((rec) (((lists.concat2) (path)) ((cons) (accessor) (nil)))) (val)) (term_) in (pair) ((pairs.first) (r)) ((cons_) ((pairs.second) (r)))."]
                else []
               ,if forManyWAInline `isInfixOf` body then
                 ["Definition hydra_mapManyWithAccessors (path : (list) (SubtermStep)) (t2 t3 t4 t5 : Type)"
                 ,"  (rec : (list) (SubtermStep) -> t2 -> t3 -> (prod) (t2) (t4)) (cons_ : (list) (t4) -> t5)"
                 ,"  (val : t2) (accessorTermPairs : (list) ((prod) (SubtermStep) (t3)))"
                 ,"  : (prod) (t2) (t5) :="
                 ,"  let rr := (((lists.foldl) (fun (r : (prod) (t2) ((list) (t4))) => fun (atp : (prod) (SubtermStep) (t3)) => let r2 := (((rec) (((lists.concat2) (path)) ((cons) ((pairs.first) (atp)) (nil)))) ((pairs.first) (r))) ((pairs.second) (atp)) in (pair) ((pairs.first) (r2)) (((lists.cons) ((pairs.second) (r2))) ((pairs.second) (r))))) ((pair) (val) (nil))) (accessorTermPairs) in (pair) ((pairs.first) (rr)) ((cons_) ((lists.reverse) ((pairs.second) (rr))))."]
                else []
               ,[""]
               ]
          in insertAfterImports helpers body'
  where
    replace old new [] = []
    replace old new s@(c:cs)
      | old `isPrefixOf` s = new ++ replace old new (drop (length old) s)
      | otherwise = c : replace old new cs

    insertAfterImports helpers text =
      let ls = lines text
          insertIdx = findLastImportLine 0 0 ls
          (before, after) = splitAt (insertIdx + 1) ls
      in unlines (before ++ ["", helpers] ++ after)

    findLastImportLine idx lastImport [] = lastImport
    findLastImportLine idx lastImport (l:ls)
      | "Require Import" `isPrefixOf` l = findLastImportLine (idx + 1) idx ls
      | otherwise = findLastImportLine (idx + 1) lastImport ls

-- | Extract type parameters (forall binders) from a type.
extractTypeParams :: Core.Type -> ([String], Core.Type)
extractTypeParams (Core.TypeForall ft) =
  let param = Core.unName (Core.forallTypeParameter ft)
      body = Core.forallTypeBody ft
      (moreParams, innerBody) = extractTypeParams body
  in (param : moreParams, innerBody)
extractTypeParams (Core.TypeAnnotated at) = extractTypeParams (Core.annotatedTypeBody at)
extractTypeParams ty = ([], ty)

-- | Extract term definitions from a module
extractTermDefs :: Pkg.Module -> [(String, Core.Term)]
extractTermDefs m = concatMap go (Pkg.moduleDefinitions m)
  where
    go (Pkg.DefinitionTerm td) =
      let name = localName (Core.unName $ Pkg.termDefinitionName td)
          term = Pkg.termDefinitionTerm td
      in [(name, term)]
    go _ = []

-- | Build a field name mapping from type definitions.
-- Maps (qualifiedTypeName, bareFieldName) -> prefixedFieldName.
-- E.g., for type "hydra.core.Binding" with field "name": "name" -> "binding_name"
-- Also builds a simple bare->prefixed map using the type name from the Projection.
buildFieldMapping :: [Pkg.Module] -> Map.Map (String, String) String
buildFieldMapping modules = Map.fromList $ concatMap moduleEntries modules
  where
    moduleEntries m = concatMap defEntries (Pkg.moduleDefinitions m)
    defEntries (Pkg.DefinitionType td) =
      let qname = Core.unName $ Pkg.typeDefinitionName td
          tname = localName qname  -- Use sanitized name to match accessor generation
          ty = Core.typeSchemeType (Pkg.typeDefinitionType td)
          (_, bodyTy) = extractTypeParams ty
      in case bodyTy of
        Core.TypeRecord fields -> map (\ft ->
          let rawFn = localNameRaw (Core.unName (Core.fieldTypeName ft))
              fn = sanitize rawFn
              prefixed = decapitalize tname ++ "_" ++ fn
          in ((qname, rawFn), prefixed)) fields
        _ -> []
    defEntries _ = []
    -- Like localName but without sanitize
    localNameRaw s = case splitOn '.' s of
      [] -> s
      parts -> last parts

-- | Rewrite terms to use prefixed field names in record projections.
rewriteTermFields :: Map.Map (String, String) String -> Core.Term -> Core.Term
rewriteTermFields fm = go
  where
    go tm = case tm of
      Core.TermAnnotated v -> Core.TermAnnotated v { Core.annotatedTermBody = go (Core.annotatedTermBody v) }
      Core.TermApplication v -> Core.TermApplication v
        { Core.applicationFunction = go (Core.applicationFunction v)
        , Core.applicationArgument = go (Core.applicationArgument v) }
      Core.TermEither v -> Core.TermEither $ case v of
        Left l -> Left (go l)
        Right r -> Right (go r)
      Core.TermLambda lam -> Core.TermLambda lam { Core.lambdaBody = go (Core.lambdaBody lam) }
      Core.TermProject proj ->
        let tname = Core.unName (Core.projectionTypeName proj)
            fname = Core.unName (Core.projectionField proj)
            key = (tname, localNameRaw fname)
            newFname = case Map.lookup key fm of
              Just prefixed -> Core.Name prefixed
              Nothing -> Core.projectionField proj
        in Core.TermProject proj { Core.projectionField = newFname }
      Core.TermCases cs -> Core.TermCases cs
        { Core.caseStatementCases = map (\f -> f { Core.fieldTerm = go (Core.fieldTerm f) }) (Core.caseStatementCases cs)
        , Core.caseStatementDefault = fmap go (Core.caseStatementDefault cs) }
      Core.TermUnwrap w -> Core.TermUnwrap w
      Core.TermLet v -> Core.TermLet v
        { Core.letBindings = map (\b -> b { Core.bindingTerm = go (Core.bindingTerm b) }) (Core.letBindings v)
        , Core.letBody = go (Core.letBody v) }
      Core.TermList v -> Core.TermList (map go v)
      Core.TermMap v -> Core.TermMap v  -- Map keys/values are rarely accessed by field name
      Core.TermMaybe v -> Core.TermMaybe (fmap go v)
      Core.TermPair v -> Core.TermPair (go (fst v), go (snd v))
      Core.TermRecord v -> Core.TermRecord v { Core.recordFields = map (\f -> f { Core.fieldTerm = go (Core.fieldTerm f) }) (Core.recordFields v) }
      Core.TermInject v -> Core.TermInject v { Core.injectionField = (Core.injectionField v) { Core.fieldTerm = go (Core.fieldTerm (Core.injectionField v)) } }
      Core.TermTypeApplication v -> Core.TermTypeApplication v { Core.typeApplicationTermBody = go (Core.typeApplicationTermBody v) }
      Core.TermTypeLambda v -> Core.TermTypeLambda v { Core.typeLambdaBody = go (Core.typeLambdaBody v) }
      Core.TermWrap v -> Core.TermWrap v { Core.wrappedTermBody = go (Core.wrappedTermBody v) }
      _ -> tm  -- Literal, Set, Unit, Variable

    localNameRaw s = case splitOn '.' s of
      [] -> s
      parts -> last parts

-- | Get the local name from a qualified name, sanitized for Coq
-- e.g., "hydra.core.Type" -> "Type_"
localName :: String -> String
localName s = sanitize $ case splitOn '.' s of
  [] -> s
  parts -> last parts

-- | Sanitize a name to avoid Coq reserved words
sanitize :: String -> String
sanitize s
  | s `elem` coqReserved = s ++ "_"
  | otherwise = s
  where
    coqReserved = [
      -- Gallina keywords
      "as", "at", "cofix", "do", "else", "end", "exists", "exists2", "fix",
      "for", "forall", "fun", "if", "IF", "in", "let", "match",
      "mod", "Prop", "return", "Set", "then", "Type", "using",
      "where", "with",
      -- Vernacular keywords that conflict
      "Axiom", "Class", "Coercion", "Context",
      "Definition", "Fixpoint", "Hypothesis", "Inductive",
      "Instance", "Lemma", "Module", "Notation", "Proof", "Qed",
      "Record", "Require", "Import", "Section", "End",
      "Theorem", "Example", "Variable", "Variables",
      -- Stdlib types
      "bool", "nat", "list", "option", "prod", "sum", "unit",
      "string", "String", "Empty_set",
      -- Stdlib values
      "true", "false", "None", "Some", "nil", "cons", "pair",
      "inl", "inr", "tt",
      -- Hydra module names that conflict with generated definition names
      "graph"]

-- | Extract the set of local type names referenced by a Hydra type expression.
-- Only returns names that are in the provided set of locally-defined names.
typeRefs :: Set.Set String -> String -> Core.Type -> Set.Set String
typeRefs locals ns ty = case ty of
  Core.TypeAnnotated v -> typeRefs locals ns (Core.annotatedTypeBody v)
  Core.TypeApplication v -> Set.union
    (typeRefs locals ns (Core.applicationTypeFunction v))
    (typeRefs locals ns (Core.applicationTypeArgument v))
  Core.TypeEither v -> Set.union
    (typeRefs locals ns (Core.eitherTypeLeft v))
    (typeRefs locals ns (Core.eitherTypeRight v))
  Core.TypeForall v -> typeRefs locals ns (Core.forallTypeBody v)
  Core.TypeFunction v -> Set.union
    (typeRefs locals ns (Core.functionTypeDomain v))
    (typeRefs locals ns (Core.functionTypeCodomain v))
  Core.TypeList v -> typeRefs locals ns v
  Core.TypeLiteral _ -> Set.empty
  Core.TypeMap v -> Set.union
    (typeRefs locals ns (Core.mapTypeKeys v))
    (typeRefs locals ns (Core.mapTypeValues v))
  Core.TypeMaybe v -> typeRefs locals ns v
  Core.TypePair v -> Set.union
    (typeRefs locals ns (Core.pairTypeFirst v))
    (typeRefs locals ns (Core.pairTypeSecond v))
  Core.TypeRecord v -> Set.unions $ map (typeRefs locals ns . Core.fieldTypeType) v
  Core.TypeSet v -> typeRefs locals ns v
  Core.TypeUnion v -> Set.unions $ map (typeRefs locals ns . Core.fieldTypeType) v
  Core.TypeUnit -> Set.empty
  Core.TypeVariable v ->
    let raw = Core.unName v
        local = localName raw
    in if local `Set.member` locals then Set.singleton local else Set.empty
  Core.TypeVoid -> Set.empty
  Core.TypeWrap v -> typeRefs locals ns v

-- | Sort type definitions using SCCs to handle mutual recursion.
-- Returns (isCyclic, group) pairs. Cyclic groups need Inductive ... with ... blocks.
sortTypeDefsSCC :: String -> [(String, Core.Type)] -> [(Bool, [(String, Core.Type)])]
sortTypeDefsSCC ns defs =
  let localNames = Set.fromList (map fst defs)
      -- Build graph edges: (node, key, [dependencies])
      graphNodes = [(d, fst d, Set.toList (typeRefs localNames ns (snd d))) | d <- defs]
      sccs = stronglyConnComp graphNodes
  in map sccToGroup sccs
  where
    sccToGroup (AcyclicSCC x) = (False, [x])
    sccToGroup (CyclicSCC xs) = (True, xs)

-- | Extract local names referenced by a term expression.
termRefs :: Set.Set String -> String -> Core.Term -> Set.Set String
termRefs locals ns tm = case tm of
  Core.TermAnnotated v -> termRefs locals ns (Core.annotatedTermBody v)
  Core.TermApplication v -> Set.union
    (termRefs locals ns (Core.applicationFunction v))
    (termRefs locals ns (Core.applicationArgument v))
  Core.TermEither v -> case v of
    Left l -> termRefs locals ns l
    Right r -> termRefs locals ns r
  Core.TermLambda lam -> termRefs locals ns (Core.lambdaBody lam)
  Core.TermProject _ -> Set.empty
  Core.TermCases cs ->
    Set.unions $ map (\f -> termRefs locals ns (Core.fieldTerm f)) (Core.caseStatementCases cs)
      ++ maybe [] (\d -> [termRefs locals ns d]) (Core.caseStatementDefault cs)
  Core.TermUnwrap _ -> Set.empty
  Core.TermLet v ->
    let bindingRefs = Set.unions $ map (\b -> termRefs locals ns (Core.bindingTerm b)) (Core.letBindings v)
    in Set.union bindingRefs (termRefs locals ns (Core.letBody v))
  Core.TermList v -> Set.unions $ map (termRefs locals ns) v
  Core.TermLiteral _ -> Set.empty
  Core.TermMap _ -> Set.empty
  Core.TermMaybe v -> maybe Set.empty (termRefs locals ns) v
  Core.TermPair v -> Set.union (termRefs locals ns (fst v)) (termRefs locals ns (snd v))
  Core.TermRecord v -> Set.unions $ map (termRefs locals ns . Core.fieldTerm) (Core.recordFields v)
  Core.TermSet _ -> Set.empty
  Core.TermTypeApplication v -> termRefs locals ns (Core.typeApplicationTermBody v)
  Core.TermTypeLambda v -> termRefs locals ns (Core.typeLambdaBody v)
  Core.TermInject v -> termRefs locals ns (Core.fieldTerm (Core.injectionField v))
  Core.TermUnit -> Set.empty
  Core.TermVariable v ->
    let raw = Core.unName v
        local = localName raw
    in if local `Set.member` locals then Set.singleton local else Set.empty
  Core.TermWrap v -> termRefs locals ns (Core.wrappedTermBody v)

-- | Topologically sort term definitions into SCC groups.
-- Returns (isCyclic, [(name, term)]) groups in dependency order.
-- Acyclic singletons become Definition; cyclic groups become Fixpoint ... with ...
sortTermDefsSCC :: String -> [(String, Core.Term)] -> [(Bool, [(String, Core.Term)])]
sortTermDefsSCC ns defs =
  let localNames = Set.fromList (map fst defs)
      graphNodes = [(d, fst d, Set.toList (termRefs localNames ns (snd d))) | d <- defs]
      sccs = stronglyConnComp graphNodes
  in map sccToGroup sccs
  where
    sccToGroup (AcyclicSCC x) = (False, [x])
    sccToGroup (CyclicSCC xs) = (True, xs)

-- | Collect all dependency namespaces for a module (both type and term deps).
moduleDependencies :: Pkg.Module -> [String]
moduleDependencies m =
  let typeDeps = map Pkg.unNamespace (Pkg.moduleTypeDependencies m)
      termDeps = map Pkg.unNamespace (Pkg.moduleTermDependencies m)
      ownNs = Pkg.unNamespace (Pkg.moduleNamespace m)
  in nub $ filter (/= ownNs) (typeDeps ++ termDeps)

-- | For type definitions, lift leading forall binders from the body into
-- the Definition's binder list. This transforms:
--   Definition Foo : Type := forall (a : Type), forall (b : Type), body.
-- into:
--   Definition Foo (a : Type) (b : Type) : Type := body.
-- This is necessary because Coq allows type application (Foo nat bool)
-- only when parameters are in the binder position.
liftTypeForalls :: Coq.Document -> Coq.Document
liftTypeForalls doc = doc { Coq.documentSentences = map liftSentence (Coq.documentSentences doc) }
  where
    liftSentence s = s { Coq.sentenceContent = liftContent (Coq.sentenceContent s) }
    liftContent (Coq.SentenceContentDefinition d)
      | isTypeDefinition d = Coq.SentenceContentDefinition (liftDef d)
    liftContent c = c
    -- A definition is a type definition if it has `: Type` annotation
    isTypeDefinition d = case Coq.definitionType d of
      Just (Coq.Type (Coq.TermTerm100 (Coq.Term100Term10 (Coq.Term10OneTerm
        (Coq.OneTermExplicit (Coq.QualidAnnotated q _)))))) ->
          Coq.qualidId q == Coq.Ident (Coq.String_ "Type")
      _ -> False
    liftDef d =
      let (binders, body) = extractForalls (Coq.definitionBody d)
      in d { Coq.definitionBinders = Coq.definitionBinders d ++ binders
           , Coq.definitionBody = body }
    -- Extract leading forall binders that bind Type-kinded variables
    extractForalls (Coq.TermForallOrFun (Coq.ForallOrFunForall (Coq.Forall
      (Coq.OpenBindersBinders binders) (Coq.Type body))))
      | all isTypeBinder binders =
          let (moreBinders, innerBody) = extractForalls body
          in (binders ++ moreBinders, innerBody)
    extractForalls t = ([], t)
    isTypeBinder (Coq.BinderType (Coq.TypeBinders _ (Coq.Type
      (Coq.TermTerm100 (Coq.Term100Term10 (Coq.Term10OneTerm
        (Coq.OneTermExplicit (Coq.QualidAnnotated q _)))))))) =
          Coq.qualidId q == Coq.Ident (Coq.String_ "Type")
    isTypeBinder _ = False

-- | Generate Require Import statements for dependency modules.
dependencyImports :: [String] -> [Coq.Sentence]
dependencyImports deps =
  if null deps then []
  else [Coq.Sentence
    { Coq.sentenceComment = Just (Coq.Comment "Module dependencies")
    , Coq.sentenceContent = Coq.SentenceContentRequireImport (Coq.RequireImport
      { Coq.requireImportFrom = Nothing
      , Coq.requireImportRequire = True
      , Coq.requireImportQualification = Just Coq.ImportQualificationImport
      , Coq.requireImportModules = map (Coder.coqQualid . namespaceToCoqModule) deps
      })
    }]

-- | Scan text for all hydra.xxx.yyy qualified references and extract
-- the namespace parts (everything except the last component).
-- E.g., "hydra.core.Term_Literal" -> "hydra.core"
-- E.g., "hydra.lib.strings.cat" -> "hydra.lib.strings"
extractReferencedNamespaces :: String -> String -> [String]
extractReferencedNamespaces ownNs body =
  let refs = extractQualifiedNames body
      namespaces = nub $ map nameToNamespace refs
      -- Filter out own namespace and directory-conflicting namespaces
      -- (e.g., "hydra.lib" conflicts with hydra/lib/ directory)
      filtered = filter (\ns -> ns /= ownNs && not (isDirectoryPrefix ns namespaces)) namespaces
  in filtered
  where
    -- A namespace is a directory prefix if another namespace starts with it + "."
    isDirectoryPrefix ns' nss = any (\other -> other /= ns' && (ns' ++ ".") `isPrefixOf` other) nss

    nameToNamespace qn = case splitOn '.' qn of
      [] -> qn
      parts -> intercalate "." (init parts)

    extractQualifiedNames [] = []
    extractQualifiedNames ('"':cs) =
      -- Skip string literals to avoid picking up hydra.* references inside strings
      extractQualifiedNames (drop 1 (dropWhile (/= '"') cs))
    extractQualifiedNames s@(_:cs)
      | "hydra." `isPrefixOf` s =
          let qname = takeQualifiedName s
              parts = splitOn '.' qname
          in if length parts >= 3  -- at least hydra.X.Y
             then qname : extractQualifiedNames (drop (length qname) s)
             else extractQualifiedNames cs
      | otherwise = extractQualifiedNames cs

    -- Take a qualified name: segments of identifier chars separated by dots,
    -- but don't include a trailing dot (which is a Coq sentence terminator).
    takeQualifiedName str =
      let seg = takeWhile isWordChar str
          rest = drop (length seg) str
      in case rest of
          ('.':c':_) | isWordChar c' -> seg ++ "." ++ takeQualifiedName (tail rest)
          _ -> seg

    isWordChar c' = isAlphaNum c' || c' == '_'

-- | Generate a .v file for a single module using the direct pretty-printer.
-- Returns (filePath, content).
generateModule :: Map.Map (String, String) String -> Pkg.Module -> (FilePath, String)
generateModule fieldMap m =
  let ns = Pkg.unNamespace (Pkg.moduleNamespace m)
      path = namespaceToPath ns
      termGroups = sortTermDefsSCC ns (extractTermDefs m)
      -- Flatten and rewrite term definitions to use prefixed field names
      termDefs = [(n, rewriteTermFields fieldMap t) | (_, group) <- termGroups, (n, t) <- group]
      desc = maybe "" (\d -> "(* " ++ d ++ " *)\n\n") (Pkg.moduleDescription m)

      -- Build type sentences with SCC-based mutual recursion support
      typeGroups = sortTypeDefsSCC ns (extractTypeDefs m)
      typeSentences = concatMap generateTypeGroup typeGroups

      -- Build term sentences using the generated DSL runtime
      termSentences = map Coder.encodeTermDefinitionPair termDefs

      -- Assemble document with standard imports
      doc = Coq.Document (Coder.standardImports : typeSentences ++ termSentences)

      -- Render without imports first to discover all referenced namespaces
      rawBody = Print.printDocument doc

      -- Discover all referenced hydra.xxx namespaces from the body
      referencedNs = extractReferencedNamespaces ns rawBody
      depSentences = dependencyImports referencedNs

      -- Add dependency imports after the standard imports
      doc' = doc { Coq.documentSentences =
        let ss = Coq.documentSentences doc
        in case ss of
          (stdImport:rest) -> stdImport : depSentences ++ rest
          _ -> depSentences ++ ss
        }

      -- Re-render with imports, then strip hydra qualifications
      bodyWithImports = Print.printDocument doc'
      body = stripHydraQualifications ns Set.empty bodyWithImports

  in (path, desc ++ body ++ "\n")

-- | Strip all hydra.xxx qualified name prefixes from a string,
-- except on Require Import lines (which need full module paths).
-- E.g., "hydra.core.Term_Literal" -> "Term_Literal"
-- E.g., "hydra.lib.strings.cat" -> "cat"
-- E.g., "Build_hydra.core.Field" -> "Build_Field"
-- Extracts the last dot-separated component and applies sanitize to it.
stripHydraQualifications :: String -> Set.Set String -> String -> String
stripHydraQualifications ownNs localDefs = unlines . map processLine . lines
  where
    processLine line
      | "Require " `isPrefixOf` line = line  -- don't touch import lines
      | otherwise = stripInLine line

    stripInLine [] = []
    stripInLine s@(c:cs)
      | "Build_hydra." `isPrefixOf` s =
          let qname = takeQName (drop (length "Build_") s)
              localN = sanitize (lastComponent qname)
              rest = drop (length "Build_" + length qname) s
          in "Build_" ++ localN ++ stripInLine rest
      | "hydra.lib." `isPrefixOf` s =
          -- For lib references, keep module.function (e.g., "sets.insert")
          -- to avoid name collisions between lib modules.
          let qname = takeQName s
              shortened = stripLibPrefix qname
              rest = drop (length qname) s
          in shortened ++ stripInLine rest
      | "hydra." `isPrefixOf` s =
          let qname = takeQName s
              qnameNs = nameToNamespace qname
              parts = splitOn '.' qname
              localName' = sanitize (lastComponent qname)
              -- When the stripped name would collide with a local definition
              -- and the reference is from a different namespace, keep the fully
              -- qualified name. Coq accepts qualified references after Require Import.
              -- Build qualified name with sanitized local part
              qualifiedN = qnameNs ++ "." ++ localName'
              localN
                | qnameNs /= ownNs && Set.member localName' localDefs = qualifiedN
                -- For modules with collision-prone function names, keep module.function
                | otherwise = case parts of
                    ["hydra", modName, funcName]
                      | modName `elem` qualifiedModules && qnameNs /= ownNs ->
                        sanitize modName ++ "." ++ sanitize funcName
                    _ -> localName'
              rest = drop (length qname) s
          in localN ++ stripInLine rest
      | otherwise = c : stripInLine cs

    nameToNamespace qn = case splitOn '.' qn of
      [] -> qn
      parts' -> intercalate "." (init parts')

    -- Non-lib modules whose function names may collide with other imports
    -- (e.g., parsers.map vs maps.map). Keep module.function format for these.
    qualifiedModules = ["parsers"]

    -- Strip "hydra.lib." prefix, keeping "module.function"
    -- E.g., "hydra.lib.sets.insert" -> "sets.insert"
    -- Also renames Coq keywords: "lists.at" -> "lists.at_", "math.mod" -> "math.mod_"
    stripLibPrefix qname =
      let short = drop (length "hydra.lib.") qname
      in renameCoqKeywords short

    -- Rename Coq keywords that can't be used as identifiers
    renameCoqKeywords s
      | s == "lists.at" = "lists.at_"
      | s == "math.mod" = "math.mod_"
      | otherwise = s

    -- Take a qualified name without grabbing trailing dots
    takeQName str =
      let seg = takeWhile isWordChar str
          rest' = drop (length seg) str
      in case rest' of
          ('.':c':_) | isWordChar c' -> seg ++ "." ++ takeQName (tail rest')
          _ -> seg

    isWordChar c' = isAlphaNum c' || c' == '_'
    lastComponent qn = case splitOn '.' qn of
      [] -> qn
      parts -> last parts

-- | Generate all .v files for a list of modules.
-- Returns list of (filePath, content) pairs.
generateAll :: [Pkg.Module] -> [(FilePath, String)]
generateAll modules =
  let fm = buildFieldMapping modules
  in map (generateModule fm) modules

-- | Write all generated .v files to a base directory.
writeAll :: FilePath -> [Pkg.Module] -> IO Int
writeAll baseDir modules = do
  let files = generateAll modules
  mapM_ (\(path, content) -> do
    let fullPath = baseDir </> path
    createDirectoryIfMissing True (takeDirectory fullPath)
    writeFile fullPath content
    putStrLn $ "  " ++ path) files
  return (length files)

-- | Generate a _CoqProject file for the output directory.
-- Maps the physical directory to the empty logical path so that
-- e.g. hydra/Core.v becomes module "hydra.Core".
writeCoqProject :: FilePath -> IO ()
writeCoqProject baseDir = do
  let content = unlines
        [ "# Generated by Hydra's Coq code generator"
        , "-Q . \"\""
        ]
  writeFile (baseDir </> "_CoqProject") content
  putStrLn "  _CoqProject"
