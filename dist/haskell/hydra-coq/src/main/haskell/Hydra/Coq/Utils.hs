-- Note: this is an automatically generated file. Do not edit.

-- | Pure helpers for the Coq code generator

module Hydra.Coq.Utils where

import qualified Hydra.Coq.Language as Language
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Sorting as Sorting
import qualified Hydra.Variables as Variables
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S

-- | Build a map from each union-type definition's name to its constructor count
buildConstructorCounts :: Ord t0 => ([(t0, Core.Type)] -> M.Map t0 Int)
buildConstructorCounts defs =
    Maps.fromList (Lists.concat (Lists.map (\nt ->
      let name = Pairs.first nt
          ty = Pairs.second nt
          extracted = extractTypeParams ty
          bodyTy = Pairs.second extracted
      in case bodyTy of
        Core.TypeUnion v0 -> [
          (name, (Lists.length v0))]
        _ -> []) defs))

-- | Build a map keyed by (qualifiedTypeName, rawFieldName) producing the prefixed Coq accessor name
buildFieldMapping :: [Packaging.Module] -> M.Map (String, String) String
buildFieldMapping modules =
    Maps.fromList (Lists.concat (Lists.map (\m -> Lists.concat (Lists.map (\def_ -> case def_ of
      Packaging.DefinitionType v0 ->
        let qname = Core.unName (Packaging.typeDefinitionName v0)
            tname = localName qname
            ty = Core.typeSchemeType (Packaging.typeDefinitionType v0)
            extracted = extractTypeParams ty
            bodyTy = Pairs.second extracted
        in case bodyTy of
          Core.TypeRecord v1 -> Lists.map (\ft ->
            let rawFn = localNameRaw (Core.unName (Core.fieldTypeName ft))
                fn = sanitize rawFn
                prefixed =
                        Strings.cat [
                          Formatting.decapitalize tname,
                          "_",
                          fn]
            in ((qname, rawFn), prefixed)) v1
          _ -> []
      _ -> []) (Packaging.moduleDefinitions m))) modules))

-- | Collect the set of free type-variable-like names (t0, t1, ...) referenced anywhere inside a Term
collectFreeTypeVars :: Core.Term -> S.Set String
collectFreeTypeVars tm =
    case tm of
      Core.TermAnnotated v0 -> collectFreeTypeVars (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Sets.union (collectFreeTypeVars (Core.applicationFunction v0)) (collectFreeTypeVars (Core.applicationArgument v0))
      Core.TermCases v0 -> Sets.union (Maybes.maybe Sets.empty (\d -> collectFreeTypeVars d) (Core.caseStatementDefault v0)) (Sets.unions (Lists.map (\f -> collectFreeTypeVars (Core.fieldTerm f)) (Core.caseStatementCases v0)))
      Core.TermEither v0 -> Eithers.either (\l -> collectFreeTypeVars l) (\r -> collectFreeTypeVars r) v0
      Core.TermInject v0 -> collectFreeTypeVars (Core.fieldTerm (Core.injectionField v0))
      Core.TermLambda v0 ->
        let paramName = Core.unName (Core.lambdaParameter v0)
            domVars = Maybes.maybe Sets.empty (\dty -> collectFreeTypeVarsInType dty) (Core.lambdaDomain v0)
            bodyVars = collectFreeTypeVars (Core.lambdaBody v0)
            allVars = Sets.union domVars bodyVars
        in (Logic.ifElse (isTypeVarLike paramName) (Sets.delete paramName allVars) allVars)
      Core.TermLet v0 ->
        let bindVars =
                Sets.unions (Lists.map (\b -> Sets.union (collectFreeTypeVars (Core.bindingTerm b)) (Maybes.maybe Sets.empty (\sch -> collectFreeTypeVarsInTypeScheme sch) (Core.bindingType b))) (Core.letBindings v0))
        in (Sets.union bindVars (collectFreeTypeVars (Core.letBody v0)))
      Core.TermList v0 -> Sets.unions (Lists.map (\el -> collectFreeTypeVars el) v0)
      Core.TermMaybe v0 -> Maybes.maybe Sets.empty (\el -> collectFreeTypeVars el) v0
      Core.TermPair v0 -> Sets.union (collectFreeTypeVars (Pairs.first v0)) (collectFreeTypeVars (Pairs.second v0))
      Core.TermRecord v0 -> Sets.unions (Lists.map (\f -> collectFreeTypeVars (Core.fieldTerm f)) (Core.recordFields v0))
      Core.TermSet v0 -> Sets.unions (Lists.map (\el -> collectFreeTypeVars el) (Sets.toList v0))
      Core.TermTypeApplication v0 -> collectFreeTypeVars (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> Sets.delete (Core.unName (Core.typeLambdaParameter v0)) (collectFreeTypeVars (Core.typeLambdaBody v0))
      Core.TermWrap v0 -> collectFreeTypeVars (Core.wrappedTermBody v0)
      _ -> Sets.empty

-- | Collect names of type-variable-like references (t0, t1, ...) inside a Type
collectFreeTypeVarsInType :: Core.Type -> S.Set String
collectFreeTypeVarsInType ty =
    case ty of
      Core.TypeAnnotated v0 -> collectFreeTypeVarsInType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Sets.union (collectFreeTypeVarsInType (Core.applicationTypeFunction v0)) (collectFreeTypeVarsInType (Core.applicationTypeArgument v0))
      Core.TypeForall v0 -> collectFreeTypeVarsInType (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Sets.union (collectFreeTypeVarsInType (Core.functionTypeDomain v0)) (collectFreeTypeVarsInType (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> collectFreeTypeVarsInType v0
      Core.TypeMap v0 -> Sets.union (collectFreeTypeVarsInType (Core.mapTypeKeys v0)) (collectFreeTypeVarsInType (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> collectFreeTypeVarsInType v0
      Core.TypePair v0 -> Sets.union (collectFreeTypeVarsInType (Core.pairTypeFirst v0)) (collectFreeTypeVarsInType (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Sets.unions (Lists.map (\f -> collectFreeTypeVarsInType (Core.fieldTypeType f)) v0)
      Core.TypeSet v0 -> collectFreeTypeVarsInType v0
      Core.TypeUnion v0 -> Sets.unions (Lists.map (\f -> collectFreeTypeVarsInType (Core.fieldTypeType f)) v0)
      Core.TypeVariable v0 ->
        let nm = Core.unName v0
        in (Logic.ifElse (isTypeVarLike nm) (Sets.singleton nm) Sets.empty)
      Core.TypeWrap v0 -> collectFreeTypeVarsInType v0
      _ -> Sets.empty

-- | Collect type-variable-like names declared or referenced by a TypeScheme
collectFreeTypeVarsInTypeScheme :: Core.TypeScheme -> S.Set String
collectFreeTypeVarsInTypeScheme ts =

      let explicit =
              Sets.fromList (Lists.map (\n -> Core.unName n) (Lists.filter (\n -> isTypeVarLike (Core.unName n)) (Core.typeSchemeVariables ts)))
      in (Sets.union explicit (collectFreeTypeVarsInType (Core.typeSchemeType ts)))

-- | Flatten consecutive TermLet wrappers into (bindings, innermostBody)
collectLetBindings :: Core.Term -> ([Core.Binding], Core.Term)
collectLetBindings tm =
    case tm of
      Core.TermLet v0 ->
        let rest = collectLetBindings (Core.letBody v0)
        in (Lists.concat2 (Core.letBindings v0) (Pairs.first rest), (Pairs.second rest))
      _ -> ([], tm)

-- | Collect the set of qualified (hydra.*) Name strings that a Hydra Term references
collectQualifiedNamesInTerm :: Core.Term -> S.Set String
collectQualifiedNamesInTerm tm =
    case tm of
      Core.TermAnnotated v0 -> collectQualifiedNamesInTerm (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Sets.union (collectQualifiedNamesInTerm (Core.applicationFunction v0)) (collectQualifiedNamesInTerm (Core.applicationArgument v0))
      Core.TermCases v0 -> Sets.union (qualifiedFromName (Core.caseStatementTypeName v0)) (Sets.union (Sets.unions (Lists.map (\f -> collectQualifiedNamesInTerm (Core.fieldTerm f)) (Core.caseStatementCases v0))) (Maybes.maybe Sets.empty (\d -> collectQualifiedNamesInTerm d) (Core.caseStatementDefault v0)))
      Core.TermEither v0 -> Eithers.either (\l -> collectQualifiedNamesInTerm l) (\r -> collectQualifiedNamesInTerm r) v0
      Core.TermInject v0 -> Sets.union (qualifiedFromName (Core.injectionTypeName v0)) (collectQualifiedNamesInTerm (Core.fieldTerm (Core.injectionField v0)))
      Core.TermLambda v0 -> Sets.union (Maybes.maybe Sets.empty (\domTy -> collectQualifiedNamesInType domTy) (Core.lambdaDomain v0)) (collectQualifiedNamesInTerm (Core.lambdaBody v0))
      Core.TermLet v0 -> Sets.union (Sets.unions (Lists.map (\b -> collectQualifiedNamesInTerm (Core.bindingTerm b)) (Core.letBindings v0))) (collectQualifiedNamesInTerm (Core.letBody v0))
      Core.TermList v0 -> Sets.unions (Lists.map (\el -> collectQualifiedNamesInTerm el) v0)
      Core.TermMaybe v0 -> Maybes.maybe Sets.empty (\el -> collectQualifiedNamesInTerm el) v0
      Core.TermPair v0 -> Sets.union (collectQualifiedNamesInTerm (Pairs.first v0)) (collectQualifiedNamesInTerm (Pairs.second v0))
      Core.TermRecord v0 -> Sets.union (qualifiedFromName (Core.recordTypeName v0)) (Sets.unions (Lists.map (\f -> collectQualifiedNamesInTerm (Core.fieldTerm f)) (Core.recordFields v0)))
      Core.TermTypeApplication v0 -> Sets.union (collectQualifiedNamesInTerm (Core.typeApplicationTermBody v0)) (collectQualifiedNamesInType (Core.typeApplicationTermType v0))
      Core.TermTypeLambda v0 -> collectQualifiedNamesInTerm (Core.typeLambdaBody v0)
      Core.TermVariable v0 -> qualifiedFromName v0
      Core.TermWrap v0 -> collectQualifiedNamesInTerm (Core.wrappedTermBody v0)
      _ -> Sets.empty

-- | Collect the set of qualified (hydra.*) Name strings that a Hydra Type references
collectQualifiedNamesInType :: Core.Type -> S.Set String
collectQualifiedNamesInType ty =
    case ty of
      Core.TypeAnnotated v0 -> collectQualifiedNamesInType (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Sets.union (collectQualifiedNamesInType (Core.applicationTypeFunction v0)) (collectQualifiedNamesInType (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Sets.union (collectQualifiedNamesInType (Core.eitherTypeLeft v0)) (collectQualifiedNamesInType (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> collectQualifiedNamesInType (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Sets.union (collectQualifiedNamesInType (Core.functionTypeDomain v0)) (collectQualifiedNamesInType (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> collectQualifiedNamesInType v0
      Core.TypeMap v0 -> Sets.union (collectQualifiedNamesInType (Core.mapTypeKeys v0)) (collectQualifiedNamesInType (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> collectQualifiedNamesInType v0
      Core.TypePair v0 -> Sets.union (collectQualifiedNamesInType (Core.pairTypeFirst v0)) (collectQualifiedNamesInType (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Sets.unions (Lists.map (\f -> collectQualifiedNamesInType (Core.fieldTypeType f)) v0)
      Core.TypeSet v0 -> collectQualifiedNamesInType v0
      Core.TypeUnion v0 -> Sets.unions (Lists.map (\f -> collectQualifiedNamesInType (Core.fieldTypeType f)) v0)
      Core.TypeVariable v0 -> qualifiedFromName v0
      Core.TypeWrap v0 -> collectQualifiedNamesInType v0
      _ -> Sets.empty

-- | Collect qualified (hydra.*) Name strings from a TypeScheme's body, after stripping forall binders
collectQualifiedNamesInTypeScheme :: Core.TypeScheme -> S.Set String
collectQualifiedNamesInTypeScheme ts =

      let extracted = extractTypeParams (Core.typeSchemeType ts)
          body = Pairs.second extracted
      in (collectQualifiedNamesInType body)

-- | Return the set of decapitalized, sanitized accessor names whose fields were replaced with unit due to positivity issues
collectSanitizedAccessors :: [(t0, [(String, Core.Type)])] -> S.Set String
collectSanitizedAccessors typeGroups =
    Sets.fromList (Lists.concat (Lists.map (\group ->
      let defs = Pairs.second group
          groupNames = Sets.fromList (Lists.map (\nt -> Pairs.first nt) defs)
      in (Logic.ifElse (hasPositivityIssue groupNames defs) (Lists.concat (Lists.map (\nt ->
        let typeName = Pairs.first nt
            ty = Pairs.second nt
            extracted = extractTypeParams ty
            bodyTy = Pairs.second extracted
        in case bodyTy of
          Core.TypeRecord v0 -> Maybes.cat (Lists.map (\f -> Logic.ifElse (fieldCausesPositivityIssue groupNames (Core.fieldTypeType f)) (Just (Strings.cat [
            Formatting.decapitalize typeName,
            "_",
            (sanitize (localName (Core.unName (Core.fieldTypeName f))))])) Nothing) v0)
          _ -> []) defs)) [])) typeGroups))

-- | Wrap a mutually recursive binding group in a hydra_fix nested-pair bundle with per-name projection lets
encodeMutualLetGroup :: [Core.Binding] -> Core.Term -> Core.Term
encodeMutualLetGroup grp body =

      let n = Lists.length grp
          bundleName = Core.Name "hydra_mutual_bundle_"
          bundleInner = Core.Name "hydra_mutual_b_"
          innerBundleVar = Core.TermVariable bundleInner
          outerBundleVar = Core.TermVariable bundleName
          appVar =
                  \fname -> \v -> Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name fname)),
                    Core.applicationArgument = v})
          nestedSecond = \k -> \v -> Logic.ifElse (Equality.equal k 0) v (appVar "pairs.second" (nestedSecond (Math.sub k 1) v))
          mkProj =
                  \bvar -> \i -> Logic.ifElse (Equality.equal i (Math.sub n 1)) (nestedSecond i bvar) (appVar "pairs.first" (nestedSecond i bvar))
          mkProjBindings =
                  \bvar -> Lists.map (\ib ->
                    let i = Pairs.first ib
                        b = Pairs.second ib
                    in Core.Binding {
                      Core.bindingName = (Core.bindingName b),
                      Core.bindingTerm = (mkProj bvar i),
                      Core.bindingType = (Core.bindingType b)}) (Lists.zip (Math.range 0 (Math.sub n 1)) grp)
          innerProjBindings = mkProjBindings innerBundleVar
          outerProjBindings = mkProjBindings outerBundleVar
          strippedBindings =
                  Lists.map (\b -> Core.Binding {
                    Core.bindingName = (Core.bindingName b),
                    Core.bindingTerm = (stripHydraFix (Core.bindingName b) (Core.bindingTerm b)),
                    Core.bindingType = (Core.bindingType b)}) grp
          mkPair =
                  \ts -> Maybes.fromMaybe (Core.TermVariable (Core.Name "tt")) (Maybes.map (\p -> Logic.ifElse (Equality.equal (Lists.length ts) 1) (Pairs.first p) (Core.TermPair (Pairs.first p, (mkPair (Pairs.second p))))) (Lists.uncons ts))
          pairExpr = mkPair (Lists.map (\b -> Core.bindingTerm b) strippedBindings)
          fixBody = rebuildLets innerProjBindings pairExpr
          fixTerm =
                  Core.TermApplication (Core.Application {
                    Core.applicationFunction = (Core.TermVariable (Core.Name "hydra_fix")),
                    Core.applicationArgument = (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = bundleInner,
                      Core.lambdaDomain = Nothing,
                      Core.lambdaBody = fixBody}))})
          bundleBinding =
                  Core.Binding {
                    Core.bindingName = bundleName,
                    Core.bindingTerm = fixTerm,
                    Core.bindingType = Nothing}
      in (Core.TermLet (Core.Let {
        Core.letBindings = [
          bundleBinding],
        Core.letBody = (rebuildLets outerProjBindings body)}))

-- | Erase lambda domain annotations referencing unbound type variables; recurse under new type binders
eraseUnboundTypeVarDomains :: S.Set String -> Core.Term -> Core.Term
eraseUnboundTypeVarDomains initialBound term0 =

      let eraseIfUnbound =
              \bound -> \mdom -> Maybes.maybe Nothing (\ty -> Logic.ifElse (hasUnboundTypeVar bound ty) Nothing (Just ty)) mdom
          f =
                  \recurse -> \bound -> \term -> case term of
                    Core.TermLambda v0 ->
                      let paramName = Core.unName (Core.lambdaParameter v0)
                          dom = Core.lambdaDomain v0
                          isTypeParam =
                                  Maybes.maybe False (\d -> case d of
                                    Core.TypeVariable v1 -> Equality.equal (Core.unName v1) "Type"
                                    _ -> False) dom
                          bound2 = Logic.ifElse (Logic.and isTypeParam (isTypeVarLike paramName)) (Sets.insert paramName bound) bound
                      in (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = (Core.lambdaParameter v0),
                        Core.lambdaDomain = (eraseIfUnbound bound dom),
                        Core.lambdaBody = (f recurse bound2 (Core.lambdaBody v0))}))
                    _ -> recurse bound term
      in (Rewriting.rewriteTermWithContext f initialBound term0)

-- | Extract the namespace (everything except the last dot-separated component) from a qualified Hydra name
extractQualifiedNamespace :: String -> String
extractQualifiedNamespace s =

      let parts = Strings.splitOn "." s
      in (Logic.ifElse (Equality.gte (Lists.length parts) 2) (Strings.intercalate "." (Maybes.fromMaybe [] (Lists.maybeInit parts))) s)

-- | Peel off leading forall binders, returning the list of parameter names and the inner body type
extractTypeParams :: Core.Type -> ([String], Core.Type)
extractTypeParams ty =
    case ty of
      Core.TypeForall v0 ->
        let param = Core.unName (Core.forallTypeParameter v0)
            rest = extractTypeParams (Core.forallTypeBody v0)
        in (Lists.cons param (Pairs.first rest), (Pairs.second rest))
      Core.TypeAnnotated v0 -> extractTypeParams (Core.annotatedTypeBody v0)
      _ -> ([], ty)

-- | Return True if the field type contains a function whose domain mentions a group member
fieldCausesPositivityIssue :: S.Set String -> Core.Type -> Bool
fieldCausesPositivityIssue groupNames fty =
    case fty of
      Core.TypeFunction v0 -> Logic.or (typeContainsGroupRef groupNames (Core.functionTypeDomain v0)) (fieldCausesPositivityIssue groupNames (Core.functionTypeCodomain v0))
      Core.TypeAnnotated v0 -> fieldCausesPositivityIssue groupNames (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> fieldCausesPositivityIssue groupNames (Core.forallTypeBody v0)
      Core.TypeWrap v0 -> fieldCausesPositivityIssue groupNames v0
      _ -> False

-- | Return True if any definition in the group has a record/union field whose type causes a positivity violation
hasPositivityIssue :: S.Set String -> [(t0, Core.Type)] -> Bool
hasPositivityIssue groupNames defs =
    Lists.foldl (\acc -> \nt -> Logic.or acc (
      let ty = Pairs.second nt
          extracted = extractTypeParams ty
          bodyTy = Pairs.second extracted
      in case bodyTy of
        Core.TypeRecord v0 -> Lists.foldl (\acc2 -> \f -> Logic.or acc2 (fieldCausesPositivityIssue groupNames (Core.fieldTypeType f))) False v0
        Core.TypeUnion v0 -> Lists.foldl (\acc2 -> \f -> Logic.or acc2 (fieldCausesPositivityIssue groupNames (Core.fieldTypeType f))) False v0
        _ -> False)) False defs

-- | Return True if the type mentions a t<digits> variable not present in the given set
hasUnboundTypeVar :: S.Set String -> Core.Type -> Bool
hasUnboundTypeVar bound ty =
    case ty of
      Core.TypeAnnotated v0 -> hasUnboundTypeVar bound (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Logic.or (hasUnboundTypeVar bound (Core.applicationTypeFunction v0)) (hasUnboundTypeVar bound (Core.applicationTypeArgument v0))
      Core.TypeFunction v0 -> Logic.or (hasUnboundTypeVar bound (Core.functionTypeDomain v0)) (hasUnboundTypeVar bound (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> hasUnboundTypeVar bound v0
      Core.TypeMap v0 -> Logic.or (hasUnboundTypeVar bound (Core.mapTypeKeys v0)) (hasUnboundTypeVar bound (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> hasUnboundTypeVar bound v0
      Core.TypePair v0 -> Logic.or (hasUnboundTypeVar bound (Core.pairTypeFirst v0)) (hasUnboundTypeVar bound (Core.pairTypeSecond v0))
      Core.TypeSet v0 -> hasUnboundTypeVar bound v0
      Core.TypeVariable v0 ->
        let nm = Core.unName v0
        in (Logic.and (isTypeVarLike nm) (Logic.not (Sets.member nm bound)))
      _ -> False

-- | Return True if a Term (possibly under TermAnnotated wrappers) is a TermTypeLambda
isTypeLambdaTerm :: Core.Term -> Bool
isTypeLambdaTerm tm =
    case tm of
      Core.TermAnnotated v0 -> isTypeLambdaTerm (Core.annotatedTermBody v0)
      Core.TermTypeLambda _ -> True
      _ -> False

-- | Return True if the string is of the form `t<digits>` with at least one digit
isTypeVarLike :: String -> Bool
isTypeVarLike s =

      let chars = Strings.toList s
      in (Maybes.fromMaybe False (Maybes.map (\p ->
        let firstCh = Pairs.first p
            rest = Pairs.second p
        in (Logic.ifElse (Logic.not (Equality.equal firstCh 116)) False (Logic.and (Logic.not (Lists.null rest)) (Lists.foldl (\acc -> \c -> Logic.and acc (Logic.and (Equality.gte c 48) (Equality.lte c 57))) True rest)))) (Lists.uncons chars)))

-- | Return the last dot-separated segment of a qualified Hydra name, sanitised via `sanitize`
localName :: String -> String
localName s =

      let parts = Strings.splitOn "." s
          raw = Maybes.fromMaybe s (Lists.maybeLast parts)
      in (sanitize raw)

-- | Return the last dot-separated segment of a qualified Hydra name, unsanitized
localNameRaw :: String -> String
localNameRaw s =

      let parts = Strings.splitOn "." s
      in (Maybes.fromMaybe s (Lists.maybeLast parts))

-- | Return the deduplicated list of dependency namespace strings for a Module, excluding its own namespace
moduleDependencies :: Packaging.Module -> [String]
moduleDependencies m =

      let typeDeps = Lists.map (\ns -> Packaging.unNamespace ns) (Packaging.moduleTypeDependencies m)
          termDeps = Lists.map (\ns -> Packaging.unNamespace ns) (Packaging.moduleTermDependencies m)
          ownNs = Packaging.unNamespace (Packaging.moduleNamespace m)
          allDeps = Lists.concat2 typeDeps termDeps
          filtered = Lists.filter (\s -> Logic.not (Equality.equal s ownNs)) allDeps
      in (Lists.nub filtered)

-- | Rewrite inner TermTypeLambda nodes and type applications so that polymorphic helpers work under Coq's erasure-based encoding
normalizeInnerTypeLambdas :: Core.Term -> Core.Term
normalizeInnerTypeLambdas term =

      let stripTypeLambdas =
              \tm -> case tm of
                Core.TermTypeLambda v0 ->
                  let rest = stripTypeLambdas (Core.typeLambdaBody v0)
                  in (Lists.cons (Core.unName (Core.typeLambdaParameter v0)) (Pairs.first rest), (Pairs.second rest))
                _ -> ([], tm)
          rebuildTypeLambdas =
                  \params -> \body -> Lists.foldr (\p -> \acc -> Core.TermTypeLambda (Core.TypeLambda {
                    Core.typeLambdaParameter = (Core.Name p),
                    Core.typeLambdaBody = acc})) body params
          f =
                  \recurse -> \polyNames -> \tm -> case tm of
                    Core.TermLet v0 ->
                      let newPoly =
                              Sets.fromList (Maybes.cat (Lists.map (\b -> Logic.ifElse (isTypeLambdaTerm (Core.bindingTerm b)) (Just (Core.unName (Core.bindingName b))) Nothing) (Core.letBindings v0)))
                          polyNames2 = Sets.union polyNames newPoly
                      in (Core.TermLet (Core.Let {
                        Core.letBindings = (Lists.map (\b -> Core.Binding {
                          Core.bindingName = (Core.bindingName b),
                          Core.bindingTerm = (f recurse polyNames2 (Core.bindingTerm b)),
                          Core.bindingType = (Logic.ifElse (isTypeLambdaTerm (Core.bindingTerm b)) Nothing (Core.bindingType b))}) (Core.letBindings v0)),
                        Core.letBody = (f recurse polyNames2 (Core.letBody v0))}))
                    Core.TermTypeLambda v0 -> f recurse polyNames (Core.TermLambda (Core.Lambda {
                      Core.lambdaParameter = (Core.typeLambdaParameter v0),
                      Core.lambdaDomain = (Just (Core.TypeVariable (Core.Name "Type"))),
                      Core.lambdaBody = (Core.typeLambdaBody v0)}))
                    Core.TermTypeApplication v0 ->
                      let body = Core.typeApplicationTermBody v0
                          ttype = Core.typeApplicationTermType v0
                      in (Logic.ifElse (targetsPolyName polyNames body) (f recurse polyNames (Core.TermApplication (Core.Application {
                        Core.applicationFunction = body,
                        Core.applicationArgument = (typeToTerm ttype)}))) (f recurse polyNames body))
                    _ -> recurse polyNames tm
          stripped = stripTypeLambdas term
          outerParams = Pairs.first stripped
          body0 = Pairs.second stripped
      in (Logic.ifElse (Lists.null outerParams) term (rebuildTypeLambdas outerParams (Rewriting.rewriteTermWithContext f Sets.empty body0)))

-- | Sort bindings into SCC groups using free-var analysis and local-name matching
processLetSCCs :: [Core.Binding] -> [[Core.Binding]]
processLetSCCs bindings =

      let getName = \b -> Core.unName (Core.bindingName b)
          names = Sets.fromList (Lists.map getName bindings)
          localNames = Sets.fromList (Lists.map (\b -> localNameRaw (getName b)) bindings)
          allNames = Sets.union names localNames
          depVars =
                  \b ->
                    let varsName = Variables.freeVariablesInTerm (Core.bindingTerm b)
                        vars = Sets.fromList (Lists.map (\n -> Core.unName n) (Sets.toList varsName))
                        localVars = Sets.fromList (Lists.map (\v -> localNameRaw v) (Sets.toList vars))
                    in (Sets.toList (Sets.intersection allNames (Sets.union vars localVars)))
      in (Sorting.topologicalSortNodes getName depVars bindings)

-- | Wrap a Hydra Name as a singleton set of its raw string, iff it is a qualified (hydra.*) reference
qualifiedFromName :: Core.Name -> S.Set String
qualifiedFromName n =

      let raw = Core.unName n
          parts = Strings.splitOn "." raw
      in (Logic.ifElse (Logic.and (Equality.gte (Lists.length parts) 2) (Equality.equal (Maybes.fromMaybe "" (Lists.maybeHead parts)) "hydra")) (Sets.singleton raw) Sets.empty)

-- | Build a chain of single-binding TermLet wrappers around the given body
rebuildLets :: [Core.Binding] -> Core.Term -> Core.Term
rebuildLets bindings body =
    Lists.foldr (\b -> \acc -> Core.TermLet (Core.Let {
      Core.letBindings = [
        b],
      Core.letBody = acc})) body bindings

-- | Rebuild a chain of TermLet/hydra_fix wrappers from SCC-sorted binding groups
rebuildMutualLets :: [[Core.Binding]] -> Core.Term -> Core.Term
rebuildMutualLets groups body =
    Lists.foldr (\grp -> \acc -> Logic.ifElse (Equality.equal (Lists.length grp) 1) (Core.TermLet (Core.Let {
      Core.letBindings = grp,
      Core.letBody = acc})) (encodeMutualLetGroup grp acc)) body groups

-- | Topologically reorder let bindings and pair-encode mutually recursive groups
reorderLetBindings :: Core.Term -> Core.Term
reorderLetBindings term0 =

      let f =
              \recurse -> \tm -> case tm of
                Core.TermLet _ ->
                  let flat = collectLetBindings tm
                      allBindings = Pairs.first flat
                      innerBody = Pairs.second flat
                      groups = processLetSCCs allBindings
                      groups2 =
                              Lists.map (\grp -> Lists.map (\b -> Core.Binding {
                                Core.bindingName = (Core.bindingName b),
                                Core.bindingTerm = (reorderLetBindings (Core.bindingTerm b)),
                                Core.bindingType = (Core.bindingType b)}) grp) groups
                  in (rebuildMutualLets groups2 (reorderLetBindings innerBody))
                _ -> recurse tm
      in (Rewriting.rewriteTerm f term0)

-- | Replace field names in TermProject nodes using the given (typeName, rawFieldName) -> prefixedName map
rewriteTermFields :: M.Map (String, String) String -> Core.Term -> Core.Term
rewriteTermFields fm term0 =

      let rewrite =
              \recurse -> \term -> case term of
                Core.TermProject v0 ->
                  let tname = Core.unName (Core.projectionTypeName v0)
                      rawFn = localNameRaw (Core.unName (Core.projectionField v0))
                      key = (tname, rawFn)
                      newFname = Maybes.fromMaybe (Core.projectionField v0) (Maybes.map (\s -> Core.Name s) (Maps.lookup key fm))
                  in (Core.TermProject (Core.Projection {
                    Core.projectionTypeName = (Core.projectionTypeName v0),
                    Core.projectionField = newFname}))
                _ -> recurse term
      in (Rewriting.rewriteTerm rewrite term0)

-- | Escape a stripped local name against Coq's stripped reserved-words set
sanitize :: String -> String
sanitize s = Formatting.escapeWithUnderscore Language.coqStrippedReservedWords s

-- | Rewrite a Type, replacing offending record/union fields with TypeUnit and restoring forall binders
sanitizePositivity :: S.Set String -> Core.Type -> Core.Type
sanitizePositivity groupNames ty =

      let extracted = extractTypeParams ty
          params = Pairs.first extracted
          bodyTy = Pairs.second extracted
          sanitizeField =
                  \f -> Logic.ifElse (fieldCausesPositivityIssue groupNames (Core.fieldTypeType f)) (Core.FieldType {
                    Core.fieldTypeName = (Core.fieldTypeName f),
                    Core.fieldTypeType = Core.TypeUnit}) f
          sanitized =
                  case bodyTy of
                    Core.TypeRecord v0 -> Core.TypeRecord (Lists.map sanitizeField v0)
                    Core.TypeUnion v0 -> Core.TypeUnion (Lists.map sanitizeField v0)
                    _ -> bodyTy
      in (Lists.foldr (\p -> \t -> Core.TypeForall (Core.ForallType {
        Core.forallTypeParameter = (Core.Name p),
        Core.forallTypeBody = t})) sanitized params)

-- | Group term definitions into SCC components with a cyclic/acyclic flag
sortTermDefsSCC :: [(String, Core.Term)] -> [(Bool, [(String, Core.Term)])]
sortTermDefsSCC defs =

      let localNames = Sets.fromList (Lists.map (\d -> Pairs.first d) defs)
          depsOf = \d -> Sets.toList (termRefs localNames (Pairs.second d))
          comps = Sorting.topologicalSortNodes (\d -> Pairs.first d) depsOf defs
      in (Lists.map (\grp -> Logic.ifElse (Equality.gte (Lists.length grp) 2) (True, grp) (Maybes.fromMaybe (False, grp) (Maybes.map (\d ->
        let name = Pairs.first d
            deps = termRefs localNames (Pairs.second d)
        in (Sets.member name deps, grp)) (Lists.maybeHead grp)))) comps)

-- | Group type definitions into SCC components with a cyclic/acyclic flag
sortTypeDefsSCC :: [(String, Core.Type)] -> [(Bool, [(String, Core.Type)])]
sortTypeDefsSCC defs =

      let localNames = Sets.fromList (Lists.map (\d -> Pairs.first d) defs)
          depsOf = \d -> Sets.toList (typeRefs localNames (Pairs.second d))
          comps = Sorting.topologicalSortNodes (\d -> Pairs.first d) depsOf defs
      in (Lists.map (\grp -> Logic.ifElse (Equality.gte (Lists.length grp) 2) (True, grp) (Maybes.fromMaybe (False, grp) (Maybes.map (\d ->
        let name = Pairs.first d
            deps = typeRefs localNames (Pairs.second d)
        in (Sets.member name deps, grp)) (Lists.maybeHead grp)))) comps)

-- | Strip an outer hydra_fix lambda wrapper, substituting the inner self-reference for the binding name
stripHydraFix :: Core.Name -> Core.Term -> Core.Term
stripHydraFix bName tm =
    case tm of
      Core.TermApplication v0 ->
        let fn = Core.applicationFunction v0
            arg = Core.applicationArgument v0
        in case fn of
          Core.TermVariable v1 -> Logic.ifElse (Equality.equal (Core.unName v1) "hydra_fix") (case arg of
            Core.TermLambda v2 -> Variables.substituteVariable (Core.lambdaParameter v2) bName (Core.lambdaBody v2)
            _ -> tm) tm
          _ -> tm
      _ -> tm

-- | Return True if the innermost target of a (possibly nested) type application is a poly-converted local name
targetsPolyName :: S.Set String -> Core.Term -> Bool
targetsPolyName polyNames tm =
    case tm of
      Core.TermVariable v0 -> Sets.member (Core.unName v0) polyNames
      Core.TermTypeApplication v0 -> targetsPolyName polyNames (Core.typeApplicationTermBody v0)
      Core.TermAnnotated v0 -> targetsPolyName polyNames (Core.annotatedTermBody v0)
      _ -> False

-- | Walk a Term and collect the local names it references, intersected with the given locally-defined names
termRefs :: S.Set String -> Core.Term -> S.Set String
termRefs locals tm =
    case tm of
      Core.TermAnnotated v0 -> termRefs locals (Core.annotatedTermBody v0)
      Core.TermApplication v0 -> Sets.union (termRefs locals (Core.applicationFunction v0)) (termRefs locals (Core.applicationArgument v0))
      Core.TermCases v0 -> Sets.union (Sets.unions (Lists.map (\f -> termRefs locals (Core.fieldTerm f)) (Core.caseStatementCases v0))) (Maybes.maybe Sets.empty (\d -> termRefs locals d) (Core.caseStatementDefault v0))
      Core.TermEither v0 -> Eithers.either (\l -> termRefs locals l) (\r -> termRefs locals r) v0
      Core.TermInject v0 -> termRefs locals (Core.fieldTerm (Core.injectionField v0))
      Core.TermLambda v0 -> termRefs locals (Core.lambdaBody v0)
      Core.TermLet v0 -> Sets.union (Sets.unions (Lists.map (\b -> termRefs locals (Core.bindingTerm b)) (Core.letBindings v0))) (termRefs locals (Core.letBody v0))
      Core.TermList v0 -> Sets.unions (Lists.map (\el -> termRefs locals el) v0)
      Core.TermMaybe v0 -> Maybes.maybe Sets.empty (\el -> termRefs locals el) v0
      Core.TermPair v0 -> Sets.union (termRefs locals (Pairs.first v0)) (termRefs locals (Pairs.second v0))
      Core.TermRecord v0 -> Sets.unions (Lists.map (\f -> termRefs locals (Core.fieldTerm f)) (Core.recordFields v0))
      Core.TermTypeApplication v0 -> termRefs locals (Core.typeApplicationTermBody v0)
      Core.TermTypeLambda v0 -> termRefs locals (Core.typeLambdaBody v0)
      Core.TermVariable v0 ->
        let local = localName (Core.unName v0)
        in (Logic.ifElse (Sets.member local locals) (Sets.singleton local) Sets.empty)
      Core.TermWrap v0 -> termRefs locals (Core.wrappedTermBody v0)
      _ -> Sets.empty

-- | Return True if the Type mentions any type variable whose local name is in the given set
typeContainsGroupRef :: S.Set String -> Core.Type -> Bool
typeContainsGroupRef groupNames ty =
    case ty of
      Core.TypeAnnotated v0 -> typeContainsGroupRef groupNames (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Logic.or (typeContainsGroupRef groupNames (Core.applicationTypeFunction v0)) (typeContainsGroupRef groupNames (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Logic.or (typeContainsGroupRef groupNames (Core.eitherTypeLeft v0)) (typeContainsGroupRef groupNames (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> typeContainsGroupRef groupNames (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Logic.or (typeContainsGroupRef groupNames (Core.functionTypeDomain v0)) (typeContainsGroupRef groupNames (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> typeContainsGroupRef groupNames v0
      Core.TypeMap v0 -> Logic.or (typeContainsGroupRef groupNames (Core.mapTypeKeys v0)) (typeContainsGroupRef groupNames (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> typeContainsGroupRef groupNames v0
      Core.TypePair v0 -> Logic.or (typeContainsGroupRef groupNames (Core.pairTypeFirst v0)) (typeContainsGroupRef groupNames (Core.pairTypeSecond v0))
      Core.TypeSet v0 -> typeContainsGroupRef groupNames v0
      Core.TypeVariable v0 -> Sets.member (localName (Core.unName v0)) groupNames
      Core.TypeWrap v0 -> typeContainsGroupRef groupNames v0
      _ -> False

-- | Walk a Type and collect the local names it references, intersected with the given locally-defined names
typeRefs :: S.Set String -> Core.Type -> S.Set String
typeRefs locals ty =
    case ty of
      Core.TypeAnnotated v0 -> typeRefs locals (Core.annotatedTypeBody v0)
      Core.TypeApplication v0 -> Sets.union (typeRefs locals (Core.applicationTypeFunction v0)) (typeRefs locals (Core.applicationTypeArgument v0))
      Core.TypeEither v0 -> Sets.union (typeRefs locals (Core.eitherTypeLeft v0)) (typeRefs locals (Core.eitherTypeRight v0))
      Core.TypeForall v0 -> typeRefs locals (Core.forallTypeBody v0)
      Core.TypeFunction v0 -> Sets.union (typeRefs locals (Core.functionTypeDomain v0)) (typeRefs locals (Core.functionTypeCodomain v0))
      Core.TypeList v0 -> typeRefs locals v0
      Core.TypeMap v0 -> Sets.union (typeRefs locals (Core.mapTypeKeys v0)) (typeRefs locals (Core.mapTypeValues v0))
      Core.TypeMaybe v0 -> typeRefs locals v0
      Core.TypePair v0 -> Sets.union (typeRefs locals (Core.pairTypeFirst v0)) (typeRefs locals (Core.pairTypeSecond v0))
      Core.TypeRecord v0 -> Sets.unions (Lists.map (\f -> typeRefs locals (Core.fieldTypeType f)) v0)
      Core.TypeSet v0 -> typeRefs locals v0
      Core.TypeUnion v0 -> Sets.unions (Lists.map (\f -> typeRefs locals (Core.fieldTypeType f)) v0)
      Core.TypeVariable v0 ->
        let local = localName (Core.unName v0)
        in (Logic.ifElse (Sets.member local locals) (Sets.singleton local) Sets.empty)
      Core.TypeWrap v0 -> typeRefs locals v0
      _ -> Sets.empty

-- | Convert a Hydra Type to a placeholder Term for use as an explicit Coq type argument. Coq-builtin type constructors are marked with a `Coq.` prefix so the encoder can emit them raw without going through sanitizeVar, which would clash with user-level lambda parameters of the same name (e.g. `list` -> `list_`).
typeToTerm :: Core.Type -> Core.Term
typeToTerm ty =
    case ty of
      Core.TypeVariable v0 -> Core.TermVariable v0
      Core.TypeList v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.list")),
        Core.applicationArgument = (typeToTerm v0)})
      Core.TypeMaybe v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.option")),
        Core.applicationArgument = (typeToTerm v0)})
      Core.TypeSet v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.list")),
        Core.applicationArgument = (typeToTerm v0)})
      Core.TypeApplication v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (typeToTerm (Core.applicationTypeFunction v0)),
        Core.applicationArgument = (typeToTerm (Core.applicationTypeArgument v0))})
      Core.TypeFunction _ -> Core.TermVariable (Core.Name "Coq.unit")
      Core.TypePair v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.prod")),
          Core.applicationArgument = (typeToTerm (Core.pairTypeFirst v0))})),
        Core.applicationArgument = (typeToTerm (Core.pairTypeSecond v0))})
      Core.TypeMap v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.list")),
        Core.applicationArgument = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.prod")),
          Core.applicationArgument = (typeToTerm (Core.mapTypeKeys v0))}))})
      Core.TypeUnit -> Core.TermVariable (Core.Name "Coq.unit")
      Core.TypeLiteral _ -> Core.TermVariable (Core.Name "Coq.unit")
      Core.TypeEither v0 -> Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermApplication (Core.Application {
          Core.applicationFunction = (Core.TermVariable (Core.Name "Coq.sum")),
          Core.applicationArgument = (typeToTerm (Core.eitherTypeLeft v0))})),
        Core.applicationArgument = (typeToTerm (Core.eitherTypeRight v0))})
      Core.TypeRecord _ -> Core.TermVariable (Core.Name "Coq.unit")
      Core.TypeUnion _ -> Core.TermVariable (Core.Name "Coq.unit")
      Core.TypeWrap v0 -> typeToTerm v0
      Core.TypeAnnotated v0 -> typeToTerm (Core.annotatedTypeBody v0)
      Core.TypeForall v0 -> typeToTerm (Core.forallTypeBody v0)
      Core.TypeVoid -> Core.TermVariable (Core.Name "Coq.Empty_set")
      _ -> Core.TermVariable (Core.Name "Coq.unit")
