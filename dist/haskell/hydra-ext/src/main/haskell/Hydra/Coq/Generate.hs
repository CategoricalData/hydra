-- Note: this is an automatically generated file. Do not edit.

-- | Coq code generation driver — pre-passes, sentence producers, and per-module pipeline

module Hydra.Coq.Generate where

import qualified Hydra.Coq.Coder as Coder
import qualified Hydra.Coq.Environment as Environment
import qualified Hydra.Coq.Serde as Serde
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Coq.Utils as Utils
import qualified Hydra.Core as Core
import qualified Hydra.Formatting as Formatting
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Render an axiom-only Coq module: imports + dependency imports + Axiom declarations
buildAxiomOnlyContent :: Environment.CoqEnvironment -> String -> t0 -> [(String, t1)] -> [(String, (t2, (t3, (Maybe Core.Type))))] -> Packaging.Module -> String
buildAxiomOnlyContent env desc nsStr typeDefs termDefs mod_ =

      let typeOfType = Core.TypeVariable (Core.Name "Type")
          typeAxioms = Lists.map (\nt -> Coder.encodeAxiomDefinitionPair env (Pairs.first nt, typeOfType)) typeDefs
          termAxioms =
                  Maybes.cat (Lists.map (\td ->
                    let name = Pairs.first td
                        mty = Pairs.second (Pairs.second (Pairs.second td))
                    in (Maybes.maybe Nothing (\schemeTy ->
                      let ep = Utils.extractTypeParams schemeTy
                          ty = Pairs.second ep
                      in (Just (Coder.encodeAxiomDefinitionPair env (name, ty)))) mty)) termDefs)
          deps = Utils.moduleDependencies mod_
          depSentences = dependencyImports deps
          allSentences = Lists.cons Coder.standardImports (Lists.concat2 depSentences (Lists.concat2 typeAxioms termAxioms))
          doc_ = Syntax.Document {
                Syntax.documentSentences = allSentences}
          body = Serialization.printExpr (Serialization.parenthesize (Serde.documentToExpr doc_))
      in (Strings.cat [
        desc,
        body,
        "\n"])

-- | Assemble the full (non-axiom) Coq source for a module
buildFullModule :: Ord t1 => (Environment.CoqEnvironment -> M.Map (String, String) String -> t0 -> String -> t1 -> String -> [(String, Core.Type)] -> [(String, (Core.Term, ([Core.Name], (Maybe Core.Type))))] -> M.Map t1 String)
buildFullModule env fieldMap mod_ nsStr path desc typeDefs termDefs =

      let termDefsForSort = Lists.map (\td -> (Pairs.first td, (Pairs.first (Pairs.second td)))) termDefs
          termDefMap = Maps.fromList (Lists.map (\td -> (Pairs.first td, (Pairs.second td))) termDefs)
          termGroups = Utils.sortTermDefsSCC termDefsForSort
          termGroups2 =
                  Lists.map (\cg ->
                    let cyc = Pairs.first cg
                        grp = Pairs.second cg
                        enriched =
                                Maybes.cat (Lists.map (\nt ->
                                  let nm = Pairs.first nt
                                      t = Pairs.second nt
                                  in (Maybes.map (\rec ->
                                    let body2 = Utils.normalizeInnerTypeLambdas (Utils.rewriteTermFields fieldMap t)
                                        rest = Pairs.second rec
                                        vs = Pairs.first rest
                                        mty = Pairs.second rest
                                    in (nm, (body2, (vs, mty)))) (Maps.lookup nm termDefMap))) grp)
                    in (cyc, enriched)) termGroups
          typeGroups = Utils.sortTypeDefsSCC typeDefs
          typeSentences = Lists.concat (Lists.map (\g -> generateTypeGroup env g) typeGroups)
          termRenderedParts =
                  Lists.concat (Lists.map (\cg ->
                    let cyc = Pairs.first cg
                        grp = Pairs.second cg
                    in (Logic.ifElse cyc [
                      encodeMutualGroupText env grp] (Lists.map (\td ->
                      let sentences = encodeTermGroupSingleton env td
                          nm = Pairs.first td
                          body = Pairs.first (Pairs.second td)
                          tv = Pairs.first (Pairs.second (Pairs.second td))
                          schemeVarNames = Sets.fromList (Lists.map (\n -> Core.unName n) tv)
                          body2 = Utils.reorderLetBindings (Utils.eraseUnboundTypeVarDomains schemeVarNames body)
                          binders = mkTypeBinders body2 tv
                          allTypeVarNames = Pairs.first binders
                          rendered = renderSentences sentences
                          argsLine =
                                  Logic.ifElse (Lists.null allTypeVarNames) "" (Strings.cat [
                                    "\nArguments ",
                                    nm,
                                    " ",
                                    (Strings.intercalate " " (Lists.map (\v -> Strings.cat [
                                      "{",
                                      v,
                                      "}"]) allTypeVarNames)),
                                    "."])
                      in (Strings.cat [
                        rendered,
                        argsLine,
                        "\n"])) grp))) termGroups2)
          allQualifiedNamesFromTypes = Sets.unions (Lists.map (\nt -> Utils.collectQualifiedNamesInType (Pairs.second nt)) typeDefs)
          allQualifiedNamesFromTerms =
                  Sets.unions (Lists.map (\td -> Utils.collectQualifiedNamesInTerm (Pairs.first (Pairs.second td))) termDefs)
          allQualifiedNamesFromTermTypes =
                  Sets.unions (Maybes.cat (Lists.map (\td ->
                    let mty = Pairs.second (Pairs.second (Pairs.second td))
                    in (Maybes.map (\ty ->
                      let ep = Utils.extractTypeParams ty
                          bodyTy = Pairs.second ep
                      in (Utils.collectQualifiedNamesInType bodyTy)) mty)) termDefs))
          allQualifiedNames =
                  Sets.union allQualifiedNamesFromTypes (Sets.union allQualifiedNamesFromTerms allQualifiedNamesFromTermTypes)
          nsSet = Sets.fromList (Lists.map (\q -> Utils.extractQualifiedNamespace q) (Sets.toList allQualifiedNames))
          strStartsWith =
                  \pref -> \s -> Logic.and (Equality.gte (Strings.length s) (Strings.length pref)) (Equality.equal (Strings.fromList (Lists.take (Strings.length pref) (Strings.toList s))) pref)
          hasStrictSuffix =
                  \nsC -> \otherList -> Maybes.isJust (Lists.find (\other -> Logic.and (Logic.not (Equality.equal other nsC)) (strStartsWith (Strings.cat [
                    nsC,
                    "."]) other)) otherList)
          referencedNs =
                  Lists.nub (Lists.filter (\nsC -> Logic.and (Logic.not (Equality.equal nsC nsStr)) (Logic.not (hasStrictSuffix nsC (Sets.toList nsSet)))) (Sets.toList nsSet))
          depSentences = dependencyImports referencedNs
          importText = renderRequireImports depSentences
          typeSentencesText = renderSentences typeSentences
          allTermText = Strings.cat termRenderedParts
          typeArgsDecls = generateArgumentsDecls typeDefs
          content =
                  Strings.cat [
                    desc,
                    importText,
                    "\n",
                    typeSentencesText,
                    "\n",
                    allTermText,
                    typeArgsDecls,
                    "\n"]
      in (Maps.fromList [
        (path, content)])

-- | Emit a Require Import sentence for the given dependency namespaces; empty list yields no sentence
dependencyImports :: [String] -> [Syntax.Sentence]
dependencyImports deps =
    Logic.ifElse (Lists.null deps) [] [
      Syntax.Sentence {
        Syntax.sentenceComment = (Just (Syntax.Comment "Module dependencies")),
        Syntax.sentenceContent = (Syntax.SentenceContentRequireImport (Syntax.RequireImport {
          Syntax.requireImportFrom = Nothing,
          Syntax.requireImportRequire = True,
          Syntax.requireImportQualification = (Just Syntax.ImportQualificationImport),
          Syntax.requireImportModules = (Lists.map (\d -> Coder.coqQualid d) deps)}))}]

-- | Render a mutually recursive term group as a hydra_fix bundle plus projection Definitions
encodeMutualGroupText :: Environment.CoqEnvironment -> [(String, (Core.Term, ([Core.Name], (Maybe Core.Type))))] -> String
encodeMutualGroupText env group =

      let groupSchemeVars =
              Sets.fromList (Lists.concat (Lists.map (\td ->
                let rest1 = Pairs.second td
                    rest2 = Pairs.second rest1
                    tv = Pairs.first rest2
                in (Lists.map (\n -> Core.unName n) tv)) group))
          funInfos =
                  Lists.map (\td ->
                    let name = Pairs.first td
                        rest1 = Pairs.second td
                        body = Pairs.first rest1
                        rest2 = Pairs.second rest1
                        mType = Pairs.second rest2
                        body2 = Utils.reorderLetBindings (Utils.eraseUnboundTypeVarDomains groupSchemeVars body)
                        coqBody = Coder.encodeTerm env body2
                        bodyText = Serialization.printExpr (Serialization.parenthesize (Serde.termToExpr coqBody))
                        typeText =
                                Maybes.maybe "_" (\ty ->
                                  let ep = Utils.extractTypeParams ty
                                      bodyTy = Pairs.second ep
                                  in (Serialization.printExpr (Serialization.parenthesize (Serde.typeToExpr (Syntax.Type (Coder.encodeType env bodyTy)))))) mType
                    in (name, (typeText, bodyText))) group
          allTypeVarNames =
                  Lists.nub (Lists.concat (Lists.map (\td ->
                    let rest1 = Pairs.second td
                        b = Pairs.first rest1
                        rest2 = Pairs.second rest1
                        tv = Pairs.first rest2
                        binders = mkTypeBinders b tv
                    in (Pairs.first binders)) group))
          names = Lists.map (\fi -> Pairs.first fi) funInfos
          bundleName =
                  Strings.cat [
                    Strings.intercalate "_" (Lists.take 2 names),
                    "_bundle"]
          n = Lists.length funInfos
          types = Lists.map (\fi -> Pairs.first (Pairs.second fi)) funInfos
          productType = makeProdType types
          projExprs = makeProjectionExprs n "bundle_"
          letParts =
                  Lists.map (\np ->
                    let nm = Pairs.first (Pairs.first np)
                        proj = Pairs.second np
                    in (Strings.cat [
                      nm,
                      " := ",
                      proj])) (Lists.zip funInfos projExprs)
          letBlock =
                  Logic.ifElse (Lists.null letParts) "" (Strings.cat [
                    "let ",
                    (Strings.intercalate " in\n    let " letParts),
                    " in\n    "])
          bodies = Lists.map (\fi -> Pairs.second (Pairs.second fi)) funInfos
          prodVal = makeProdVal bodies
          typBindText =
                  Logic.ifElse (Lists.null allTypeVarNames) "" (Strings.cat [
                    " ",
                    (Strings.intercalate " " (Lists.map (\v -> Strings.cat [
                      "(",
                      v,
                      " : Type)"]) allTypeVarNames))])
          bundleArgsLine = implicitArgsLine bundleName allTypeVarNames
          bundleDef =
                  Strings.cat [
                    "Definition ",
                    bundleName,
                    typBindText,
                    " :=\n  hydra_fix (fun (bundle_ : ",
                    productType,
                    ") =>\n    ",
                    letBlock,
                    prodVal,
                    ").\n",
                    bundleArgsLine]
          indexed = Lists.zip (Math.range 0 (Math.sub n 1)) funInfos
          projDefs =
                  Strings.cat (Lists.map (\iFi ->
                    let i = Pairs.first iFi
                        fi = Pairs.second iFi
                        nm = Pairs.first fi
                        t = Pairs.first (Pairs.second fi)
                        projText0 = Maybes.fromMaybe "" (Maps.lookup i (Maps.fromList (Lists.zip (Math.range 0 (Math.sub n 1)) projExprs)))
                        projText = replaceBundle projText0 bundleName
                        argsDef = implicitArgsLine nm allTypeVarNames
                    in (Strings.cat [
                      "Definition ",
                      nm,
                      typBindText,
                      " : ",
                      t,
                      " :=\n  ",
                      projText,
                      ".\n",
                      argsDef])) indexed)
      in (Strings.cat [
        bundleDef,
        "\n",
        projDefs])

-- | Encode a non-cyclic term definition as a Coq Definition sentence
encodeTermGroupSingleton :: Environment.CoqEnvironment -> (String, (Core.Term, ([Core.Name], (Maybe Core.Type)))) -> [Syntax.Sentence]
encodeTermGroupSingleton env td =

      let name = Pairs.first td
          rest1 = Pairs.second td
          body = Pairs.first rest1
          rest2 = Pairs.second rest1
          typeVars = Pairs.first rest2
          mType = Pairs.second rest2
          schemeVarNames = Sets.fromList (Lists.map (\n -> Core.unName n) typeVars)
          body2 = Utils.reorderLetBindings (Utils.eraseUnboundTypeVarDomains schemeVarNames body)
          coqBody = Coder.encodeTerm env body2
          binders = mkTypeBinders body2 typeVars
          typeBinders = Pairs.second binders
          returnType =
                  Maybes.maybe Nothing (\ty ->
                    let ep = Utils.extractTypeParams ty
                        bodyTy = Pairs.second ep
                    in (Just (Syntax.Type (Coder.encodeType env bodyTy)))) mType
      in [
        Syntax.Sentence {
          Syntax.sentenceComment = Nothing,
          Syntax.sentenceContent = (Syntax.SentenceContentDefinition (Syntax.Definition {
            Syntax.definitionLocality = Nothing,
            Syntax.definitionName = (Coder.coqIdent name),
            Syntax.definitionBinders = typeBinders,
            Syntax.definitionType = returnType,
            Syntax.definitionBody = coqBody}))}]

-- | Produce Arguments {p} declarations for every parameterized type's constructor and field accessors
generateArgumentsDecls :: [(String, Core.Type)] -> String
generateArgumentsDecls typeDefs =

      let implicitAll =
              \params -> Strings.intercalate " " (Lists.map (\p -> Strings.cat [
                "{",
                p,
                "}"]) params)
          linesFor =
                  \triple ->
                    let name = Pairs.first triple
                        params = Pairs.first (Pairs.second triple)
                        bodyTy = Pairs.second (Pairs.second triple)
                        impAll = implicitAll params
                    in case bodyTy of
                      Core.TypeUnion v0 -> Lists.map (\ft -> Strings.cat [
                        "Arguments ",
                        name,
                        "_",
                        (Formatting.capitalize (Core.unName (Core.fieldTypeName ft))),
                        " ",
                        impAll,
                        "."]) v0
                      Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) [] (
                        let constrLine =
                                Strings.cat [
                                  "Arguments Build_",
                                  name,
                                  " ",
                                  impAll,
                                  "."]
                            fieldLines =
                                    Lists.map (\ft -> Strings.cat [
                                      "Arguments ",
                                      (Formatting.decapitalize name),
                                      "_",
                                      (Utils.sanitize (Utils.localName (Core.unName (Core.fieldTypeName ft)))),
                                      " ",
                                      impAll,
                                      "."]) v0
                        in (Lists.cons constrLine fieldLines))
                      _ -> []
          triples =
                  Maybes.cat (Lists.map (\nt ->
                    let name = Pairs.first nt
                        ty = Pairs.second nt
                        ep = Utils.extractTypeParams ty
                        params = Pairs.first ep
                        bodyTy = Pairs.second ep
                    in (Logic.ifElse (Lists.null params) Nothing (Just (name, (params, bodyTy))))) typeDefs)
          allLines = Lists.concat (Lists.map linesFor triples)
      in (Logic.ifElse (Lists.null allLines) "" (Strings.cat [
        "\n",
        (Strings.intercalate "\n" allLines),
        "\n"]))

-- | Emit Coq sentences for a type-definition SCC group, handling mutual recursion and positivity
generateTypeGroup :: Environment.CoqEnvironment -> (Bool, [(String, Core.Type)]) -> [Syntax.Sentence]
generateTypeGroup env group =

      let cyclic = Pairs.first group
          defs = Pairs.second group
      in (Logic.ifElse (Logic.and (Logic.not cyclic) (Equality.equal (Lists.length defs) 1)) (
        let d = Lists.head defs
        in (generateTypeSentence env (Pairs.first d) (Pairs.second d))) (
        let groupNames = Sets.fromList (Lists.map (\d -> Pairs.first d) defs)
            hasPositivity = Utils.hasPositivityIssue groupNames defs
            sanitizedGroup =
                    Logic.ifElse hasPositivity (Lists.map (\d -> (Pairs.first d, (Utils.sanitizePositivity groupNames (Pairs.second d)))) defs) defs
            bodies = Lists.concat (Lists.map (\d -> makeInductiveBody env (Pairs.first d) (Pairs.second d)) sanitizedGroup)
            accessors = Lists.concat (Lists.map (\d -> makeAccessorDefs d) sanitizedGroup)
            inductiveSent =
                    Logic.ifElse (Lists.null bodies) [] [
                      Syntax.Sentence {
                        Syntax.sentenceComment = Nothing,
                        Syntax.sentenceContent = (Syntax.SentenceContentInductive (Syntax.InductiveDefinition {
                          Syntax.inductiveDefinitionLocality = Nothing,
                          Syntax.inductiveDefinitionCoinductive = False,
                          Syntax.inductiveDefinitionBodies = bodies}))}]
        in (Lists.concat2 inductiveSent accessors)))

-- | Generate the Coq sentence(s) for a non-cyclic type definition
generateTypeSentence :: Environment.CoqEnvironment -> String -> Core.Type -> [Syntax.Sentence]
generateTypeSentence env name ty =

      let extracted = Utils.extractTypeParams ty
          params = Pairs.first extracted
          bodyTy = Pairs.second extracted
          paramBinders =
                  Lists.map (\p -> Syntax.BinderType (Syntax.TypeBinders {
                    Syntax.typeBindersNames = [
                      Coder.coqName p],
                    Syntax.typeBindersType = (Syntax.Type (Coder.coqTermQualid "Type"))})) params
          mkDef =
                  \n -> \binders -> \body -> Syntax.Sentence {
                    Syntax.sentenceComment = Nothing,
                    Syntax.sentenceContent = (Syntax.SentenceContentDefinition (Syntax.Definition {
                      Syntax.definitionLocality = Nothing,
                      Syntax.definitionName = (Coder.coqIdent n),
                      Syntax.definitionBinders = binders,
                      Syntax.definitionType = (Just (Syntax.Type (Coder.coqTermQualid "Type"))),
                      Syntax.definitionBody = body}))}
      in case bodyTy of
        Core.TypeUnion v0 ->
          let body =
                  Syntax.InductiveBody {
                    Syntax.inductiveBodyName = (Coder.coqIdent name),
                    Syntax.inductiveBodyBinders = paramBinders,
                    Syntax.inductiveBodyType = (Just (Syntax.Type (Coder.coqTermQualid "Type"))),
                    Syntax.inductiveBodyConstructors = (Lists.map (\ft -> makeConstructor env name params ft) v0)}
              indDef =
                      Syntax.InductiveDefinition {
                        Syntax.inductiveDefinitionLocality = Nothing,
                        Syntax.inductiveDefinitionCoinductive = False,
                        Syntax.inductiveDefinitionBodies = [
                          body]}
          in [
            Syntax.Sentence {
              Syntax.sentenceComment = Nothing,
              Syntax.sentenceContent = (Syntax.SentenceContentInductive indDef)}]
        Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) [
          mkDef name paramBinders (Coder.coqTermQualid "unit")] [
          Syntax.Sentence {
            Syntax.sentenceComment = Nothing,
            Syntax.sentenceContent = (Syntax.SentenceContentRecord (Syntax.RecordDefinition {
              Syntax.recordDefinitionLocality = Nothing,
              Syntax.recordDefinitionName = (Coder.coqIdent name),
              Syntax.recordDefinitionBinders = paramBinders,
              Syntax.recordDefinitionSort = (Just Syntax.SortType),
              Syntax.recordDefinitionBody = Syntax.RecordBody {
                Syntax.recordBodyConstructor = (Just (Coder.coqIdent (Strings.cat [
                  "Build_",
                  name]))),
                Syntax.recordBodyFields = (Lists.map (\ft ->
                  let fn = Utils.sanitize (Utils.localName (Core.unName (Core.fieldTypeName ft)))
                      prefixedFn =
                              Strings.cat [
                                Formatting.decapitalize name,
                                "_",
                                fn]
                      ftCoq = Coder.encodeType env (Core.fieldTypeType ft)
                  in Syntax.RecordField {
                    Syntax.recordFieldName = (Coder.coqIdent prefixedFn),
                    Syntax.recordFieldType = (Syntax.Type ftCoq)}) v0)}}))}]
        _ -> [
          mkDef name paramBinders (Coder.encodeType env bodyTy)]

-- | Collect local names that occur in more than one module's type or term definitions
globalAmbiguousNames :: [Packaging.Module] -> S.Set String
globalAmbiguousNames modules =

      let allNames =
              Lists.concat (Lists.map (\m ->
                let nsStr = Packaging.unNamespace (Packaging.moduleNamespace m)
                    fromDef =
                            \def_ -> case def_ of
                              Packaging.DefinitionType v0 -> Just (Utils.localName (Core.unName (Packaging.typeDefinitionName v0)), nsStr)
                              Packaging.DefinitionTerm v0 -> Just (Utils.localName (Core.unName (Packaging.termDefinitionName v0)), nsStr)
                              _ -> Nothing
                in (Maybes.cat (Lists.map fromDef (Packaging.moduleDefinitions m)))) modules)
          nameToNs =
                  Lists.foldl (\acc -> \np ->
                    let n = Pairs.first np
                        nsVal = Pairs.second np
                        existing = Maybes.fromMaybe Sets.empty (Maps.lookup n acc)
                    in (Maps.insert n (Sets.insert nsVal existing) acc)) Maps.empty allNames
      in (Sets.fromList (Maybes.cat (Lists.map (\entry -> Logic.ifElse (Equality.gte (Lists.length (Sets.toList (Pairs.second entry))) 2) (Just (Pairs.first entry)) Nothing) (Maps.toList nameToNs))))

-- | Collect all type definitions from every module and run buildConstructorCounts over them
globalConstructorCounts :: [Packaging.Module] -> M.Map String Int
globalConstructorCounts modules =

      let allTypeDefs =
              Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\def_ -> case def_ of
                Packaging.DefinitionType v0 -> Just (Utils.localName (Core.unName (Packaging.typeDefinitionName v0)), (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                _ -> Nothing) (Packaging.moduleDefinitions m))) modules)
      in (Utils.buildConstructorCounts allTypeDefs)

-- | Delegate to CoqUtils.buildFieldMapping across all supplied modules
globalFieldMapping :: [Packaging.Module] -> M.Map (String, String) String
globalFieldMapping modules = Utils.buildFieldMapping modules

-- | Collect sanitized accessor names by SCC-sorting every module's type defs and folding collectSanitizedAccessors
globalSanitizedAccessors :: [Packaging.Module] -> S.Set String
globalSanitizedAccessors modules =

      let allTypeGroups =
              Lists.concat (Lists.map (\m ->
                let typeDefs =
                        Maybes.cat (Lists.map (\def_ -> case def_ of
                          Packaging.DefinitionType v0 -> Just (Utils.localName (Core.unName (Packaging.typeDefinitionName v0)), (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                          _ -> Nothing) (Packaging.moduleDefinitions m))
                in (Utils.sortTypeDefsSCC typeDefs)) modules)
      in (Utils.collectSanitizedAccessors allTypeGroups)

-- | Emit an Arguments line marking every type parameter of a definition as implicit
implicitArgsLine :: String -> [String] -> String
implicitArgsLine name typeVarNames =
    Logic.ifElse (Lists.null typeVarNames) "" (Strings.cat [
      "Arguments ",
      name,
      " ",
      (Strings.intercalate " " (Lists.map (\v -> Strings.cat [
        "{",
        v,
        "}"]) typeVarNames)),
      ".\n"])

-- | Build one Definition per record field, pattern-matching on Build_T
makeAccessorDefs :: (String, Core.Type) -> [Syntax.Sentence]
makeAccessorDefs nt =

      let name = Pairs.first nt
          ty = Pairs.second nt
          extracted = Utils.extractTypeParams ty
          bodyTy = Pairs.second extracted
      in case bodyTy of
        Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) [] (
          let nFields = Lists.length v0
              fieldVars =
                      Lists.map (\i -> Strings.cat [
                        "f",
                        (Literals.showInt32 i)]) (Math.range 0 (Math.sub nFields 1))
              constrPat =
                      Syntax.Pattern10_Qualid {
                        Syntax.pattern10_QualidQualid = (Coder.coqQualid (Strings.cat [
                          "Build_",
                          name])),
                        Syntax.pattern10_QualidPatterns = (Lists.map (\v -> Syntax.Pattern1 {
                          Syntax.pattern1Pattern = (Syntax.Pattern0Qualid (Coder.coqQualid v)),
                          Syntax.pattern1Scope = Nothing}) fieldVars)}
              indexed = Lists.zip (Math.range 0 (Math.sub nFields 1)) v0
          in (Lists.map (\ift -> makeOneAccessor name constrPat fieldVars (Pairs.first ift) (Pairs.second ift)) indexed))
        _ -> []

-- | Build a Coq Constructor from a union field (prepended with the type name and capitalized field name)
makeConstructor :: Environment.CoqEnvironment -> String -> [String] -> Core.FieldType -> Syntax.Constructor
makeConstructor env typeName params ft =

      let fn = Core.unName (Core.fieldTypeName ft)
          constrName =
                  Strings.cat [
                    typeName,
                    "_",
                    (Formatting.capitalize fn)]
          fieldTy = Core.fieldTypeType ft
          argType = Coder.encodeType env fieldTy
          returnType = makeReturnType typeName params
      in Syntax.Constructor {
        Syntax.constructorName = (Coder.coqIdent constrName),
        Syntax.constructorBinders = [],
        Syntax.constructorType = (Just (Syntax.Type (Coder.coqArrow argType returnType)))}

-- | Build an Inductive body for a union or record type in a mutual group
makeInductiveBody :: Environment.CoqEnvironment -> String -> Core.Type -> [Syntax.InductiveBody]
makeInductiveBody env name ty =

      let extracted = Utils.extractTypeParams ty
          params = Pairs.first extracted
          bodyTy = Pairs.second extracted
          paramBinders =
                  Lists.map (\p -> Syntax.BinderType (Syntax.TypeBinders {
                    Syntax.typeBindersNames = [
                      Coder.coqName p],
                    Syntax.typeBindersType = (Syntax.Type (Coder.coqTermQualid "Type"))})) params
      in case bodyTy of
        Core.TypeUnion v0 -> [
          Syntax.InductiveBody {
            Syntax.inductiveBodyName = (Coder.coqIdent name),
            Syntax.inductiveBodyBinders = paramBinders,
            Syntax.inductiveBodyType = (Just (Syntax.Type (Coder.coqTermQualid "Type"))),
            Syntax.inductiveBodyConstructors = (Lists.map (\ft -> makeConstructor env name params ft) v0)}]
        Core.TypeRecord v0 -> Logic.ifElse (Lists.null v0) [
          Syntax.InductiveBody {
            Syntax.inductiveBodyName = (Coder.coqIdent name),
            Syntax.inductiveBodyBinders = paramBinders,
            Syntax.inductiveBodyType = (Just (Syntax.Type (Coder.coqTermQualid "Type"))),
            Syntax.inductiveBodyConstructors = [
              Syntax.Constructor {
                Syntax.constructorName = (Coder.coqIdent (Strings.cat [
                  "Build_",
                  name])),
                Syntax.constructorBinders = [],
                Syntax.constructorType = (Just (Syntax.Type (makeReturnType name params)))}]}] (
          let constrType =
                  Lists.foldr (\ft -> \acc -> Coder.coqArrow (Coder.encodeType env (Core.fieldTypeType ft)) acc) (makeReturnType name params) v0
          in [
            Syntax.InductiveBody {
              Syntax.inductiveBodyName = (Coder.coqIdent name),
              Syntax.inductiveBodyBinders = paramBinders,
              Syntax.inductiveBodyType = (Just (Syntax.Type (Coder.coqTermQualid "Type"))),
              Syntax.inductiveBodyConstructors = [
                Syntax.Constructor {
                  Syntax.constructorName = (Coder.coqIdent (Strings.cat [
                    "Build_",
                    name])),
                  Syntax.constructorBinders = [],
                  Syntax.constructorType = (Just (Syntax.Type constrType))}]}])
        _ -> []

-- | Emit a Definition for a record field accessor, keyed by the Build_T pattern
makeOneAccessor :: String -> Syntax.Pattern10_Qualid -> [String] -> Int -> Core.FieldType -> Syntax.Sentence
makeOneAccessor typeName constrPat fieldVars idx ft =

      let fn = Utils.sanitize (Utils.localName (Core.unName (Core.fieldTypeName ft)))
          prefixedFn =
                  Strings.cat [
                    Formatting.decapitalize typeName,
                    "_",
                    fn]
          returnExpr =
                  Coder.coqTermQualid (Maybes.fromMaybe "" (Maps.lookup idx (Maps.fromList (Lists.zip (Math.range 0 (Math.sub (Lists.length fieldVars) 1)) fieldVars))))
          matchExpr =
                  Syntax.TermTerm100 (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermTerm1 (Syntax.Term1Term0 (Syntax.Term0Match (Syntax.Match {
                    Syntax.matchCaseItems = [
                      Syntax.CaseItem {
                        Syntax.caseItemTerm = (Syntax.Term100Term10 (Syntax.Term10OneTerm (Syntax.OneTermExplicit (Syntax.QualidAnnotated {
                          Syntax.qualidAnnotatedQualid = (Coder.coqQualid "r_"),
                          Syntax.qualidAnnotatedUnivAnnot = Nothing})))),
                        Syntax.caseItemAs = Nothing,
                        Syntax.caseItemIn = Nothing}],
                    Syntax.matchReturn = Nothing,
                    Syntax.matchPipe = False,
                    Syntax.matchEquations = [
                      Syntax.Equation {
                        Syntax.equationPattern = [
                          [
                            Syntax.PatternPattern (Syntax.Pattern10Qualiid constrPat)]],
                        Syntax.equationTerm = returnExpr}]}))))))
      in Syntax.Sentence {
        Syntax.sentenceComment = Nothing,
        Syntax.sentenceContent = (Syntax.SentenceContentDefinition (Syntax.Definition {
          Syntax.definitionLocality = Nothing,
          Syntax.definitionName = (Coder.coqIdent prefixedFn),
          Syntax.definitionBinders = [
            Syntax.BinderType (Syntax.TypeBinders {
              Syntax.typeBindersNames = [
                Coder.coqName "r_"],
              Syntax.typeBindersType = (Syntax.Type (Coder.coqTermQualid typeName))})],
          Syntax.definitionType = Nothing,
          Syntax.definitionBody = matchExpr}))}

-- | Emit nested `prod (T1) (prod ...)` textual type expression
makeProdType :: [String] -> String
makeProdType ts =
    Logic.ifElse (Lists.null ts) "unit" (Logic.ifElse (Equality.equal (Lists.length ts) 1) (Lists.head ts) (Strings.cat [
      "prod (",
      (Lists.head ts),
      ") (",
      (makeProdType (Lists.tail ts)),
      ")"]))

-- | Emit a nested `(pair (b1) (...))` textual value expression
makeProdVal :: [String] -> String
makeProdVal bs =
    Logic.ifElse (Lists.null bs) "tt" (Logic.ifElse (Equality.equal (Lists.length bs) 1) (Lists.head bs) (Strings.cat [
      "(pair (",
      (Lists.head bs),
      ") (",
      (makeProdVal (Lists.tail bs)),
      "))"]))

-- | Emit the n projection expressions extracting each member of a nested pair bundle
makeProjectionExprs :: Int -> String -> [String]
makeProjectionExprs n bvar =

      let snds =
              \k -> \v -> Logic.ifElse (Equality.equal k 0) v (snds (Math.sub k 1) (Strings.cat [
                "(snd ",
                v,
                ")"]))
          mkProj =
                  \i -> \total -> \v -> Logic.ifElse (Equality.equal i 0) (Strings.cat [
                    "(fst ",
                    v,
                    ")"]) (Logic.ifElse (Equality.equal i (Math.sub total 1)) (snds i v) (Strings.cat [
                    "(fst ",
                    (snds i v),
                    ")"]))
      in (Logic.ifElse (Equality.lte n 0) [] (Logic.ifElse (Equality.equal n 1) [
        bvar] (Lists.map (\i -> mkProj i n bvar) (Math.range 0 (Math.sub n 1)))))

-- | Return-type Coq term: `TypeName` or `TypeName p1 p2 ...`
makeReturnType :: String -> [String] -> Syntax.Term
makeReturnType typeName params =
    Logic.ifElse (Lists.null params) (Coder.coqTermQualid typeName) (Coder.coqTermApp (Coder.coqTermQualid typeName) (Lists.map (\p -> Coder.coqTermQualid p) params))

-- | Build a Coq `(p : Type)` binder for a type parameter
makeTypeBinder :: String -> Syntax.Binder
makeTypeBinder p =
    Syntax.BinderType (Syntax.TypeBinders {
      Syntax.typeBindersNames = [
        Coder.coqName p],
      Syntax.typeBindersType = (Syntax.Type (Coder.coqTermQualid "Type"))})

-- | Collect type-variable names and the Coq binders needed for a term definition
mkTypeBinders :: Core.Term -> [Core.Name] -> ([String], [Syntax.Binder])
mkTypeBinders body typeVars =

      let schemeVarNames = Sets.fromList (Lists.map (\n -> Core.unName n) typeVars)
          innerTypeVars = Logic.ifElse (Lists.null typeVars) Sets.empty (Utils.collectFreeTypeVars body)
          explicit = Lists.map (\n -> Core.unName n) typeVars
          extras = Lists.filter (\nm -> Logic.not (Sets.member nm schemeVarNames)) (Sets.toList innerTypeVars)
          allTypeVarNames = Lists.nub (Lists.concat2 explicit extras)
          binders = Lists.map (\v -> makeTypeBinder v) allTypeVarNames
      in (allTypeVarNames, binders)

-- | Top-level driver: dispatch a module to either full-emission or axiom-only emission, producing (path, content) pairs
moduleToCoq :: M.Map (String, String) String -> M.Map String Int -> S.Set String -> S.Set String -> Packaging.Module -> [Packaging.Definition] -> M.Map String String
moduleToCoq fieldMap constrCounts ambiguousNames globalSanitizedAcc mod_ defs =

      let nsStr = Packaging.unNamespace (Packaging.moduleNamespace mod_)
          path = namespaceToPath nsStr
          desc =
                  Maybes.maybe "" (\d -> Strings.cat [
                    "(* ",
                    d,
                    " *)\n\n"]) (Packaging.moduleDescription mod_)
          axiomOnlyModules =
                  [
                    "hydra.hoisting",
                    "hydra.inference"]
          isAxiomOnly = Lists.elem nsStr axiomOnlyModules
          typeDefs =
                  Maybes.cat (Lists.map (\def_ -> case def_ of
                    Packaging.DefinitionType v0 -> Just (Utils.localName (Core.unName (Packaging.typeDefinitionName v0)), (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                    _ -> Nothing) defs)
          termDefs =
                  Maybes.cat (Lists.map (\def_ -> case def_ of
                    Packaging.DefinitionTerm v0 ->
                      let mts = Packaging.termDefinitionType v0
                          vs = Maybes.maybe [] (\ts -> Core.typeSchemeVariables ts) mts
                          mty = Maybes.map (\ts -> Core.typeSchemeType ts) mts
                      in (Just (Utils.localName (Core.unName (Packaging.termDefinitionName v0)), (Packaging.termDefinitionTerm v0, (vs, mty))))
                    _ -> Nothing) defs)
          localDefNames =
                  Sets.fromList (Lists.concat2 (Lists.map (\nt -> Pairs.first nt) typeDefs) (Lists.map (\td -> Pairs.first td) termDefs))
          moduleAmbig = Sets.union ambiguousNames localDefNames
          env =
                  Environment.CoqEnvironment {
                    Environment.coqEnvironmentCurrentNamespace = nsStr,
                    Environment.coqEnvironmentConstructorCounts = constrCounts,
                    Environment.coqEnvironmentAmbiguousNames = moduleAmbig,
                    Environment.coqEnvironmentSanitizedAccessors = globalSanitizedAcc}
      in (Logic.ifElse isAxiomOnly (Maps.fromList [
        (path, (buildAxiomOnlyContent env desc nsStr typeDefs termDefs mod_))]) (buildFullModule env fieldMap mod_ nsStr path desc typeDefs termDefs))

-- | Convert a Hydra namespace string (e.g. hydra.show.core) into a relative .v file path
namespaceToPath :: String -> String
namespaceToPath ns =

      let parts = Strings.splitOn "." ns
          dirParts = Lists.init parts
          fileName =
                  Strings.cat [
                    Lists.last parts,
                    ".v"]
      in (Logic.ifElse (Lists.null dirParts) fileName (Strings.cat [
        Strings.intercalate "/" dirParts,
        "/",
        fileName]))

-- | Pretty-print the standard-imports sentence followed by additional dependency imports
renderRequireImports :: [Syntax.Sentence] -> String
renderRequireImports depSentences = renderSentences (Lists.cons Coder.standardImports depSentences)

-- | Pretty-print a Document containing the given Coq sentences
renderSentences :: [Syntax.Sentence] -> String
renderSentences sentences =
    Serialization.printExpr (Serialization.parenthesize (Serde.documentToExpr (Syntax.Document {
      Syntax.documentSentences = sentences})))

-- | Replace literal `bundle_` with the given replacement string
replaceBundle :: String -> String -> String
replaceBundle s bname = Strings.intercalate bname (Strings.splitOn "bundle_" s)
