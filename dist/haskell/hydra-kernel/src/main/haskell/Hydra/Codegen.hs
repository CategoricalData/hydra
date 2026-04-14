-- Note: this is an automatically generated file. Do not edit.

-- | Pure code generation pipeline for bootstrapping Hydra across languages.

module Hydra.Codegen where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Packaging as DecodePackaging
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Packaging as EncodePackaging
import qualified Hydra.Environment as Environment
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Lexical as Lexical
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
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Build a schema map (Name -> Type) from a graph's schema types
buildSchemaMap :: Graph.Graph -> M.Map Core.Name Core.Type
buildSchemaMap g = Maps.map (\ts -> Strip.deannotateType (Core.typeSchemeType ts)) (Graph.graphSchemaTypes g)

-- | Decode a single module from a JSON value
decodeModuleFromJson :: Graph.Graph -> [Packaging.Module] -> Model.Value -> Either Errors.Error Packaging.Module
decodeModuleFromJson bsGraph universeModules jsonVal =

      let graph = modulesToGraph bsGraph universeModules universeModules
          schemaMap = buildSchemaMap graph
          modType = Core.TypeVariable (Core.Name "hydra.packaging.Module")
      in (Eithers.either (\err -> Left (Errors.ErrorOther (Errors.OtherError err))) (\term -> Eithers.either (\decErr -> Left (Errors.ErrorDecoding decErr)) (\mod -> Right mod) (DecodePackaging.module_ graph term)) (Decode.fromJson schemaMap (Core.Name "hydra.packaging.Module") modType jsonVal))

-- | Escape unescaped control characters inside JSON string literals
escapeControlCharsInJson :: [Int] -> [Int]
escapeControlCharsInJson input =

      let hexDigit = \n -> Logic.ifElse (Equality.lt n 10) (Math.add 48 n) (Math.add 97 (Math.sub n 10))
          escapeToUnicode =
                  \b -> [
                    92,
                    117,
                    48,
                    48,
                    (hexDigit (Math.div b 16)),
                    (hexDigit (Math.mod b 16))]
          go =
                  \inStr -> \esc -> \bytes -> Logic.ifElse (Lists.null bytes) [] (
                    let b = Lists.head bytes
                        bs = Lists.tail bytes
                    in (Logic.ifElse esc (Lists.cons b (go inStr False bs)) (Logic.ifElse (Logic.and (Equality.equal b 92) inStr) (Lists.cons b (go inStr True bs)) (Logic.ifElse (Equality.equal b 34) (Lists.cons b (go (Logic.not inStr) False bs)) (Logic.ifElse (Logic.and inStr (Equality.lt b 32)) (Lists.concat2 (escapeToUnicode b) (go inStr False bs)) (Lists.cons b (go inStr False bs)))))))
      in (go False False input)

-- | Format a primitive for the lexicon
formatPrimitive :: Graph.Primitive -> String
formatPrimitive prim =

      let name = Core.unName (Graph.primitiveName prim)
          typeStr = ShowCore.typeScheme (Graph.primitiveType prim)
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a term binding for the lexicon
formatTermBinding :: Core.Binding -> String
formatTermBinding binding =

      let name = Core.unName (Core.bindingName binding)
          typeStr = Maybes.maybe "?" (\scheme -> ShowCore.typeScheme scheme) (Core.bindingType binding)
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a type binding for the lexicon
formatTypeBinding :: Graph.Graph -> Core.Binding -> Either Errors.Error String
formatTypeBinding graph binding =
    Eithers.bind (Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\_a -> _a) (DecodeCore.type_ graph (Core.bindingTerm binding))) (\typ -> Right (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " (Core.unName (Core.bindingName binding))) " = ") (ShowCore.type_ typ)))

-- | Generate encoder or decoder modules for a list of type modules
generateCoderModules :: (t0 -> Graph.Graph -> t1 -> Either t2 (Maybe t3)) -> Graph.Graph -> [Packaging.Module] -> [t1] -> t0 -> Either t2 [t3]
generateCoderModules codec bsGraph universeModules typeModules cx =

      let universe = Maps.fromList (Lists.map (\m -> (Packaging.moduleNamespace m, m)) (Lists.concat2 universeModules universeModules))
          schemaModules = moduleTypeDepsTransitive universe universeModules
          dataModules = moduleTermDepsTransitive universe universeModules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) (Lists.concat2 schemaModules universeModules))
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) dataModules)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes = Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment schemaGraph)
          allElements = Lists.concat2 schemaElements dataElements
          graph = Lexical.elementsToGraph bsGraph schemaTypes allElements
      in (Eithers.map (\results -> Maybes.cat results) (Eithers.mapList (\m -> codec cx graph m) typeModules))

-- | Generate the lexicon content from a graph
generateLexicon :: Graph.Graph -> Either Errors.Error String
generateLexicon graph =

      let bindings = Lexical.graphToBindings graph
          primitives = Maps.elems (Graph.graphPrimitives graph)
          partitioned = Lists.partition (\b -> Annotations.isNativeType b) bindings
          typeBindings = Pairs.first partitioned
          termBindings = Pairs.second partitioned
          sortedPrimitives = Lists.sortOn (\p -> Graph.primitiveName p) primitives
          sortedTypes = Lists.sortOn (\b -> Core.bindingName b) typeBindings
          sortedTerms = Lists.sortOn (\b -> Core.bindingName b) termBindings
      in (Eithers.bind (Eithers.mapList (\b -> formatTypeBinding graph b) sortedTypes) (\typeLines ->
        let termLines = Lists.map (\b -> formatTermBinding b) sortedTerms
            primitiveLines = Lists.map (\p -> formatPrimitive p) sortedPrimitives
        in (Right (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "Primitives:\n" (Strings.unlines primitiveLines)) "\nTypes:\n") (Strings.unlines typeLines)) "\nTerms:\n") (Strings.unlines termLines)))))

-- | Pure core of code generation: given a coder, language, flags, bootstrap graph, universe, and modules to generate, produce a list of (filePath, content) pairs.
generateSourceFiles :: Ord t0 => ((Packaging.Module -> [Packaging.Definition] -> Context.Context -> Graph.Graph -> Either Errors.Error (M.Map t0 t1)) -> Coders.Language -> Bool -> Bool -> Bool -> Bool -> Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Context.Context -> Either Errors.Error [(t0, t1)])
generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bsGraph universeModules modsToGenerate cx =

      let namespaceMap =
              Maps.fromList (Lists.map (\m -> (Packaging.moduleNamespace m, m)) (Lists.concat2 universeModules modsToGenerate))
          constraints = Coders.languageConstraints lang
          typeModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions mod))))) modsToGenerate
          termModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions mod))))) modsToGenerate
          schemaMods = moduleTypeDepsTransitive namespaceMap modsToGenerate
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) (Lists.concat2 schemaMods typeModulesToGenerate))
          dataMods = moduleTermDepsTransitive namespaceMap modsToGenerate
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) dataMods)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes2 = Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment schemaGraph)
          dataGraph = Lexical.elementsToGraph bsGraph schemaTypes2 dataElements
      in (Eithers.bind (Logic.ifElse (Lists.null typeModulesToGenerate) (Right []) (
        let nameLists =
                Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                  Packaging.DefinitionType v0 -> Just (Packaging.typeDefinitionName v0)
                  _ -> Nothing) (Packaging.moduleDefinitions m))) typeModulesToGenerate
        in (Eithers.bind (Adapt.schemaGraphToDefinitions constraints schemaGraph nameLists cx) (\schemaResult ->
          let defLists = Pairs.second schemaResult
              schemaGraphWithTypes =
                      Graph.Graph {
                        Graph.graphBoundTerms = (Graph.graphBoundTerms schemaGraph),
                        Graph.graphBoundTypes = (Graph.graphBoundTypes schemaGraph),
                        Graph.graphClassConstraints = (Graph.graphClassConstraints schemaGraph),
                        Graph.graphLambdaVariables = (Graph.graphLambdaVariables schemaGraph),
                        Graph.graphMetadata = (Graph.graphMetadata schemaGraph),
                        Graph.graphPrimitives = (Graph.graphPrimitives schemaGraph),
                        Graph.graphSchemaTypes = schemaTypes2,
                        Graph.graphTypeVariables = (Graph.graphTypeVariables schemaGraph)}
          in (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\p ->
            let mod = Pairs.first p
                defs = Pairs.second p
            in (Eithers.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Packaging.DefinitionType d) defs) cx schemaGraphWithTypes))) (Lists.zip typeModulesToGenerate defLists))))))) (\schemaFiles -> Eithers.bind (Logic.ifElse (Lists.null termModulesToGenerate) (Right []) (
        let namespaces = Lists.map (\m -> Packaging.moduleNamespace m) termModulesToGenerate
        in (Eithers.bind (Adapt.dataGraphToDefinitions constraints doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings dataElements dataGraph namespaces cx) (\dataResult ->
          let g1 = Pairs.first dataResult
              defLists = Pairs.second dataResult
              defName =
                      \d -> case d of
                        Packaging.DefinitionTerm v0 -> Packaging.termDefinitionName v0
                        Packaging.DefinitionType v0 -> Packaging.typeDefinitionName v0
              refreshModule =
                      \els -> \m -> Packaging.Module {
                        Packaging.moduleNamespace = (Packaging.moduleNamespace m),
                        Packaging.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
                          Packaging.DefinitionType v0 -> Just (Packaging.DefinitionType v0)
                          Packaging.DefinitionTerm v0 -> Maybes.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                            Packaging.termDefinitionName = (Core.bindingName b),
                            Packaging.termDefinitionTerm = (Core.bindingTerm b),
                            Packaging.termDefinitionType = (Core.bindingType b)})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging.termDefinitionName v0)) els)) (Packaging.moduleDefinitions m))),
                        Packaging.moduleTermDependencies = (Packaging.moduleTermDependencies m),
                        Packaging.moduleTypeDependencies = (Packaging.moduleTypeDependencies m),
                        Packaging.moduleDescription = (Packaging.moduleDescription m)}
              allBindings = Lexical.graphToBindings g1
              refreshedMods = Lists.map (\m -> refreshModule allBindings m) termModulesToGenerate
              dedupDefs = \defs -> Maps.elems (Maps.fromList (Lists.map (\d -> (Packaging.termDefinitionName d, d)) defs))
              dedupedDefLists = Lists.map dedupDefs defLists
          in (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\p ->
            let mod = Pairs.first p
                defs = Pairs.second p
            in (Eithers.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Packaging.DefinitionTerm d) defs) cx g1))) (Lists.zip refreshedMods dedupedDefLists))))))) (\termFiles -> Right (Lists.concat2 schemaFiles termFiles))))

-- | Perform type inference and generate the lexicon for a set of modules
inferAndGenerateLexicon :: Context.Context -> Graph.Graph -> [Packaging.Module] -> Either Errors.Error String
inferAndGenerateLexicon cx bsGraph kernelModules =

      let g0 = modulesToGraph bsGraph kernelModules kernelModules
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) kernelModules)
      in (Eithers.bind (Inference.inferGraphTypes cx dataElements g0) (\inferResultWithCx ->
        let g1 = Pairs.first (Pairs.first inferResultWithCx)
        in (generateLexicon g1)))

-- | Perform type inference on modules and reconstruct with inferred types
inferModules :: Context.Context -> Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Either Errors.Error [Packaging.Module]
inferModules cx bsGraph universeMods targetMods =

      let g0 = modulesToGraph bsGraph universeMods universeMods
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) universeMods)
      in (Eithers.bind (Inference.inferGraphTypes cx dataElements g0) (\inferResultWithCx ->
        let inferResult = Pairs.first inferResultWithCx
            g1 = Pairs.first inferResult
            inferredElements = Pairs.second inferResult
            isTypeOnlyModule =
                    \mod -> Logic.not (Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                      Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                        Core.bindingName = (Packaging.termDefinitionName v0),
                        Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                        Core.bindingType = (Packaging.termDefinitionType v0)})
                      _ -> Nothing) (Packaging.moduleDefinitions mod)))))
            defName =
                    \d -> case d of
                      Packaging.DefinitionTerm v0 -> Packaging.termDefinitionName v0
                      Packaging.DefinitionType v0 -> Packaging.typeDefinitionName v0
            refreshModule =
                    \m -> Logic.ifElse (isTypeOnlyModule m) m (Packaging.Module {
                      Packaging.moduleNamespace = (Packaging.moduleNamespace m),
                      Packaging.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
                        Packaging.DefinitionType v0 -> Just (Packaging.DefinitionType v0)
                        Packaging.DefinitionTerm v0 -> Maybes.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                          Packaging.termDefinitionName = (Core.bindingName b),
                          Packaging.termDefinitionTerm = (Core.bindingTerm b),
                          Packaging.termDefinitionType = (Core.bindingType b)})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging.termDefinitionName v0)) inferredElements)) (Packaging.moduleDefinitions m))),
                      Packaging.moduleTermDependencies = (Packaging.moduleTermDependencies m),
                      Packaging.moduleTypeDependencies = (Packaging.moduleTypeDependencies m),
                      Packaging.moduleDescription = (Packaging.moduleDescription m)})
        in (Right (Lists.map refreshModule targetMods))))

-- | Compute transitive closure of term dependencies for a set of modules
moduleTermDepsTransitive :: M.Map Packaging.Namespace Packaging.Module -> [Packaging.Module] -> [Packaging.Module]
moduleTermDepsTransitive nsMap modules =

      let closure =
              Sets.union (transitiveDeps (\m -> Packaging.moduleTermDependencies m) nsMap modules) (Sets.fromList (Lists.map (\m -> Packaging.moduleNamespace m) modules))
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) (Sets.toList closure)))

-- | Convert a Module to a JSON string
moduleToJson :: M.Map Core.Name Core.Type -> Packaging.Module -> Either Errors.Error String
moduleToJson schemaMap m =

      let term = EncodePackaging.module_ m
          modType = Core.TypeVariable (Core.Name "hydra.packaging.Module")
      in (Eithers.map (\json -> Writer.printJson json) (Eithers.bimap (\_e -> Errors.ErrorOther (Errors.OtherError _e)) (\_a -> _a) (Encode.toJson schemaMap (Core.Name "hydra.packaging.Module") modType term)))

-- | Convert a generated Module into a Source module
moduleToSourceModule :: Packaging.Module -> Packaging.Module
moduleToSourceModule m =

      let sourceNs =
              Packaging.Namespace (Strings.cat2 "hydra.sources." (Strings.intercalate "." (Lists.drop 1 (Strings.splitOn "." (Packaging.unNamespace (Packaging.moduleNamespace m))))))
          modTypeNs = Packaging.Namespace "hydra.packaging"
          moduleDef =
                  Packaging.DefinitionTerm (Packaging.TermDefinition {
                    Packaging.termDefinitionName = (Core.Name (Strings.cat2 (Packaging.unNamespace sourceNs) ".module_")),
                    Packaging.termDefinitionTerm = (EncodePackaging.module_ m),
                    Packaging.termDefinitionType = Nothing})
      in Packaging.Module {
        Packaging.moduleNamespace = sourceNs,
        Packaging.moduleDefinitions = [
          moduleDef],
        Packaging.moduleTermDependencies = [
          modTypeNs],
        Packaging.moduleTypeDependencies = [
          modTypeNs],
        Packaging.moduleDescription = (Just (Strings.cat2 "Source module for " (Packaging.unNamespace (Packaging.moduleNamespace m))))}

-- | Compute transitive closure of type dependencies for a set of modules
moduleTypeDepsTransitive :: M.Map Packaging.Namespace Packaging.Module -> [Packaging.Module] -> [Packaging.Module]
moduleTypeDepsTransitive nsMap modules =

      let termMods = moduleTermDepsTransitive nsMap modules
          typeNamespaces = Sets.toList (transitiveDeps (\m -> Packaging.moduleTypeDependencies m) nsMap termMods)
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) typeNamespaces))

-- | Build a graph from universe modules and working modules, using an explicit bootstrap graph
modulesToGraph :: Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Graph.Graph
modulesToGraph bsGraph universeModules modules =

      let universe = Maps.fromList (Lists.map (\m -> (Packaging.moduleNamespace m, m)) (Lists.concat2 universeModules modules))
          schemaModules = moduleTypeDepsTransitive universe modules
          dataModules = moduleTermDepsTransitive universe modules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeType (Packaging.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) (Lists.concat2 schemaModules modules))
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingType = (Packaging.termDefinitionType v0)})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) dataModules)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes = Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment schemaGraph)
      in (Lexical.elementsToGraph bsGraph schemaTypes dataElements)

-- | Convert a namespace to a file path (e.g., hydra.core -> hydra/core)
namespaceToPath :: Packaging.Namespace -> String
namespaceToPath ns = Strings.intercalate "/" (Strings.splitOn "." (Packaging.unNamespace ns))

-- | Compute transitive closure of module dependencies
transitiveDeps :: (Packaging.Module -> [Packaging.Namespace]) -> M.Map Packaging.Namespace Packaging.Module -> [Packaging.Module] -> S.Set Packaging.Namespace
transitiveDeps getDeps nsMap startMods =

      let initialDeps =
              Sets.fromList (Lists.concat (Lists.map (\m -> Lists.filter (\dep -> Logic.not (Equality.equal dep (Packaging.moduleNamespace m))) (getDeps m)) startMods))
          go =
                  \pending -> \visited -> Logic.ifElse (Sets.null pending) visited (
                    let newVisited = Sets.union visited pending
                        nextDeps =
                                Sets.fromList (Lists.concat (Lists.map (\nsv -> Maybes.maybe [] (\depMod -> getDeps depMod) (Maps.lookup nsv nsMap)) (Sets.toList pending)))
                        newPending = Sets.difference nextDeps newVisited
                    in (go newPending newVisited))
      in (go initialDeps Sets.empty)
