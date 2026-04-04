-- Note: this is an automatically generated file. Do not edit.

-- | Pure code generation pipeline for bootstrapping Hydra across languages.

module Hydra.Codegen where

import qualified Hydra.Adapt as Adapt
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Packaging as Packaging
import qualified Hydra.Encode.Core as Core__
import qualified Hydra.Encode.Packaging as Packaging_
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
import qualified Hydra.Packaging as Packaging__
import qualified Hydra.Show.Core as Core___
import qualified Hydra.Show.Errors as Errors_
import qualified Hydra.Strip as Strip
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Build a schema map (Name -> Type) from a graph's schema types
buildSchemaMap :: Graph.Graph -> M.Map Core.Name Core.Type
buildSchemaMap g = Maps.map (\ts -> Strip.deannotateType (Core.typeSchemeType ts)) (Graph.graphSchemaTypes g)

-- | Decode a single module from a JSON value
decodeModuleFromJson :: Graph.Graph -> [Packaging__.Module] -> Model.Value -> Either String Packaging__.Module
decodeModuleFromJson bsGraph universeModules jsonVal =

      let graph = modulesToGraph bsGraph universeModules universeModules
          schemaMap = buildSchemaMap graph
          modType = Core.TypeVariable (Core.Name "hydra.packaging.Module")
      in (Eithers.either (\err -> Left err) (\term -> Eithers.either (\decErr -> Left (Errors.unDecodingError decErr)) (\mod -> Right mod) (Packaging.module_ graph term)) (Decode.fromJson schemaMap (Core.Name "hydra.packaging.Module") modType jsonVal))

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
          typeStr = Core___.typeScheme (Graph.primitiveType prim)
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a term binding for the lexicon
formatTermBinding :: Core.Binding -> String
formatTermBinding binding =

      let name = Core.unName (Core.bindingName binding)
          typeStr = Maybes.maybe "?" (\scheme -> Core___.typeScheme scheme) (Core.bindingType binding)
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a type binding for the lexicon
formatTypeBinding :: Graph.Graph -> Core.Binding -> Either Errors.DecodingError String
formatTypeBinding graph binding =
    Eithers.bind (Core_.type_ graph (Core.bindingTerm binding)) (\typ -> Right (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " (Core.unName (Core.bindingName binding))) " = ") (Core___.type_ typ)))

-- | Generate encoder or decoder modules for a list of type modules
generateCoderModules :: (t0 -> Graph.Graph -> t1 -> Either t2 (Maybe t3)) -> Graph.Graph -> [Packaging__.Module] -> [t1] -> t0 -> Either t2 [t3]
generateCoderModules codec bsGraph universeModules typeModules cx =

      let universe =
              Maps.fromList (Lists.map (\m -> (Packaging__.moduleNamespace m, m)) (Lists.concat2 universeModules universeModules))
          schemaModules = moduleTypeDepsTransitive universe universeModules
          dataModules = moduleTermDepsTransitive universe universeModules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (Core__.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging__.typeDefinitionName v0) (Core.typeSchemeType (Packaging__.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) (Lists.concat2 schemaModules universeModules))
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) dataModules)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes =
                  Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment Lexical.emptyContext schemaGraph)
          allElements = Lists.concat2 schemaElements dataElements
          graph = Lexical.elementsToGraph bsGraph schemaTypes allElements
      in (Eithers.map (\results -> Maybes.cat results) (Eithers.mapList (\m -> codec cx graph m) typeModules))

-- | Generate the lexicon content from a graph
generateLexicon :: Graph.Graph -> Either Errors.DecodingError String
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
generateSourceFiles :: Ord t0 => ((Packaging__.Module -> [Packaging__.Definition] -> Context.Context -> Graph.Graph -> Either (Context.InContext Errors.Error) (M.Map t0 t1)) -> Coders.Language -> Bool -> Bool -> Bool -> Bool -> Graph.Graph -> [Packaging__.Module] -> [Packaging__.Module] -> Context.Context -> Either (Context.InContext Errors.Error) [(t0, t1)])
generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bsGraph universeModules modsToGenerate cx =

      let namespaceMap =
              Maps.fromList (Lists.map (\m -> (Packaging__.moduleNamespace m, m)) (Lists.concat2 universeModules modsToGenerate))
          constraints = Coders.languageConstraints lang
          typeModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (Core__.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging__.typeDefinitionName v0) (Core.typeSchemeType (Packaging__.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging__.moduleDefinitions mod))))) modsToGenerate
          termModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions mod))))) modsToGenerate
          schemaMods = moduleTypeDepsTransitive namespaceMap modsToGenerate
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (Core__.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging__.typeDefinitionName v0) (Core.typeSchemeType (Packaging__.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) (Lists.concat2 schemaMods typeModulesToGenerate))
          dataMods = moduleTermDepsTransitive namespaceMap modsToGenerate
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) dataMods)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes2 =
                  Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment Lexical.emptyContext schemaGraph)
          dataGraph = Lexical.elementsToGraph bsGraph schemaTypes2 dataElements
      in (Eithers.bind (Logic.ifElse (Lists.null typeModulesToGenerate) (Right []) (
        let nameLists =
                Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                  Packaging__.DefinitionType v0 -> Just (Packaging__.typeDefinitionName v0)
                  _ -> Nothing) (Packaging__.moduleDefinitions m))) typeModulesToGenerate
        in (Eithers.bind (Eithers.bimap (\s -> Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError s)),
          Context.inContextContext = cx}) (\r -> r) (Adapt.schemaGraphToDefinitions constraints schemaGraph nameLists cx)) (\schemaResult ->
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
            in (Eithers.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Packaging__.DefinitionType d) defs) cx schemaGraphWithTypes))) (Lists.zip typeModulesToGenerate defLists))))))) (\schemaFiles -> Eithers.bind (Logic.ifElse (Lists.null termModulesToGenerate) (Right []) (
        let namespaces = Lists.map (\m -> Packaging__.moduleNamespace m) termModulesToGenerate
        in (Eithers.bind (Eithers.bimap (\s -> Context.InContext {
          Context.inContextObject = (Errors.ErrorOther (Errors.OtherError s)),
          Context.inContextContext = cx}) (\r -> r) (Adapt.dataGraphToDefinitions constraints doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings dataElements dataGraph namespaces cx)) (\dataResult ->
          let g1 = Pairs.first dataResult
              defLists = Pairs.second dataResult
              defName =
                      \d -> case d of
                        Packaging__.DefinitionTerm v0 -> Packaging__.termDefinitionName v0
                        Packaging__.DefinitionType v0 -> Packaging__.typeDefinitionName v0
              refreshModule =
                      \els -> \m -> Packaging__.Module {
                        Packaging__.moduleNamespace = (Packaging__.moduleNamespace m),
                        Packaging__.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
                          Packaging__.DefinitionType v0 -> Just (Packaging__.DefinitionType v0)
                          Packaging__.DefinitionTerm v0 -> Maybes.map (\b -> Packaging__.DefinitionTerm (Packaging__.TermDefinition {
                            Packaging__.termDefinitionName = (Core.bindingName b),
                            Packaging__.termDefinitionTerm = (Core.bindingTerm b),
                            Packaging__.termDefinitionType = (Core.bindingType b)})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging__.termDefinitionName v0)) els)) (Packaging__.moduleDefinitions m))),
                        Packaging__.moduleTermDependencies = (Packaging__.moduleTermDependencies m),
                        Packaging__.moduleTypeDependencies = (Packaging__.moduleTypeDependencies m),
                        Packaging__.moduleDescription = (Packaging__.moduleDescription m)}
              allBindings = Lexical.graphToBindings g1
              refreshedMods = Lists.map (\m -> refreshModule allBindings m) termModulesToGenerate
              dedupDefs = \defs -> Maps.elems (Maps.fromList (Lists.map (\d -> (Packaging__.termDefinitionName d, d)) defs))
              dedupedDefLists = Lists.map dedupDefs defLists
          in (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\p ->
            let mod = Pairs.first p
                defs = Pairs.second p
            in (Eithers.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Packaging__.DefinitionTerm d) defs) cx g1))) (Lists.zip refreshedMods dedupedDefLists))))))) (\termFiles -> Right (Lists.concat2 schemaFiles termFiles))))

-- | Perform type inference and generate the lexicon for a set of modules
inferAndGenerateLexicon :: Context.Context -> Graph.Graph -> [Packaging__.Module] -> Either String String
inferAndGenerateLexicon cx bsGraph kernelModules =

      let g0 = modulesToGraph bsGraph kernelModules kernelModules
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) kernelModules)
      in (Eithers.bind (Eithers.bimap (\ic -> Errors_.error (Context.inContextObject ic)) (\x -> x) (Inference.inferGraphTypes cx dataElements g0)) (\inferResultWithCx ->
        let g1 = Pairs.first (Pairs.first inferResultWithCx)
        in (Eithers.bimap Errors.unDecodingError (\x -> x) (generateLexicon g1))))

-- | Perform type inference on modules and reconstruct with inferred types
inferModules :: Context.Context -> Graph.Graph -> [Packaging__.Module] -> [Packaging__.Module] -> Either (Context.InContext Errors.Error) [Packaging__.Module]
inferModules cx bsGraph universeMods targetMods =

      let g0 = modulesToGraph bsGraph universeMods universeMods
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) universeMods)
      in (Eithers.bind (Inference.inferGraphTypes cx dataElements g0) (\inferResultWithCx ->
        let inferResult = Pairs.first inferResultWithCx
            g1 = Pairs.first inferResult
            inferredElements = Pairs.second inferResult
            isTypeOnlyModule =
                    \mod -> Logic.not (Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                      Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                        Core.bindingName = (Packaging__.termDefinitionName v0),
                        Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                        Core.bindingType = (Packaging__.termDefinitionType v0)})
                      _ -> Nothing) (Packaging__.moduleDefinitions mod)))))
            defName =
                    \d -> case d of
                      Packaging__.DefinitionTerm v0 -> Packaging__.termDefinitionName v0
                      Packaging__.DefinitionType v0 -> Packaging__.typeDefinitionName v0
            refreshModule =
                    \m -> Logic.ifElse (isTypeOnlyModule m) m (Packaging__.Module {
                      Packaging__.moduleNamespace = (Packaging__.moduleNamespace m),
                      Packaging__.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
                        Packaging__.DefinitionType v0 -> Just (Packaging__.DefinitionType v0)
                        Packaging__.DefinitionTerm v0 -> Maybes.map (\b -> Packaging__.DefinitionTerm (Packaging__.TermDefinition {
                          Packaging__.termDefinitionName = (Core.bindingName b),
                          Packaging__.termDefinitionTerm = (Core.bindingTerm b),
                          Packaging__.termDefinitionType = (Core.bindingType b)})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging__.termDefinitionName v0)) inferredElements)) (Packaging__.moduleDefinitions m))),
                      Packaging__.moduleTermDependencies = (Packaging__.moduleTermDependencies m),
                      Packaging__.moduleTypeDependencies = (Packaging__.moduleTypeDependencies m),
                      Packaging__.moduleDescription = (Packaging__.moduleDescription m)})
        in (Right (Lists.map refreshModule targetMods))))

-- | Compute transitive closure of term dependencies for a set of modules
moduleTermDepsTransitive :: M.Map Packaging__.Namespace Packaging__.Module -> [Packaging__.Module] -> [Packaging__.Module]
moduleTermDepsTransitive nsMap modules =

      let closure =
              Sets.union (transitiveDeps (\m -> Packaging__.moduleTermDependencies m) nsMap modules) (Sets.fromList (Lists.map (\m -> Packaging__.moduleNamespace m) modules))
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) (Sets.toList closure)))

-- | Convert a Module to a JSON string
moduleToJson :: Packaging__.Module -> Either String String
moduleToJson m =

      let term = Packaging_.module_ m
          modType = Core.TypeVariable (Core.Name "hydra.packaging.Module")
      in (Eithers.map (\json -> Writer.printJson json) (Encode.toJson Maps.empty (Core.Name "hydra.packaging.Module") modType term))

-- | Convert a generated Module into a Source module
moduleToSourceModule :: Packaging__.Module -> Packaging__.Module
moduleToSourceModule m =

      let sourceNs =
              Packaging__.Namespace (Strings.cat2 "hydra.sources." (Strings.intercalate "." (Lists.drop 1 (Strings.splitOn "." (Packaging__.unNamespace (Packaging__.moduleNamespace m))))))
          modTypeNs = Packaging__.Namespace "hydra.packaging"
          moduleDef =
                  Packaging__.DefinitionTerm (Packaging__.TermDefinition {
                    Packaging__.termDefinitionName = (Core.Name (Strings.cat2 (Packaging__.unNamespace sourceNs) ".module_")),
                    Packaging__.termDefinitionTerm = (Packaging_.module_ m),
                    Packaging__.termDefinitionType = Nothing})
      in Packaging__.Module {
        Packaging__.moduleNamespace = sourceNs,
        Packaging__.moduleDefinitions = [
          moduleDef],
        Packaging__.moduleTermDependencies = [
          modTypeNs],
        Packaging__.moduleTypeDependencies = [
          modTypeNs],
        Packaging__.moduleDescription = (Just (Strings.cat2 "Source module for " (Packaging__.unNamespace (Packaging__.moduleNamespace m))))}

-- | Compute transitive closure of type dependencies for a set of modules
moduleTypeDepsTransitive :: M.Map Packaging__.Namespace Packaging__.Module -> [Packaging__.Module] -> [Packaging__.Module]
moduleTypeDepsTransitive nsMap modules =

      let termMods = moduleTermDepsTransitive nsMap modules
          typeNamespaces = Sets.toList (transitiveDeps (\m -> Packaging__.moduleTypeDependencies m) nsMap termMods)
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) typeNamespaces))

-- | Build a graph from universe modules and working modules, using an explicit bootstrap graph
modulesToGraph :: Graph.Graph -> [Packaging__.Module] -> [Packaging__.Module] -> Graph.Graph
modulesToGraph bsGraph universeModules modules =

      let universe = Maps.fromList (Lists.map (\m -> (Packaging__.moduleNamespace m, m)) (Lists.concat2 universeModules modules))
          schemaModules = moduleTypeDepsTransitive universe modules
          dataModules = moduleTermDepsTransitive universe modules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (Core__.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.key_type, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingType = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeType = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging__.typeDefinitionName v0) (Core.typeSchemeType (Packaging__.typeDefinitionType v0)))
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) (Lists.concat2 schemaModules modules))
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging__.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging__.termDefinitionName v0),
                      Core.bindingTerm = (Packaging__.termDefinitionTerm v0),
                      Core.bindingType = (Packaging__.termDefinitionType v0)})
                    _ -> Nothing) (Packaging__.moduleDefinitions m))) dataModules)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes =
                  Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment Lexical.emptyContext schemaGraph)
      in (Lexical.elementsToGraph bsGraph schemaTypes dataElements)

-- | Convert a namespace to a file path (e.g., hydra.core -> hydra/core)
namespaceToPath :: Packaging__.Namespace -> String
namespaceToPath ns = Strings.intercalate "/" (Strings.splitOn "." (Packaging__.unNamespace ns))

-- | Compute transitive closure of module dependencies
transitiveDeps :: (Packaging__.Module -> [Packaging__.Namespace]) -> M.Map Packaging__.Namespace Packaging__.Module -> [Packaging__.Module] -> S.Set Packaging__.Namespace
transitiveDeps getDeps nsMap startMods =

      let initialDeps =
              Sets.fromList (Lists.concat (Lists.map (\m -> Lists.filter (\dep -> Logic.not (Equality.equal dep (Packaging__.moduleNamespace m))) (getDeps m)) startMods))
          go =
                  \pending -> \visited -> Logic.ifElse (Sets.null pending) visited (
                    let newVisited = Sets.union visited pending
                        nextDeps =
                                Sets.fromList (Lists.concat (Lists.map (\nsv -> Maybes.maybe [] (\depMod -> getDeps depMod) (Maps.lookup nsv nsMap)) (Sets.toList pending)))
                        newPending = Sets.difference nextDeps newVisited
                    in (go newPending newVisited))
      in (go initialDeps Sets.empty)
