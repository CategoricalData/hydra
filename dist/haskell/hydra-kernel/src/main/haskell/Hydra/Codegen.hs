-- Note: this is an automatically generated file. Do not edit.
-- | Pure code generation pipeline for bootstrapping Hydra across languages.

module Hydra.Codegen where
import qualified Hydra.Adapt as Adapt
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Constants as Constants
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Decode.Packaging as DecodePackaging
import qualified Hydra.Decoding as Decoding
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Packaging as EncodePackaging
import qualified Hydra.Encoding as Encoding
import qualified Hydra.Environment as Environment
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Haskell.Lib.Eithers as Eithers
import qualified Hydra.Haskell.Lib.Equality as Equality
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Logic as Logic
import qualified Hydra.Haskell.Lib.Maps as Maps
import qualified Hydra.Haskell.Lib.Math as Math
import qualified Hydra.Haskell.Lib.Maybes as Maybes
import qualified Hydra.Haskell.Lib.Pairs as Pairs
import qualified Hydra.Haskell.Lib.Sets as Sets
import qualified Hydra.Haskell.Lib.Strings as Strings
import qualified Hydra.Names as Names
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.Scoping as Scoping
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Show.Errors as ShowErrors
import qualified Hydra.Strip as Strip
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Build a schema map (Name -> Type) from a graph's schema types
buildSchemaMap :: Graph.Graph -> M.Map Core.Name Core.Type
buildSchemaMap g = Maps.map (\ts -> Strip.deannotateType (Core.typeSchemeBody ts)) (Graph.graphSchemaTypes g)
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
                    (hexDigit (Maybes.fromMaybe 0 (Math.maybeDiv b 16))),
                    (hexDigit (Maybes.fromMaybe 0 (Math.maybeMod b 16)))]
          go =
                  \inStr -> \esc -> \bytes -> Maybes.maybe [] (\uc ->
                    let b = Pairs.first uc
                        bs = Pairs.second uc
                    in (Logic.ifElse esc (Lists.cons b (go inStr False bs)) (Logic.ifElse (Logic.and (Equality.equal b 92) inStr) (Lists.cons b (go inStr True bs)) (Logic.ifElse (Equality.equal b 34) (Lists.cons b (go (Logic.not inStr) False bs)) (Logic.ifElse (Logic.and inStr (Equality.lt b 32)) (Lists.concat2 (escapeToUnicode b) (go inStr False bs)) (Lists.cons b (go inStr False bs))))))) (Lists.uncons bytes)
      in (go False False input)
-- | Format a primitive for the lexicon
formatPrimitive :: Graph.Primitive -> String
formatPrimitive prim =

      let name = Core.unName (Packaging.primitiveDefinitionName (Graph.primitiveDefinition prim))
          typ = Scoping.termSignatureToTypeScheme (Packaging.primitiveDefinitionSignature (Graph.primitiveDefinition prim))
          typeStr = ShowCore.typeScheme typ
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)
-- | Format a term binding for the lexicon
formatTermBinding :: Core.Binding -> String
formatTermBinding binding =

      let name = Core.unName (Core.bindingName binding)
          typeStr = Maybes.maybe "?" (\scheme -> ShowCore.typeScheme scheme) (Core.bindingTypeScheme binding)
      in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)
-- | Format a type binding for the lexicon
formatTypeBinding :: Graph.Graph -> Core.Binding -> Either Errors.Error String
formatTypeBinding graph binding =
    Eithers.bind (Eithers.bimap (\_e -> Errors.ErrorDecoding _e) (\_a -> _a) (DecodeCore.type_ graph (Core.bindingTerm binding))) (\typ -> Right (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " (Core.unName (Core.bindingName binding))) " = ") (ShowCore.type_ typ)))
-- | Generate encoder or decoder modules for a list of type modules
generateCoderModules :: (t0 -> Graph.Graph -> t1 -> Either t2 (Maybe t3)) -> Graph.Graph -> [Packaging.Module] -> [t1] -> t0 -> Either t2 [t3]
generateCoderModules codec bsGraph universeModules typeModules cx =

      let universe = Maps.fromList (Lists.map (\m -> (Packaging.moduleName m, m)) (Lists.concat2 universeModules universeModules))
          closureModules = moduleDepsTransitive universe universeModules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.keyType, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingTypeScheme = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureModules)
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureModules)
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
          sortedPrimitives = Lists.sortOn (\p -> Packaging.primitiveDefinitionName (Graph.primitiveDefinition p)) primitives
          sortedTypes = Lists.sortOn (\b -> Core.bindingName b) typeBindings
          sortedTerms = Lists.sortOn (\b -> Core.bindingName b) termBindings
      in (Eithers.bind (Eithers.mapList (\b -> formatTypeBinding graph b) sortedTypes) (\typeLines ->
        let termLines = Lists.map (\b -> formatTermBinding b) sortedTerms
            primitiveLines = Lists.map (\p -> formatPrimitive p) sortedPrimitives
        in (Right (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "Primitives:\n" (Strings.unlines primitiveLines)) "\nTypes:\n") (Strings.unlines typeLines)) "\nTerms:\n") (Strings.unlines termLines)))))
-- | Pure core of code generation: given a coder, language, flags, bootstrap graph, universe, and modules to generate, produce a list of (filePath, content) pairs.
generateSourceFiles :: Ord t0 => ((Packaging.Module -> [Packaging.Definition] -> Typing.InferenceContext -> Graph.Graph -> Either Errors.Error (M.Map t0 t1)) -> Coders.Language -> Bool -> Bool -> Bool -> Bool -> Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Typing.InferenceContext -> Either Errors.Error [(t0, t1)])
generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bsGraph universeModules modsToGenerate cx =

      let namespaceMap = Maps.fromList (Lists.map (\m -> (Packaging.moduleName m, m)) (Lists.concat2 universeModules modsToGenerate))
          constraints = Coders.languageConstraints lang
          typeModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.keyType, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingTypeScheme = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions mod))))) modsToGenerate
          termModulesToGenerate =
                  Lists.filter (\mod -> Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions mod))))) modsToGenerate
          closureMods = moduleDepsTransitive namespaceMap modsToGenerate
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.keyType, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingTypeScheme = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureMods)
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureMods)
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
        let namespaces = Lists.map (\m -> Packaging.moduleName m) termModulesToGenerate
        in (Eithers.bind (Adapt.dataGraphToDefinitions constraints doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings dataElements dataGraph namespaces cx) (\dataResult ->
          let g1 = Pairs.first dataResult
              defLists = Pairs.second dataResult
              defName =
                      \d -> case d of
                        Packaging.DefinitionTerm v0 -> Packaging.termDefinitionName v0
                        Packaging.DefinitionType v0 -> Packaging.typeDefinitionName v0
                        Packaging.DefinitionPrimitive v0 -> Packaging.primitiveDefinitionName v0
              refreshModule =
                      \els -> \m -> Packaging.Module {
                        Packaging.moduleDescription = (Packaging.moduleDescription m),
                        Packaging.moduleName = (Packaging.moduleName m),
                        Packaging.moduleDependencies = (Packaging.moduleDependencies m),
                        Packaging.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
                          Packaging.DefinitionType v0 -> Just (Packaging.DefinitionType v0)
                          Packaging.DefinitionTerm v0 -> Maybes.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                            Packaging.termDefinitionName = (Core.bindingName b),
                            Packaging.termDefinitionTerm = (Core.bindingTerm b),
                            Packaging.termDefinitionSignature = (Maybes.map Scoping.typeSchemeToTermSignature (Core.bindingTypeScheme b))})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging.termDefinitionName v0)) els)
                          Packaging.DefinitionPrimitive v0 -> Just (Packaging.DefinitionPrimitive v0)) (Packaging.moduleDefinitions m)))}
              allBindings = Lexical.graphToBindings g1
              refreshedMods = Lists.map (\m -> refreshModule allBindings m) termModulesToGenerate
              dedupDefs = \defs -> Maps.elems (Maps.fromList (Lists.map (\d -> (Packaging.termDefinitionName d, d)) defs))
              dedupedDefLists = Lists.map dedupDefs defLists
          in (Eithers.map (\xs -> Lists.concat xs) (Eithers.mapList (\p ->
            let mod = Pairs.first p
                defs = Pairs.second p
            in (Eithers.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Packaging.DefinitionTerm d) defs) cx g1))) (Lists.zip refreshedMods dedupedDefLists))))))) (\termFiles -> Right (Lists.concat2 schemaFiles termFiles))))
-- | Perform type inference and generate the lexicon for a set of modules
inferAndGenerateLexicon :: Typing.InferenceContext -> Graph.Graph -> [Packaging.Module] -> Either Errors.Error String
inferAndGenerateLexicon cx bsGraph kernelModules =

      let g0 = modulesToGraph bsGraph kernelModules kernelModules
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) kernelModules)
      in (Eithers.bind (Inference.inferGraphTypes cx dataElements g0) (\inferResultWithCx ->
        let g1 = Pairs.first (Pairs.first inferResultWithCx)
        in (generateLexicon g1)))
-- | Perform type inference on modules and reconstruct with inferred types
inferModules :: Typing.InferenceContext -> Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Either Errors.Error [Packaging.Module]
inferModules cx bsGraph universeMods targetMods =

      let g0 = modulesToGraph bsGraph universeMods universeMods
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) universeMods)
      in (Eithers.bind (Inference.inferGraphTypes cx dataElements g0) (\inferResultWithCx ->
        let inferResult = Pairs.first inferResultWithCx
            inferredElements = Pairs.second inferResult
        in (Right (Lists.map (refreshModule inferredElements) targetMods))))
-- | Infer types for target modules in the context of a typed universe
inferModulesGiven :: Typing.InferenceContext -> Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Either Errors.Error [Packaging.Module]
inferModulesGiven cx bsGraph universeMods targetMods =

      let g0 = modulesToGraph bsGraph universeMods universeMods
          nsMap = Maps.fromList (Lists.map (\m -> (Packaging.moduleName m, m)) universeMods)
          closureMods = moduleDepsTransitive nsMap targetMods
          targetNamespaces = Sets.fromList (Lists.map Packaging.moduleName targetMods)
          termBindings =
                  Lists.concat (Lists.map (\m ->
                    let isTarget = Sets.member (Packaging.moduleName m) targetNamespaces
                        bs =
                                Maybes.cat (Lists.map (\d -> case d of
                                  Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                                    Core.bindingName = (Packaging.termDefinitionName v0),
                                    Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                                    Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                                  _ -> Nothing) (Packaging.moduleDefinitions m))
                    in (Logic.ifElse isTarget bs (Lists.filter (\b -> Maybes.isNothing (Core.bindingTypeScheme b)) bs))) closureMods)
          primitiveBindings =
                  Lists.concat (Lists.map (\m ->
                    let isTarget = Sets.member (Packaging.moduleName m) targetNamespaces
                        bs =
                                Maybes.cat (Lists.map (\d -> case d of
                                  Packaging.DefinitionPrimitive v0 -> Maybes.map (\impl -> Core.Binding {
                                    Core.bindingName = (Packaging.primitiveDefinitionName v0),
                                    Core.bindingTerm = impl,
                                    Core.bindingTypeScheme = (Just (Scoping.termSignatureToTypeScheme (Packaging.primitiveDefinitionSignature v0)))}) (Packaging.primitiveDefinitionDefaultImplementation v0)
                                  _ -> Nothing) (Packaging.moduleDefinitions m))
                    in (Logic.ifElse isTarget bs [])) closureMods)
          bindingsToInfer = Lists.concat2 termBindings primitiveBindings
          untouchedTypedBindings =
                  Lists.concat (Lists.map (\m ->
                    let isTarget = Sets.member (Packaging.moduleName m) targetNamespaces
                        bs =
                                Maybes.cat (Lists.map (\d -> case d of
                                  Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                                    Core.bindingName = (Packaging.termDefinitionName v0),
                                    Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                                    Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                                  _ -> Nothing) (Packaging.moduleDefinitions m))
                    in (Logic.ifElse isTarget [] (Lists.filter (\b -> Maybes.isJust (Core.bindingTypeScheme b)) bs))) closureMods)
      in (Eithers.bind (Inference.inferGraphTypes cx bindingsToInfer g0) (\inferResultWithCx ->
        let inferResult = Pairs.first inferResultWithCx
            newlyInferredBindings = Pairs.second inferResult
            allInferredBindings = Lists.concat2 newlyInferredBindings untouchedTypedBindings
        in (Right (Lists.map (refreshModule allInferredBindings) targetMods))))
-- | Lower Definition.primitive arms to Definition.term arms with term-encoded PrimitiveDefinition
lowerPrimitiveDefinitions :: Packaging.Module -> Packaging.Module
lowerPrimitiveDefinitions m =

      let pkgNs = Packaging.ModuleName "hydra.packaging"
          coreNs = Packaging.ModuleName "hydra.core"
          primDefSig =
                  Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                    Core.typeSchemeVariables = [],
                    Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.packaging.PrimitiveDefinition")),
                    Core.typeSchemeConstraints = Nothing})
          origDefs = Packaging.moduleDefinitions m
          hasPrim =
                  Lists.foldl (\acc -> \d -> Logic.or acc (case d of
                    Packaging.DefinitionPrimitive _ -> True
                    _ -> False)) False origDefs
      in (Logic.ifElse (Logic.not hasPrim) m (
        let newDefs =
                Lists.map (\d -> case d of
                  Packaging.DefinitionPrimitive v0 -> Packaging.DefinitionTerm (Packaging.TermDefinition {
                    Packaging.termDefinitionName = (Packaging.primitiveDefinitionName v0),
                    Packaging.termDefinitionTerm = (EncodePackaging.primitiveDefinition v0),
                    Packaging.termDefinitionSignature = (Just primDefSig)})
                  _ -> d) origDefs
            currentDeps = Packaging.moduleDependencies m
            filteredDeps =
                    Lists.filter (\dep -> Logic.and (Logic.not (Equality.equal (Packaging.moduleDependencyModule dep) pkgNs)) (Logic.not (Equality.equal (Packaging.moduleDependencyModule dep) coreNs))) currentDeps
            newDeps =
                    Lists.concat2 filteredDeps [
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = pkgNs,
                        Packaging.moduleDependencyPackage = Nothing},
                      Packaging.ModuleDependency {
                        Packaging.moduleDependencyModule = coreNs,
                        Packaging.moduleDependencyPackage = Nothing}]
        in Packaging.Module {
          Packaging.moduleDescription = (Packaging.moduleDescription m),
          Packaging.moduleName = (Packaging.moduleName m),
          Packaging.moduleDependencies = newDeps,
          Packaging.moduleDefinitions = newDefs}))
-- | Compute transitive closure of dependencies for a set of modules
moduleDepsTransitive :: M.Map Packaging.ModuleName Packaging.Module -> [Packaging.Module] -> [Packaging.Module]
moduleDepsTransitive nsMap modules =

      let closure =
              Sets.union (transitiveDeps (\m -> Lists.map (\dep -> Packaging.moduleDependencyModule dep) (Packaging.moduleDependencies m)) nsMap modules) (Sets.fromList (Lists.map (\m -> Packaging.moduleName m) modules))
      in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) (Sets.toList closure)))
-- | Convert a module name to a file path (e.g., hydra.core -> hydra/core)
moduleNameToPath :: Packaging.ModuleName -> String
moduleNameToPath ns = Strings.intercalate "/" (Strings.splitOn "." (Packaging.unModuleName ns))
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
              Packaging.ModuleName (Strings.cat2 "hydra.sources." (Strings.intercalate "." (Lists.drop 1 (Strings.splitOn "." (Packaging.unModuleName (Packaging.moduleName m))))))
          modTypeNs = Packaging.ModuleName "hydra.packaging"
          moduleDef =
                  Packaging.DefinitionTerm (Packaging.TermDefinition {
                    Packaging.termDefinitionName = (Core.Name (Strings.cat2 (Packaging.unModuleName sourceNs) ".module_")),
                    Packaging.termDefinitionTerm = (EncodePackaging.module_ m),
                    Packaging.termDefinitionSignature = (Just (Scoping.typeSchemeToTermSignature (Core.TypeScheme {
                      Core.typeSchemeVariables = [],
                      Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.packaging.Module")),
                      Core.typeSchemeConstraints = Nothing})))})
      in Packaging.Module {
        Packaging.moduleDescription = (Just (Strings.cat2 "Source module for " (Packaging.unModuleName (Packaging.moduleName m)))),
        Packaging.moduleName = sourceNs,
        Packaging.moduleDependencies = [
          Packaging.ModuleDependency {
            Packaging.moduleDependencyModule = modTypeNs,
            Packaging.moduleDependencyPackage = Nothing}],
        Packaging.moduleDefinitions = [
          moduleDef]}
-- | Build a graph from universe modules and working modules, using an explicit bootstrap graph
modulesToGraph :: Graph.Graph -> [Packaging.Module] -> [Packaging.Module] -> Graph.Graph
modulesToGraph bsGraph universeModules modules =

      let universe = Maps.fromList (Lists.map (\m -> (Packaging.moduleName m, m)) (Lists.concat2 universeModules modules))
          closureModules = moduleDepsTransitive universe modules
          schemaElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionType v0 -> Just ((\name -> \typ ->
                      let schemaTerm = Core.TermVariable (Core.Name "hydra.core.Type")
                          dataTerm =
                                  Annotations.normalizeTermAnnotations (Core.TermAnnotated (Core.AnnotatedTerm {
                                    Core.annotatedTermBody = (EncodeCore.type_ typ),
                                    Core.annotatedTermAnnotation = (Maps.fromList [
                                      (Constants.keyType, schemaTerm)])}))
                      in Core.Binding {
                        Core.bindingName = name,
                        Core.bindingTerm = dataTerm,
                        Core.bindingTypeScheme = (Just (Core.TypeScheme {
                          Core.typeSchemeVariables = [],
                          Core.typeSchemeBody = (Core.TypeVariable (Core.Name "hydra.core.Type")),
                          Core.typeSchemeConstraints = Nothing}))}) (Packaging.typeDefinitionName v0) (Core.typeSchemeBody (Packaging.typeDefinitionTypeScheme v0)))
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureModules)
          dataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) closureModules)
          schemaGraph = Lexical.elementsToGraph bsGraph Maps.empty schemaElements
          schemaTypes = Eithers.either (\_ -> Maps.empty) (\_r -> _r) (Environment.schemaGraphToTypingEnvironment schemaGraph)
          baseGraph = Lexical.elementsToGraph bsGraph schemaTypes dataElements
          universeDataElements =
                  Lists.concat (Lists.map (\m -> Maybes.cat (Lists.map (\d -> case d of
                    Packaging.DefinitionTerm v0 -> Just (Core.Binding {
                      Core.bindingName = (Packaging.termDefinitionName v0),
                      Core.bindingTerm = (Packaging.termDefinitionTerm v0),
                      Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
                    _ -> Nothing) (Packaging.moduleDefinitions m))) universeModules)
          universeBoundTypes =
                  Maps.fromList (Maybes.cat (Lists.map (\b -> Maybes.map (\ts -> (Core.bindingName b, ts)) (Core.bindingTypeScheme b)) universeDataElements))
      in Graph.Graph {
        Graph.graphBoundTerms = (Graph.graphBoundTerms baseGraph),
        Graph.graphBoundTypes = universeBoundTypes,
        Graph.graphClassConstraints = (Graph.graphClassConstraints baseGraph),
        Graph.graphLambdaVariables = (Graph.graphLambdaVariables baseGraph),
        Graph.graphMetadata = (Graph.graphMetadata baseGraph),
        Graph.graphPrimitives = (Graph.graphPrimitives baseGraph),
        Graph.graphSchemaTypes = (Graph.graphSchemaTypes baseGraph),
        Graph.graphTypeVariables = (Graph.graphTypeVariables baseGraph)}
-- | Rebuild a module's term definitions using freshly inferred bindings
refreshModule :: [Core.Binding] -> Packaging.Module -> Packaging.Module
refreshModule inferredElements m =
    Logic.ifElse (Logic.not (Logic.not (Lists.null (Maybes.cat (Lists.map (\d -> case d of
      Packaging.DefinitionTerm v0 -> Just (Core.Binding {
        Core.bindingName = (Packaging.termDefinitionName v0),
        Core.bindingTerm = (Packaging.termDefinitionTerm v0),
        Core.bindingTypeScheme = (Maybes.map Scoping.termSignatureToTypeScheme (Packaging.termDefinitionSignature v0))})
      _ -> Nothing) (Packaging.moduleDefinitions m)))))) m (Packaging.Module {
      Packaging.moduleDescription = (Packaging.moduleDescription m),
      Packaging.moduleName = (Packaging.moduleName m),
      Packaging.moduleDependencies = (Packaging.moduleDependencies m),
      Packaging.moduleDefinitions = (Maybes.cat (Lists.map (\d -> case d of
        Packaging.DefinitionType v0 -> Just (Packaging.DefinitionType v0)
        Packaging.DefinitionTerm v0 -> Maybes.map (\b -> Packaging.DefinitionTerm (Packaging.TermDefinition {
          Packaging.termDefinitionName = (Core.bindingName b),
          Packaging.termDefinitionTerm = (Core.bindingTerm b),
          Packaging.termDefinitionSignature = (Maybes.map Scoping.typeSchemeToTermSignature (Core.bindingTypeScheme b))})) (Lists.find (\b -> Equality.equal (Core.bindingName b) (Packaging.termDefinitionName v0)) inferredElements)
        Packaging.DefinitionPrimitive v0 -> Just (Packaging.DefinitionPrimitive v0)) (Packaging.moduleDefinitions m)))})
-- | Compute transitive closure of module dependencies
transitiveDeps :: (Packaging.Module -> [Packaging.ModuleName]) -> M.Map Packaging.ModuleName Packaging.Module -> [Packaging.Module] -> S.Set Packaging.ModuleName
transitiveDeps getDeps nsMap startMods =

      let initialDeps =
              Sets.fromList (Lists.concat (Lists.map (\m -> Lists.filter (\dep -> Logic.not (Equality.equal dep (Packaging.moduleName m))) (getDeps m)) startMods))
          go =
                  \pending -> \visited -> Logic.ifElse (Sets.null pending) visited (
                    let newVisited = Sets.union visited pending
                        nextDeps =
                                Sets.fromList (Lists.concat (Lists.map (\nsv -> Maybes.maybe [] (\depMod -> getDeps depMod) (Maps.lookup nsv nsMap)) (Sets.toList pending)))
                        newPending = Sets.difference nextDeps newVisited
                    in (go newPending newVisited))
      in (go initialDeps Sets.empty)
