-- Note: this is an automatically generated file. Do not edit.

-- | Pure code generation pipeline for bootstrapping Hydra across languages.

module Hydra.CodeGeneration where

import qualified Hydra.Adapt.Simple as Simple
import qualified Hydra.Annotations as Annotations
import qualified Hydra.Coders as Coders
import qualified Hydra.Compute as Compute
import qualified Hydra.Core as Core
import qualified Hydra.Decode.Core as Core_
import qualified Hydra.Decode.Module as Module
import qualified Hydra.Encode.Module as Module_
import qualified Hydra.Graph as Graph
import qualified Hydra.Inference as Inference
import qualified Hydra.Json.Decode as Decode
import qualified Hydra.Json.Encode as Encode
import qualified Hydra.Json.Model as Model
import qualified Hydra.Json.Writer as Writer
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Eithers as Eithers
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Module as Module__
import qualified Hydra.Monads as Monads
import qualified Hydra.Rewriting as Rewriting
import qualified Hydra.Schemas as Schemas
import qualified Hydra.Show.Core as Core__
import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Convert a namespace to a file path (e.g., hydra.core -> hydra/core)
namespaceToPath :: (Module__.Namespace -> String)
namespaceToPath ns = (Strings.intercalate "/" (Strings.splitOn "." (Module__.unNamespace ns)))

-- | Strip TypeSchemes from term bindings in a module, preserving type binding TypeSchemes. JSON-loaded modules carry inferred TypeSchemes from the original compilation. After adaptation (e.g., bigfloat -> float64), these TypeSchemes become stale and can cause inference errors. Stripping them allows the inference engine to reconstruct correct TypeSchemes from scratch.
stripModuleTypeSchemes :: (Module__.Module -> Module__.Module)
stripModuleTypeSchemes m =  
  let stripIfTerm = (\b -> Logic.ifElse (Annotations.isNativeType b) b (Core.Binding {
          Core.bindingName = (Core.bindingName b),
          Core.bindingTerm = (Core.bindingTerm b),
          Core.bindingType = Nothing}))
  in Module__.Module {
    Module__.moduleNamespace = (Module__.moduleNamespace m),
    Module__.moduleElements = (Lists.map stripIfTerm (Module__.moduleElements m)),
    Module__.moduleTermDependencies = (Module__.moduleTermDependencies m),
    Module__.moduleTypeDependencies = (Module__.moduleTypeDependencies m),
    Module__.moduleDescription = (Module__.moduleDescription m)}

-- | Compute transitive closure of module dependencies
transitiveDeps :: ((Module__.Module -> [Module__.Namespace]) -> M.Map Module__.Namespace Module__.Module -> [Module__.Module] -> S.Set Module__.Namespace)
transitiveDeps getDeps nsMap startMods =  
  let initialDeps = (Sets.fromList (Lists.concat (Lists.map (\m -> Lists.filter (\dep -> Logic.not (Equality.equal dep (Module__.moduleNamespace m))) (getDeps m)) startMods)))
  in  
    let go = (\pending -> \visited -> Logic.ifElse (Sets.null pending) visited ( 
            let newVisited = (Sets.union visited pending)
            in  
              let nextDeps = (Sets.fromList (Lists.concat (Lists.map (\nsv -> Maybes.maybe [] (\depMod -> getDeps depMod) (Maps.lookup nsv nsMap)) (Sets.toList pending))))
              in  
                let newPending = (Sets.difference nextDeps newVisited)
                in (go newPending newVisited)))
    in (go initialDeps Sets.empty)

-- | Compute transitive closure of term dependencies for a set of modules
moduleTermDepsTransitive :: (M.Map Module__.Namespace Module__.Module -> [Module__.Module] -> [Module__.Module])
moduleTermDepsTransitive nsMap modules =  
  let closure = (Sets.union (transitiveDeps (\m -> Module__.moduleTermDependencies m) nsMap modules) (Sets.fromList (Lists.map (\m -> Module__.moduleNamespace m) modules)))
  in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) (Sets.toList closure)))

-- | Compute transitive closure of type dependencies for a set of modules
moduleTypeDepsTransitive :: (M.Map Module__.Namespace Module__.Module -> [Module__.Module] -> [Module__.Module])
moduleTypeDepsTransitive nsMap modules =  
  let termMods = (moduleTermDepsTransitive nsMap modules)
  in  
    let typeNamespaces = (Sets.toList (transitiveDeps (\m -> Module__.moduleTypeDependencies m) nsMap termMods))
    in (Maybes.cat (Lists.map (\n -> Maps.lookup n nsMap) typeNamespaces))

-- | Build a graph from universe modules and working modules, using an explicit bootstrap graph
modulesToGraph :: (Graph.Graph -> [Module__.Module] -> [Module__.Module] -> Graph.Graph)
modulesToGraph bsGraph universeModules modules =  
  let universe = (Maps.fromList (Lists.map (\m -> (Module__.moduleNamespace m, m)) (Lists.concat2 universeModules modules)))
  in  
    let schemaModules = (moduleTypeDepsTransitive universe modules)
    in  
      let dataModules = (moduleTermDepsTransitive universe modules)
      in  
        let schemaElements = (Lists.filter (\e -> Annotations.isNativeType e) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) (Lists.concat2 schemaModules modules))))
        in  
          let dataElements = (Lists.filter (\e -> Logic.not (Annotations.isNativeType e)) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) dataModules)))
          in  
            let schemaGraph = (Lexical.elementsToGraph bsGraph Maps.empty schemaElements)
            in  
              let schemaTypes = (Monads.fromFlow Maps.empty schemaGraph (Schemas.schemaGraphToTypingEnvironment schemaGraph))
              in (Lexical.elementsToGraph bsGraph schemaTypes dataElements)

-- | Pure core of code generation: given a coder, language, flags, bootstrap graph, universe, and modules to generate, produce a list of (filePath, content) pairs.
generateSourceFiles :: Ord t0 => ((Module__.Module -> [Module__.Definition] -> Compute.Flow Graph.Graph (M.Map t0 t1)) -> Coders.Language -> Bool -> Bool -> Bool -> Bool -> Graph.Graph -> [Module__.Module] -> [Module__.Module] -> Compute.Flow Graph.Graph [(t0, t1)])
generateSourceFiles printDefinitions lang doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings bsGraph universeModules modsToGenerate =  
  let namespaceMap = (Maps.fromList (Lists.map (\m -> (Module__.moduleNamespace m, m)) (Lists.concat2 universeModules modsToGenerate)))
  in  
    let constraints = (Coders.languageConstraints lang)
    in  
      let isTypeModule = (\mod -> Logic.not (Lists.null (Lists.filter (\e -> Annotations.isNativeType e) (Module__.moduleElements mod))))
      in  
        let partitioned = (Lists.partition isTypeModule modsToGenerate)
        in  
          let typeModulesToGenerate = (Pairs.first partitioned)
          in  
            let termModulesToGenerate = (Pairs.second partitioned)
            in  
              let schemaMods = (moduleTypeDepsTransitive namespaceMap modsToGenerate)
              in  
                let schemaElements = (Lists.filter (\e -> Annotations.isNativeType e) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) (Lists.concat2 schemaMods typeModulesToGenerate))))
                in  
                  let dataMods = (moduleTermDepsTransitive namespaceMap modsToGenerate)
                  in  
                    let dataElements = (Lists.concat (Lists.map (\m -> Module__.moduleElements m) dataMods))
                    in  
                      let schemaGraph = (Lexical.elementsToGraph bsGraph Maps.empty schemaElements)
                      in  
                        let schemaTypes2 = (Monads.fromFlow Maps.empty schemaGraph (Schemas.schemaGraphToTypingEnvironment schemaGraph))
                        in  
                          let dataGraph = (Lexical.elementsToGraph bsGraph schemaTypes2 dataElements)
                          in (Flows.bind (Logic.ifElse (Lists.null typeModulesToGenerate) (Flows.pure []) (Monads.withTrace "generate type modules" ( 
                            let nameLists = (Lists.map (\m -> Lists.map (\e -> Core.bindingName e) (Lists.filter (\e -> Annotations.isNativeType e) (Module__.moduleElements m))) typeModulesToGenerate)
                            in (Flows.bind (Simple.schemaGraphToDefinitions constraints schemaGraph nameLists) (\schemaResult ->  
                              let defLists = (Pairs.second schemaResult)
                              in (Monads.withState (Graph.Graph {
                                Graph.graphBoundTerms = (Graph.graphBoundTerms schemaGraph),
                                Graph.graphBoundTypes = (Graph.graphBoundTypes schemaGraph),
                                Graph.graphClassConstraints = (Graph.graphClassConstraints schemaGraph),
                                Graph.graphLambdaVariables = (Graph.graphLambdaVariables schemaGraph),
                                Graph.graphMetadata = (Graph.graphMetadata schemaGraph),
                                Graph.graphPrimitives = (Graph.graphPrimitives schemaGraph),
                                Graph.graphSchemaTypes = schemaTypes2,
                                Graph.graphTypeVariables = (Graph.graphTypeVariables schemaGraph)}) (Flows.map (\xs -> Lists.concat xs) (Flows.mapList (\p ->  
                                let mod = (Pairs.first p)
                                in  
                                  let defs = (Pairs.second p)
                                  in (Monads.withTrace (Strings.cat2 "type module " (Module__.unNamespace (Module__.moduleNamespace mod))) (Flows.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Module__.DefinitionType d) defs))))) (Lists.zip typeModulesToGenerate defLists))))))))) (\schemaFiles -> Flows.bind (Logic.ifElse (Lists.null termModulesToGenerate) (Flows.pure []) (Monads.withTrace "generate term modules" ( 
                            let namespaces = (Lists.map (\m -> Module__.moduleNamespace m) termModulesToGenerate)
                            in (Flows.bind (Simple.dataGraphToDefinitions constraints doInfer doExpand doHoistCaseStatements doHoistPolymorphicLetBindings dataElements dataGraph namespaces) (\dataResult ->  
                              let g1 = (Pairs.first dataResult)
                              in  
                                let defLists = (Pairs.second dataResult)
                                in  
                                  let refreshModule = (\els -> \m -> Module__.Module {
                                          Module__.moduleNamespace = (Module__.moduleNamespace m),
                                          Module__.moduleElements = (Maybes.cat (Lists.map (\e -> Lists.find (\b -> Equality.equal (Core.bindingName b) (Core.bindingName e)) els) (Module__.moduleElements m))),
                                          Module__.moduleTermDependencies = (Module__.moduleTermDependencies m),
                                          Module__.moduleTypeDependencies = (Module__.moduleTypeDependencies m),
                                          Module__.moduleDescription = (Module__.moduleDescription m)})
                                  in  
                                    let refreshedMods = (Lists.map (\m -> refreshModule (Lexical.graphToBindings g1) m) termModulesToGenerate)
                                    in (Monads.withState g1 (Flows.map (\xs -> Lists.concat xs) (Flows.mapList (\p ->  
                                      let mod = (Pairs.first p)
                                      in  
                                        let defs = (Pairs.second p)
                                        in (Monads.withTrace (Strings.cat2 "term module " (Module__.unNamespace (Module__.moduleNamespace mod))) (Flows.map (\m -> Maps.toList m) (printDefinitions mod (Lists.map (\d -> Module__.DefinitionTerm d) defs))))) (Lists.zip refreshedMods defLists))))))))) (\termFiles -> Flows.pure (Lists.concat2 schemaFiles termFiles))))

-- | Format a term binding for the lexicon
formatTermBinding :: (Core.Binding -> String)
formatTermBinding binding =  
  let name = (Core.unName (Core.bindingName binding))
  in  
    let typeStr = (Maybes.maybe "?" (\scheme -> Core__.typeScheme scheme) (Core.bindingType binding))
    in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a primitive for the lexicon
formatPrimitive :: (Graph.Primitive -> String)
formatPrimitive prim =  
  let name = (Core.unName (Graph.primitiveName prim))
  in  
    let typeStr = (Core__.typeScheme (Graph.primitiveType prim))
    in (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " name) " : ") typeStr)

-- | Format a type binding for the lexicon
formatTypeBinding :: (Core.Binding -> Compute.Flow Graph.Graph String)
formatTypeBinding binding = (Flows.bind Monads.getState (\graph -> Flows.bind (Monads.eitherToFlow Util.unDecodingError (Core_.type_ graph (Core.bindingTerm binding))) (\typ -> Flows.pure (Strings.cat2 (Strings.cat2 (Strings.cat2 "  " (Core.unName (Core.bindingName binding))) " = ") (Core__.type_ typ)))))

-- | Build a schema map (Name -> Type) from a graph's schema types
buildSchemaMap :: (Graph.Graph -> M.Map Core.Name Core.Type)
buildSchemaMap g = (Maps.map (\ts -> Rewriting.deannotateType (Core.typeSchemeType ts)) (Graph.graphSchemaTypes g))

-- | Convert a generated Module into a Source module
moduleToSourceModule :: (Module__.Module -> Module__.Module)
moduleToSourceModule m =  
  let sourceNs = (Module__.Namespace (Strings.cat2 "hydra.sources." (Strings.intercalate "." (Lists.drop 1 (Strings.splitOn "." (Module__.unNamespace (Module__.moduleNamespace m)))))))
  in  
    let modTypeNs = (Module__.Namespace "hydra.module")
    in  
      let moduleBinding = Core.Binding {
              Core.bindingName = (Core.Name (Strings.cat2 (Module__.unNamespace sourceNs) ".module_")),
              Core.bindingTerm = (Module_.module_ m),
              Core.bindingType = Nothing}
      in Module__.Module {
        Module__.moduleNamespace = sourceNs,
        Module__.moduleElements = [
          moduleBinding],
        Module__.moduleTermDependencies = [
          modTypeNs],
        Module__.moduleTypeDependencies = [
          modTypeNs],
        Module__.moduleDescription = (Just (Strings.cat2 "Source module for " (Module__.unNamespace (Module__.moduleNamespace m))))}

-- | Generate the lexicon content from a graph
generateLexicon :: (Graph.Graph -> Compute.Flow Graph.Graph String)
generateLexicon graph =  
  let bindings = (Lexical.graphToBindings graph)
  in  
    let primitives = (Maps.elems (Graph.graphPrimitives graph))
    in  
      let partitioned = (Lists.partition (\b -> Annotations.isNativeType b) bindings)
      in  
        let typeBindings = (Pairs.first partitioned)
        in  
          let termBindings = (Pairs.second partitioned)
          in  
            let sortedPrimitives = (Lists.sortOn (\p -> Graph.primitiveName p) primitives)
            in  
              let sortedTypes = (Lists.sortOn (\b -> Core.bindingName b) typeBindings)
              in  
                let sortedTerms = (Lists.sortOn (\b -> Core.bindingName b) termBindings)
                in (Flows.bind (Flows.mapList (\b -> formatTypeBinding b) sortedTypes) (\typeLines ->  
                  let termLines = (Lists.map (\b -> formatTermBinding b) sortedTerms)
                  in  
                    let primitiveLines = (Lists.map (\p -> formatPrimitive p) sortedPrimitives)
                    in (Flows.pure (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 (Strings.cat2 "Primitives:\n" (Strings.unlines primitiveLines)) "\nTypes:\n") (Strings.unlines typeLines)) "\nTerms:\n") (Strings.unlines termLines)))))

-- | Convert a Module to a JSON string
moduleToJson :: (Module__.Module -> Either String String)
moduleToJson m =  
  let term = (Module_.module_ m)
  in (Eithers.map (\json -> Writer.printJson json) (Encode.toJson term))

-- | Perform type inference on modules and reconstruct with inferred types
inferModules :: (Graph.Graph -> [Module__.Module] -> [Module__.Module] -> Compute.Flow t0 [Module__.Module])
inferModules bsGraph universeMods targetMods =  
  let g0 = (modulesToGraph bsGraph universeMods universeMods)
  in  
    let dataElements = (Lists.filter (\e -> Logic.not (Annotations.isNativeType e)) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) universeMods)))
    in (Flows.bind (Inference.inferGraphTypes dataElements g0) (\inferResult ->  
      let g1 = (Pairs.first inferResult)
      in  
        let inferredElements = (Pairs.second inferResult)
        in  
          let isTypeModule = (\mod -> Lists.null (Lists.filter (\e -> Logic.not (Annotations.isNativeType e)) (Module__.moduleElements mod)))
          in  
            let refreshModule = (\m -> Logic.ifElse (isTypeModule m) m (Module__.Module {
                    Module__.moduleNamespace = (Module__.moduleNamespace m),
                    Module__.moduleElements = (Maybes.cat (Lists.map (\e -> Lists.find (\b -> Equality.equal (Core.bindingName b) (Core.bindingName e)) inferredElements) (Module__.moduleElements m))),
                    Module__.moduleTermDependencies = (Module__.moduleTermDependencies m),
                    Module__.moduleTypeDependencies = (Module__.moduleTypeDependencies m),
                    Module__.moduleDescription = (Module__.moduleDescription m)}))
            in (Flows.pure (Lists.map refreshModule targetMods))))

-- | Generate encoder or decoder modules for a list of type modules
generateCoderModules :: ((t0 -> Compute.Flow Graph.Graph (Maybe t1)) -> Graph.Graph -> [Module__.Module] -> [t0] -> Compute.Flow t2 [t1])
generateCoderModules codec bsGraph universeModules typeModules =  
  let universe = (Maps.fromList (Lists.map (\m -> (Module__.moduleNamespace m, m)) (Lists.concat2 universeModules universeModules)))
  in  
    let schemaModules = (moduleTypeDepsTransitive universe universeModules)
    in  
      let dataModules = (moduleTermDepsTransitive universe universeModules)
      in  
        let schemaElements = (Lists.filter (\e -> Annotations.isNativeType e) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) (Lists.concat2 schemaModules universeModules))))
        in  
          let dataElements = (Lists.filter (\e -> Logic.not (Annotations.isNativeType e)) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) dataModules)))
          in  
            let schemaGraph = (Lexical.elementsToGraph bsGraph Maps.empty schemaElements)
            in  
              let schemaTypes = (Monads.fromFlow Maps.empty schemaGraph (Schemas.schemaGraphToTypingEnvironment schemaGraph))
              in  
                let allElements = (Lists.concat2 schemaElements dataElements)
                in  
                  let graph = (Lexical.elementsToGraph bsGraph schemaTypes allElements)
                  in (Monads.withState graph (Flows.map (\results -> Maybes.cat results) (Flows.mapList codec typeModules)))

-- | Perform type inference and generate the lexicon for a set of modules
inferAndGenerateLexicon :: (Graph.Graph -> [Module__.Module] -> Compute.Flow t0 String)
inferAndGenerateLexicon bsGraph kernelModules =  
  let g0 = (modulesToGraph bsGraph kernelModules kernelModules)
  in  
    let dataElements = (Lists.filter (\e -> Logic.not (Annotations.isNativeType e)) (Lists.concat (Lists.map (\m -> Module__.moduleElements m) kernelModules)))
    in (Flows.bind (Inference.inferGraphTypes dataElements g0) (\inferResult ->  
      let g1 = (Pairs.first inferResult)
      in (Monads.withState g1 (generateLexicon g1))))

-- | Escape unescaped control characters inside JSON string literals
escapeControlCharsInJson :: ([Int] -> [Int])
escapeControlCharsInJson input =  
  let hexDigit = (\n -> Logic.ifElse (Equality.lt n 10) (Math.add 48 n) (Math.add 97 (Math.sub n 10)))
  in  
    let escapeToUnicode = (\b -> [
            92,
            117,
            48,
            48,
            (hexDigit (Math.div b 16)),
            (hexDigit (Math.mod b 16))])
    in  
      let go = (\inStr -> \esc -> \bytes -> Logic.ifElse (Lists.null bytes) [] ( 
              let b = (Lists.head bytes)
              in  
                let bs = (Lists.tail bytes)
                in (Logic.ifElse esc (Lists.cons b (go inStr False bs)) (Logic.ifElse (Logic.and (Equality.equal b 92) inStr) (Lists.cons b (go inStr True bs)) (Logic.ifElse (Equality.equal b 34) (Lists.cons b (go (Logic.not inStr) False bs)) (Logic.ifElse (Logic.and inStr (Equality.lt b 32)) (Lists.concat2 (escapeToUnicode b) (go inStr False bs)) (Lists.cons b (go inStr False bs))))))))
      in (go False False input)

-- | Decode a single module from a JSON value
decodeModuleFromJson :: (Graph.Graph -> [Module__.Module] -> Bool -> Model.Value -> Either String Module__.Module)
decodeModuleFromJson bsGraph universeModules doStripTypeSchemes jsonVal =  
  let graph = (modulesToGraph bsGraph universeModules universeModules)
  in  
    let schemaMap = (buildSchemaMap graph)
    in  
      let modType = (Core.TypeVariable (Core.Name "hydra.module.Module"))
      in (Eithers.either (\err -> Left err) (\term -> Eithers.either (\decErr -> Left (Util.unDecodingError decErr)) (\mod -> Right (Logic.ifElse doStripTypeSchemes (stripModuleTypeSchemes mod) mod)) (Module.module_ graph term)) (Decode.fromJson schemaMap modType jsonVal))
