-- | DSL source module for the pure code generation pipeline.
-- Promotes the pure core of Hydra.Generation into a kernel module
-- that gets generated to all target languages (Haskell, Java, Python).
-- Issue #225: Minimize Generation.hs

module Hydra.Sources.Kernel.Terms.CodeGeneration where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  buildSchemaMap, decodeModuleFromJson,
  escapeControlCharsInJson, formatPrimitive, formatTermBinding, formatTypeBinding,
  generateCoderModules, generateLexicon, generateSourceFiles,
  inferAndGenerateLexicon, inferModules,
  moduleToJson, moduleToSourceModule, modulesToGraph,
  moduleTermDepsTransitive, moduleTypeDepsTransitive,
  namespaceToPath, stripModuleTypeSchemes, transitiveDeps)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors    as Accessors
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Coders       as Coders
import qualified Hydra.Dsl.Meta.Compute      as Compute
import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Grammar      as Grammar
import qualified Hydra.Dsl.Grammars          as Grammars
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Meta.Json         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Meta.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Meta.Error        as Error
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import qualified Hydra.Json.Model                as JsonModel
import           Prelude hiding ((++))
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

-- Dependencies on other term modules
import qualified Hydra.Sources.Json.Decode                  as JsonDecode
import qualified Hydra.Sources.Kernel.Terms.Adapt           as Adapt
import qualified Hydra.Sources.Kernel.Terms.Annotations     as Annotations
import qualified Hydra.Sources.Kernel.Terms.Inference       as Inference
import qualified Hydra.Sources.Kernel.Terms.Lexical         as Lexical
import qualified Hydra.Sources.Kernel.Terms.Names           as Names

import qualified Hydra.Sources.Kernel.Terms.Rewriting       as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Schemas         as Schemas
import qualified Hydra.Sources.Kernel.Terms.Show.Core       as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Show.Error      as ShowError

-- Dependencies on secondary generated modules (decode/encode)
import qualified Hydra.Sources.Decode.Core                  as DecodeCore
import qualified Hydra.Sources.Decode.Module                as DecodeModule
import qualified Hydra.Sources.Encode.Module                as EncodeModule


ns :: Namespace
ns = Namespace "hydra.codeGeneration"

module_ :: Module
module_ = Module ns elements
    [Adapt.ns, Annotations.ns, Inference.ns, JsonDecode.ns, Lexical.ns, Names.ns,
     Rewriting.ns, Schemas.ns, ShowCore.ns, ShowError.ns,
     Namespace "hydra.decoding", Namespace "hydra.encoding",
     Namespace "hydra.json.decode", Namespace "hydra.json.encode", Namespace "hydra.json.writer",
     moduleNamespace DecodeCore.module_, moduleNamespace DecodeModule.module_, moduleNamespace EncodeModule.module_]
    kernelTypesNamespaces $
    Just "Pure code generation pipeline for bootstrapping Hydra across languages."
  where
    elements = [
      toBinding namespaceToPath,
      toBinding stripModuleTypeSchemes,
      toBinding transitiveDeps,
      toBinding moduleTermDepsTransitive,
      toBinding moduleTypeDepsTransitive,
      toBinding modulesToGraph,
      toBinding generateSourceFiles,
      toBinding formatTermBinding,
      toBinding formatPrimitive,
      toBinding formatTypeBinding,
      toBinding buildSchemaMap,
      toBinding moduleToSourceModule,
      toBinding generateLexicon,
      toBinding moduleToJson,
      toBinding inferModules,
      toBinding generateCoderModules,
      toBinding inferAndGenerateLexicon,
      toBinding escapeControlCharsInJson,
      toBinding decodeModuleFromJson]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_


-- | Convert a namespace to a file path (e.g., "hydra.core" -> "hydra/core").
namespaceToPath :: TBinding (Namespace -> String)
namespaceToPath = define "namespaceToPath" $
  doc "Convert a namespace to a file path (e.g., hydra.core -> hydra/core)" $
  "ns" ~>
  Strings.intercalate (string "/") (Strings.splitOn (string ".") (Module.unNamespace $ var "ns"))

-- | Strip TypeSchemes from term bindings in a module, preserving type binding TypeSchemes.
stripModuleTypeSchemes :: TBinding (Module -> Module)
stripModuleTypeSchemes = define "stripModuleTypeSchemes" $
  doc ("Strip TypeSchemes from term bindings in a module, preserving type binding TypeSchemes."
    <> " JSON-loaded modules carry inferred TypeSchemes from the original compilation."
    <> " After adaptation (e.g., bigfloat -> float64), these TypeSchemes become stale"
    <> " and can cause inference errors. Stripping them allows the inference engine"
    <> " to reconstruct correct TypeSchemes from scratch.") $
  "m" ~>
  "stripIfTerm" <~ ("b" ~> Logic.ifElse (Annotations.isNativeType @@ var "b")
    (var "b")
    (Core.binding
      (Core.bindingName $ var "b")
      (Core.bindingTerm $ var "b")
      nothing)) $
  Module.module_
    (Module.moduleNamespace $ var "m")
    (Lists.map (var "stripIfTerm") (Module.moduleElements $ var "m"))
    (Module.moduleTermDependencies $ var "m")
    (Module.moduleTypeDependencies $ var "m")
    (Module.moduleDescription $ var "m")

-- | Compute transitive closure of dependencies.
-- Given a function that extracts dependency namespaces from a module,
-- a namespace-to-module map, and starting modules, returns the transitive closure
-- of all reachable namespaces (excluding self-references from start modules).
transitiveDeps :: TBinding ((Module -> [Namespace]) -> M.Map Namespace Module -> [Module] -> S.Set Namespace)
transitiveDeps = define "transitiveDeps" $
  doc "Compute transitive closure of module dependencies" $
  "getDeps" ~> "nsMap" ~> "startMods" ~>
  -- Start with dependencies of start modules, excluding self-references
  "initialDeps" <~ Sets.fromList (Lists.concat (Lists.map
    ("m" ~> Lists.filter
      ("dep" ~> Logic.not $ Equality.equal (var "dep") (Module.moduleNamespace $ var "m"))
      (var "getDeps" @@ var "m"))
    (var "startMods"))) $
  -- Iterative closure: go pending visited
  "go" <~ ("pending" ~> "visited" ~>
    Logic.ifElse (Sets.null $ var "pending")
      (var "visited")
      ("newVisited" <~ Sets.union (var "visited") (var "pending") $
       "nextDeps" <~ Sets.fromList (Lists.concat (Lists.map
         ("nsv" ~> optCases (Maps.lookup (var "nsv") (var "nsMap"))
           (TTerm (Terms.list []) :: TTerm [Namespace])
           ("depMod" ~> var "getDeps" @@ var "depMod"))
         (Sets.toList $ var "pending"))) $
       "newPending" <~ Sets.difference (var "nextDeps") (var "newVisited") $
       var "go" @@ var "newPending" @@ var "newVisited")) $
  var "go" @@ var "initialDeps" @@ (Sets.empty :: TTerm (S.Set Namespace))

-- | Compute the transitive closure of term dependencies for a set of modules.
-- Returns the modules that are transitively depended upon (including the input modules).
moduleTermDepsTransitive :: TBinding (M.Map Namespace Module -> [Module] -> [Module])
moduleTermDepsTransitive = define "moduleTermDepsTransitive" $
  doc "Compute transitive closure of term dependencies for a set of modules" $
  "nsMap" ~> "modules" ~>
  "closure" <~ Sets.union
    (transitiveDeps @@ ("m" ~> Module.moduleTermDependencies (var "m")) @@ var "nsMap" @@ var "modules")
    (Sets.fromList $ Lists.map ("m" ~> Module.moduleNamespace (var "m")) (var "modules")) $
  Maybes.cat $ Lists.map
    ("n" ~> Maps.lookup (var "n") (var "nsMap"))
    (Sets.toList $ var "closure")

-- | Compute the transitive closure of type dependencies for a set of modules.
-- First computes transitive term deps, then type deps from those.
moduleTypeDepsTransitive :: TBinding (M.Map Namespace Module -> [Module] -> [Module])
moduleTypeDepsTransitive = define "moduleTypeDepsTransitive" $
  doc "Compute transitive closure of type dependencies for a set of modules" $
  "nsMap" ~> "modules" ~>
  "termMods" <~ moduleTermDepsTransitive @@ var "nsMap" @@ var "modules" $
  "typeNamespaces" <~ Sets.toList (transitiveDeps @@ ("m" ~> Module.moduleTypeDependencies (var "m")) @@ var "nsMap" @@ var "termMods") $
  Maybes.cat $ Lists.map
    ("n" ~> Maps.lookup (var "n") (var "nsMap"))
    (var "typeNamespaces")

-- | Build a graph from a list of modules, using an explicit bootstrap graph.
-- Elements are partitioned into schema (type definitions) and data (term definitions)
-- based on the isNativeType predicate.
modulesToGraph :: TBinding (Graph -> [Module] -> [Module] -> Graph)
modulesToGraph = define "modulesToGraph" $
  doc "Build a graph from universe modules and working modules, using an explicit bootstrap graph" $
  "bsGraph" ~> "universeModules" ~> "modules" ~>
  "universe" <~ Maps.fromList (Lists.map
    ("m" ~> pair (Module.moduleNamespace $ var "m") (var "m"))
    (Lists.concat2 (var "universeModules") (var "modules"))) $
  "schemaModules" <~ moduleTypeDepsTransitive @@ var "universe" @@ var "modules" $
  "dataModules" <~ moduleTermDepsTransitive @@ var "universe" @@ var "modules" $
  -- Include type elements from both transitive type dependencies AND the input modules themselves
  "schemaElements" <~ Lists.filter ("e" ~> Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m"))
      (Lists.concat2 (var "schemaModules") (var "modules"))) $
  "dataElements" <~ Lists.filter ("e" ~> Logic.not $ Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m")) (var "dataModules")) $
  "schemaGraph" <~ Lexical.elementsToGraph @@ var "bsGraph" @@ Maps.empty @@ var "schemaElements" $
  -- Decode the schema graph to a Map Name TypeScheme (interim solution until schema graphs are eliminated)
  "schemaTypes" <~ Eithers.either_
    (constant (Maps.empty :: TTerm (M.Map Name TypeScheme)))
    ("_r" ~> var "_r")
    (Schemas.schemaGraphToTypingEnvironment @@ Lexical.emptyContext @@ var "schemaGraph") $
  Lexical.elementsToGraph @@ var "bsGraph" @@ var "schemaTypes" @@ var "dataElements"

-- | Pure core of code generation: given a coder, language, flags, bootstrap graph, universe,
-- and modules to generate, produce a list of (filePath, content) pairs.
-- This function contains no I/O and can be generated to other languages.
generateSourceFiles
  :: TBinding ((Module -> [Definition] -> Context -> Graph -> Prelude.Either (InContext Error) (M.Map String String))
    -> Language
    -> Bool -> Bool -> Bool -> Bool
    -> Graph -> [Module] -> [Module]
    -> Context -> Prelude.Either (InContext Error) [(String, String)])
generateSourceFiles = define "generateSourceFiles" $
  doc ("Pure core of code generation: given a coder, language, flags, bootstrap graph, universe,"
    <> " and modules to generate, produce a list of (filePath, content) pairs.") $
  "printDefinitions" ~> "lang" ~>
  "doInfer" ~> "doExpand" ~> "doHoistCaseStatements" ~> "doHoistPolymorphicLetBindings" ~>
  "bsGraph" ~> "universeModules" ~> "modsToGenerate" ~> "cx" ~>

  "namespaceMap" <~ Maps.fromList (Lists.map
    ("m" ~> pair (Module.moduleNamespace $ var "m") (var "m"))
    (Lists.concat2 (var "universeModules") (var "modsToGenerate"))) $

  "constraints" <~ Coders.languageConstraintsProjection (var "lang") $

  -- Partition modules into type and term modules
  "isTypeModule" <~ ("mod" ~> Logic.not $ Lists.null $
    Lists.filter ("e" ~> Annotations.isNativeType @@ var "e") (Module.moduleElements $ var "mod")) $
  "partitioned" <~ Lists.partition (var "isTypeModule") (var "modsToGenerate") $
  "typeModulesToGenerate" <~ Pairs.first (var "partitioned") $
  "termModulesToGenerate" <~ Pairs.second (var "partitioned") $

  -- Compute transitive deps and build graphs
  "schemaMods" <~ moduleTypeDepsTransitive @@ var "namespaceMap" @@ var "modsToGenerate" $
  "schemaElements" <~ Lists.filter ("e" ~> Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m"))
      (Lists.concat2 (var "schemaMods") (var "typeModulesToGenerate"))) $
  "dataMods" <~ moduleTermDepsTransitive @@ var "namespaceMap" @@ var "modsToGenerate" $
  "dataElements" <~ Lists.concat (Lists.map ("m" ~> Module.moduleElements (var "m")) (var "dataMods")) $
  "schemaGraph" <~ Lexical.elementsToGraph @@ var "bsGraph" @@ Maps.empty @@ var "schemaElements" $
  "schemaTypes2" <~ Eithers.either_
    (constant (Maps.empty :: TTerm (M.Map Name TypeScheme)))
    ("_r" ~> var "_r")
    (Schemas.schemaGraphToTypingEnvironment @@ Lexical.emptyContext @@ var "schemaGraph") $
  "dataGraph" <~ Lexical.elementsToGraph @@ var "bsGraph" @@ var "schemaTypes2" @@ var "dataElements" $

  -- Generate type modules
  "schemaFiles" <<~ Logic.ifElse (Lists.null $ var "typeModulesToGenerate")
    (right (TTerm (Terms.list []) :: TTerm [(String, String)]))
    ("nameLists" <~ Lists.map
        ("m" ~> Lists.map ("e" ~> Core.bindingName $ var "e")
          (Lists.filter ("e" ~> Annotations.isNativeType @@ var "e") (Module.moduleElements $ var "m")))
        (var "typeModulesToGenerate") $
      "schemaResult" <<~ Eithers.bimap ("s" ~> Ctx.inContext (Error.errorOther $ Error.otherError (var "s")) (var "cx")) ("r" ~> var "r")
        (Adapt.schemaGraphToDefinitions @@ var "constraints" @@ var "schemaGraph" @@ var "nameLists" @@ var "cx") $
      "defLists" <~ Pairs.second (var "schemaResult") $
      "schemaGraphWithTypes" <~ Graph.graphWithSchemaTypes (var "schemaGraph") (var "schemaTypes2") $
      Eithers.map ("xs" ~> Lists.concat (var "xs")) $
        Eithers.mapList ("p" ~>
          "mod" <~ Pairs.first (var "p") $
          "defs" <~ Pairs.second (var "p") $
          Eithers.map ("m" ~> Maps.toList (var "m")) $
            var "printDefinitions" @@ var "mod" @@ Lists.map ("d" ~> Module.definitionType (var "d")) (var "defs") @@ var "cx" @@ var "schemaGraphWithTypes")
        (Lists.zip (var "typeModulesToGenerate") (var "defLists"))) $

  -- Generate term modules
  "termFiles" <<~ Logic.ifElse (Lists.null $ var "termModulesToGenerate")
    (right (TTerm (Terms.list []) :: TTerm [(String, String)]))
    ("namespaces" <~ Lists.map ("m" ~> Module.moduleNamespace (var "m")) (var "termModulesToGenerate") $
      "dataResult" <<~ Eithers.bimap ("s" ~> Ctx.inContext (Error.errorOther $ Error.otherError (var "s")) (var "cx")) ("r" ~> var "r")
        (Adapt.dataGraphToDefinitions
          @@ var "constraints"
          @@ var "doInfer" @@ var "doExpand" @@ var "doHoistCaseStatements" @@ var "doHoistPolymorphicLetBindings"
          @@ var "dataElements" @@ var "dataGraph" @@ var "namespaces" @@ var "cx") $
      "g1" <~ Pairs.first (var "dataResult") $
      "defLists" <~ Pairs.second (var "dataResult") $
      -- Refresh modules with elements from the inferred graph
      "refreshModule" <~ ("els" ~> "m" ~>
        Module.module_
          (Module.moduleNamespace $ var "m")
          (Maybes.cat $ Lists.map
            ("e" ~> Lists.find ("b" ~> Equality.equal (Core.bindingName $ var "b") (Core.bindingName $ var "e")) (var "els"))
            (Module.moduleElements $ var "m"))
          (Module.moduleTermDependencies $ var "m")
          (Module.moduleTypeDependencies $ var "m")
          (Module.moduleDescription $ var "m")) $
      "refreshedMods" <~ Lists.map ("m" ~> var "refreshModule" @@ (Lexical.graphToBindings @@ var "g1") @@ var "m") (var "termModulesToGenerate") $
      Eithers.map ("xs" ~> Lists.concat (var "xs")) $
        Eithers.mapList ("p" ~>
          "mod" <~ Pairs.first (var "p") $
          "defs" <~ Pairs.second (var "p") $
          Eithers.map ("m" ~> Maps.toList (var "m")) $
            var "printDefinitions" @@ var "mod" @@ Lists.map ("d" ~> Module.definitionTerm (var "d")) (var "defs") @@ var "cx" @@ var "g1")
        (Lists.zip (var "refreshedMods") (var "defLists"))) $

  -- Combine results
  right $ Lists.concat2 (var "schemaFiles") (var "termFiles")

-- | Format a term binding for the lexicon: "  name : typeScheme"
formatTermBinding :: TBinding (Binding -> String)
formatTermBinding = define "formatTermBinding" $
  doc "Format a term binding for the lexicon" $
  "binding" ~>
  "name" <~ Core.unName (Core.bindingName $ var "binding") $
  "typeStr" <~ optCases (Core.bindingType $ var "binding")
    (string "?")
    ("scheme" ~> ShowCore.typeScheme @@ var "scheme") $
  (string "  ") ++ var "name" ++ (string " : ") ++ var "typeStr"

-- | Format a primitive for the lexicon: "  name : typeScheme"
formatPrimitive :: TBinding (Primitive -> String)
formatPrimitive = define "formatPrimitive" $
  doc "Format a primitive for the lexicon" $
  "prim" ~>
  "name" <~ Core.unName (Graph.primitiveName $ var "prim") $
  "typeStr" <~ ShowCore.typeScheme @@ (Graph.primitiveType $ var "prim") $
  (string "  ") ++ var "name" ++ (string " : ") ++ var "typeStr"

-- | Format a type binding for the lexicon: "  name = type"
formatTypeBinding :: TBinding (Graph -> Binding -> Prelude.Either DecodingError String)
formatTypeBinding = define "formatTypeBinding" $
  doc "Format a type binding for the lexicon" $
  "graph" ~> "binding" ~>
  "typ" <<~ decoderFor _Type @@ var "graph" @@ (Core.bindingTerm $ var "binding") $
  right $
    (string "  ") ++ Core.unName (Core.bindingName $ var "binding") ++ (string " = ") ++ (ShowCore.type_ @@ var "typ")

-- | Build a schema map (Name -> Type) from a graph's schema types.
-- Used by the JSON decoder to resolve type variables.
buildSchemaMap :: TBinding (Graph -> M.Map Name Type)
buildSchemaMap = define "buildSchemaMap" $
  doc "Build a schema map (Name -> Type) from a graph's schema types" $
  "g" ~>
  Maps.map ("ts" ~> Rewriting.deannotateType @@ (Core.typeSchemeType $ var "ts"))
    (Graph.graphSchemaTypes $ var "g")

-- | Convert a generated Module into a Source module.
-- The Source module contains a single binding `module_` which holds the Module encoded as a Term.
-- The namespace transforms e.g. "hydra.encode.util" to "hydra.sources.encode.util"
moduleToSourceModule :: TBinding (Module -> Module)
moduleToSourceModule = define "moduleToSourceModule" $
  doc "Convert a generated Module into a Source module" $
  "m" ~>
  -- Transform namespace: hydra.encode.util -> hydra.sources.encode.util
  "sourceNs" <~ wrap _Namespace (
    (string "hydra.sources.") ++ Strings.intercalate (string ".")
      (Lists.drop (int32 1) (Strings.splitOn (string ".") (Module.unNamespace $ Module.moduleNamespace $ var "m")))) $
  -- The module type namespace
  "modTypeNs" <~ (wrap _Namespace (string "hydra.module") :: TTerm Namespace) $
  -- Create binding: module_ = <encoded Module term>
  "moduleBinding" <~ Core.binding
    (wrap _Name (Module.unNamespace (var "sourceNs") ++ (string ".module_")))
    (encoderFor _Module @@ var "m")
    nothing $
  Module.module_
    (var "sourceNs")
    (list [var "moduleBinding"])
    (list [var "modTypeNs"])
    (list [var "modTypeNs"])
    (just $ (string "Source module for ") ++ Module.unNamespace (Module.moduleNamespace $ var "m"))

-- | Generate the lexicon content from a graph.
-- Lists all primitives, types, and terms with their types.
generateLexicon :: TBinding (Graph -> Prelude.Either DecodingError String)
generateLexicon = define "generateLexicon" $
  doc "Generate the lexicon content from a graph" $
  "graph" ~>
  "bindings" <~ Lexical.graphToBindings @@ var "graph" $
  "primitives" <~ Maps.elems (Graph.graphPrimitives $ var "graph") $
  "partitioned" <~ Lists.partition ("b" ~> Annotations.isNativeType @@ var "b") (var "bindings") $
  "typeBindings" <~ Pairs.first (var "partitioned") $
  "termBindings" <~ Pairs.second (var "partitioned") $
  "sortedPrimitives" <~ Lists.sortOn ("p" ~> Graph.primitiveName (var "p")) (var "primitives") $
  "sortedTypes" <~ Lists.sortOn ("b" ~> Core.bindingName (var "b")) (var "typeBindings") $
  "sortedTerms" <~ Lists.sortOn ("b" ~> Core.bindingName (var "b")) (var "termBindings") $
  "typeLines" <<~ Eithers.mapList ("b" ~> formatTypeBinding @@ var "graph" @@ var "b") (var "sortedTypes") $
  "termLines" <~ Lists.map ("b" ~> formatTermBinding @@ var "b") (var "sortedTerms") $
  "primitiveLines" <~ Lists.map ("p" ~> formatPrimitive @@ var "p") (var "sortedPrimitives") $
  right $
    (string "Primitives:\n") ++ Strings.unlines (var "primitiveLines")
    ++ (string "\nTypes:\n") ++ Strings.unlines (var "typeLines")
    ++ (string "\nTerms:\n") ++ Strings.unlines (var "termLines")

-- | Convert a Module to a JSON string.
-- Encodes the Module as a Term, converts to JSON, then serializes to a string.
moduleToJson :: TBinding (Module -> Either String String)
moduleToJson = define "moduleToJson" $
  doc "Convert a Module to a JSON string" $
  "m" ~>
  "term" <~ encoderFor _Module @@ var "m" $
  Eithers.map ("json" ~> var "hydra.json.writer.printJson" @@ var "json")
    (var "hydra.json.encode.toJson" @@ var "term")

-- | Perform type inference on a set of modules and reconstruct the target modules
-- with inferred types. Type-only modules (containing only native type definitions)
-- are passed through unchanged.
inferModules :: TBinding (Context -> Graph -> [Module] -> [Module] -> Prelude.Either (InContext Error) [Module])
inferModules = define "inferModules" $
  doc "Perform type inference on modules and reconstruct with inferred types" $
  "cx" ~> "bsGraph" ~> "universeMods" ~> "targetMods" ~>
  "g0" <~ modulesToGraph @@ var "bsGraph" @@ var "universeMods" @@ var "universeMods" $
  "dataElements" <~ Lists.filter ("e" ~> Logic.not $ Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m")) (var "universeMods")) $
  "inferResultWithCx" <<~ Inference.inferGraphTypes @@ var "cx" @@ var "dataElements" @@ var "g0" $
  "inferResult" <~ Pairs.first (var "inferResultWithCx") $
  "g1" <~ Pairs.first (var "inferResult") $
  "inferredElements" <~ Pairs.second (var "inferResult") $
  "isTypeModule" <~ ("mod" ~> Lists.null $
    Lists.filter ("e" ~> Logic.not $ Annotations.isNativeType @@ var "e")
      (Module.moduleElements $ var "mod")) $
  "refreshModule" <~ ("m" ~>
    Logic.ifElse (var "isTypeModule" @@ var "m")
      (var "m")
      (Module.module_
        (Module.moduleNamespace $ var "m")
        (Maybes.cat $ Lists.map
          ("e" ~> Lists.find ("b" ~> Equality.equal (Core.bindingName $ var "b") (Core.bindingName $ var "e"))
            (var "inferredElements"))
          (Module.moduleElements $ var "m"))
        (Module.moduleTermDependencies $ var "m")
        (Module.moduleTypeDependencies $ var "m")
        (Module.moduleDescription $ var "m"))) $
  right $ Lists.map (var "refreshModule") (var "targetMods")

-- | Generate encoder or decoder modules for a list of type modules.
-- Takes a codec function, bootstrap graph, universe modules, and type modules.
-- Returns the generated coder modules (Nothing results are filtered out).
generateCoderModules
  :: TBinding ((Context -> Graph -> Module -> Prelude.Either (InContext Error) (Maybe Module)) -> Graph -> [Module] -> [Module]
    -> Context -> Prelude.Either (InContext Error) [Module])
generateCoderModules = define "generateCoderModules" $
  doc "Generate encoder or decoder modules for a list of type modules" $
  "codec" ~> "bsGraph" ~> "universeModules" ~> "typeModules" ~> "cx" ~>
  -- Build a graph that includes both schema and data elements, since codecs need to dereference type elements
  "universe" <~ Maps.fromList (Lists.map
    ("m" ~> pair (Module.moduleNamespace $ var "m") (var "m"))
    (Lists.concat2 (var "universeModules") (var "universeModules"))) $
  "schemaModules" <~ moduleTypeDepsTransitive @@ var "universe" @@ var "universeModules" $
  "dataModules" <~ moduleTermDepsTransitive @@ var "universe" @@ var "universeModules" $
  "schemaElements" <~ Lists.filter ("e" ~> Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m"))
      (Lists.concat2 (var "schemaModules") (var "universeModules"))) $
  "dataElements" <~ Lists.filter ("e" ~> Logic.not $ Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m")) (var "dataModules")) $
  "schemaGraph" <~ Lexical.elementsToGraph @@ var "bsGraph" @@ Maps.empty @@ var "schemaElements" $
  "schemaTypes" <~ Eithers.either_
    (constant (Maps.empty :: TTerm (M.Map Name TypeScheme)))
    ("_r" ~> var "_r")
    (Schemas.schemaGraphToTypingEnvironment @@ Lexical.emptyContext @@ var "schemaGraph") $
  "allElements" <~ Lists.concat2 (var "schemaElements") (var "dataElements") $
  "graph" <~ Lexical.elementsToGraph @@ var "bsGraph" @@ var "schemaTypes" @@ var "allElements" $
  Eithers.map ("results" ~> Maybes.cat (var "results")) $
    Eithers.mapList ("m" ~> var "codec" @@ var "cx" @@ var "graph" @@ var "m") (var "typeModules")

-- | Perform type inference on a graph and generate its lexicon.
-- Composes inferGraphTypes and generateLexicon into a single computation.
inferAndGenerateLexicon :: TBinding (Context -> Graph -> [Module] -> Prelude.Either String String)
inferAndGenerateLexicon = define "inferAndGenerateLexicon" $
  doc "Perform type inference and generate the lexicon for a set of modules" $
  "cx" ~> "bsGraph" ~> "kernelModules" ~>
  "g0" <~ modulesToGraph @@ var "bsGraph" @@ var "kernelModules" @@ var "kernelModules" $
  "dataElements" <~ Lists.filter ("e" ~> Logic.not $ Annotations.isNativeType @@ var "e")
    (Lists.concat $ Lists.map ("m" ~> Module.moduleElements (var "m")) (var "kernelModules")) $
  "inferResultWithCx" <<~ Eithers.bimap ("ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")) ("x" ~> var "x") (Inference.inferGraphTypes @@ var "cx" @@ var "dataElements" @@ var "g0") $
  "g1" <~ Pairs.first (Pairs.first $ var "inferResultWithCx") $
  Eithers.bimap Error.unDecodingError ("x" ~> var "x") (generateLexicon @@ var "g1")

-- | Escape unescaped control characters (< 0x20) inside JSON string literals.
-- Operates on a list of int32 character codes (bytes).
-- Walks through the list tracking whether we're inside a string and
-- replaces raw control chars with \\uXXXX escape sequences.
escapeControlCharsInJson :: TBinding ([Int] -> [Int])
escapeControlCharsInJson = define "escapeControlCharsInJson" $
  doc "Escape unescaped control characters inside JSON string literals" $
  "input" ~>
  "hexDigit" <~ ("n" ~>
    Logic.ifElse (Equality.lt (var "n") (int32 10))
      (Math.add (int32 0x30) (var "n"))       -- '0' + n
      (Math.add (int32 0x61) (Math.sub (var "n") (int32 10)))) $ -- 'a' + (n - 10)
  "escapeToUnicode" <~ ("b" ~>
    list [int32 0x5C, int32 0x75, int32 0x30, int32 0x30,
          var "hexDigit" @@ Math.div (var "b") (int32 16),
          var "hexDigit" @@ Math.mod (var "b") (int32 16)]) $
  -- go :: Bool -> Bool -> [Int32] -> [Int32]
  "go" <~ ("inStr" ~> "esc" ~> "bytes" ~>
    Logic.ifElse (Lists.null $ var "bytes")
      (TTerm (Terms.list []) :: TTerm [Int])
      ("b" <~ Lists.head (var "bytes") $
       "bs" <~ Lists.tail (var "bytes") $
       Logic.ifElse (var "esc")
         -- after backslash, pass through next byte
         (Lists.cons (var "b") (var "go" @@ var "inStr" @@ boolean False @@ var "bs"))
         (Logic.ifElse (Logic.and (Equality.equal (var "b") (int32 0x5C)) (var "inStr"))
           -- backslash inside string
           (Lists.cons (var "b") (var "go" @@ var "inStr" @@ boolean True @@ var "bs"))
           (Logic.ifElse (Equality.equal (var "b") (int32 0x22))
             -- quote toggles string mode
             (Lists.cons (var "b") (var "go" @@ (Logic.not $ var "inStr") @@ boolean False @@ var "bs"))
             (Logic.ifElse (Logic.and (var "inStr") (Equality.lt (var "b") (int32 0x20)))
               -- control char: replace with \uXXXX
               (Lists.concat2 (var "escapeToUnicode" @@ var "b") (var "go" @@ var "inStr" @@ boolean False @@ var "bs"))
               -- normal byte
               (Lists.cons (var "b") (var "go" @@ var "inStr" @@ boolean False @@ var "bs"))))))) $
  var "go" @@ boolean False @@ boolean False @@ var "input"

-- | Decode a single module from a JSON value.
-- Given a bootstrap graph, universe modules, whether to strip TypeSchemes,
-- and a JSON value, decodes it to a Module.
-- This is the pure core of the JSON module loading pipeline.
decodeModuleFromJson :: TBinding (Graph -> [Module] -> Bool -> JsonModel.Value -> Either String Module)
decodeModuleFromJson = define "decodeModuleFromJson" $
  doc "Decode a single module from a JSON value" $
  "bsGraph" ~> "universeModules" ~> "doStripTypeSchemes" ~> "jsonVal" ~>
  "graph" <~ modulesToGraph @@ var "bsGraph" @@ var "universeModules" @@ var "universeModules" $
  "schemaMap" <~ buildSchemaMap @@ var "graph" $
  "modType" <~ Core.typeVariable (wrap _Name (string "hydra.module.Module")) $
  -- Step 1: JSON -> Term
  Eithers.either_
    ("err" ~> left (var "err"))
    ("term" ~>
      -- Step 2: Term -> Module (via decoderFor _Module)
      Eithers.either_
        ("decErr" ~> left (Error.unDecodingError @@ var "decErr"))
        ("mod" ~> right (Logic.ifElse (var "doStripTypeSchemes")
          (stripModuleTypeSchemes @@ var "mod")
          (var "mod")))
        (decoderFor _Module @@ var "graph" @@ var "term"))
    (JsonDecode.fromJson @@ var "schemaMap" @@ Core.nameLift _Module @@ var "modType" @@ var "jsonVal")
