module Hydra.Sources.Kernel.Types.Graph where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Errors as Error
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging
import qualified Hydra.Sources.Kernel.Types.Typing as Typing


ns :: ModuleName
ns = ModuleName "hydra.graph"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Error.ns, Packaging.ns, Typing.ns],
            moduleDescription = Just "The extension to graphs of Hydra's core type system (hydra.core)"}
  where
    definitions = [
      graph,
      library,
      primitive,
      termCoder]

graph :: TypeDefinition
graph = define "Graph" $
  doc "A graph, or lexical environment which binds names to terms, types, primitives, and metadata" $
  T.record [
    "boundTerms">:
      doc "The terms bound by all term variables in scope" $
      T.map Core.name Core.term,
    "boundTypes">:
      doc "The type schemes of all term variables in scope" $
      T.map Core.name Core.typeScheme,
    "classConstraints">:
      doc ("A mutable map from type variable names to their accumulated class constraints."
        ++ " This is populated during type inference when operations requiring Eq or Ord are encountered.") $
      T.map Core.name Core.typeVariableMetadata,
    "lambdaVariables">:
      doc "The set of term variables introduced by specifically by lambdas" $
      T.set Core.name,
    "metadata">:
      doc "Any additional metadata bound to term variables in scope" $
      T.map Core.name Core.term,
    "primitives">:
      doc "All primitive functions and constants by name" $
      T.map Core.name primitive,
    "schemaTypes">:
      doc "All schema types (type schemes) in scope" $
      T.map Core.name Core.typeScheme,
    "typeVariables">:
      doc "The set of type variables introduced specifically by type lambdas" $
      T.set Core.name]

library :: TypeDefinition
library = define "Library" $
  doc "A library of primitive functions" $
  T.record [
    "namespace">:
      doc "A common prefix for all primitive function names in the library"
      Packaging.moduleNameDef,
    "prefix">:
      doc "A preferred namespace prefix for function names in the library"
      T.string,
    "primitives">:
      doc "The primitives defined in this library" $
      T.list primitive]

primitive :: TypeDefinition
primitive = define "Primitive" $
  doc "A built-in function or constant, consisting of the host-independent PrimitiveDefinition (name, signature, metadata) plus a host-specific implementation." $
  T.record [
    "definition">:
      doc "The host-independent declarative metadata for the primitive: name, description, signature, totality and purity flags, and an optional reference implementation."
      Packaging.primitiveDefinition,
    "implementation">:
      doc ("A concrete implementation of the primitive function."
        ++ " The InferenceContext and Graph parameters are needed by higher-order primitives"
        ++ " (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments"
        ++ " via term reduction; the Graph provides variable and primitive bindings,"
        ++ " while the InferenceContext supports subterm-path tracing for error reporting.") $
      Typing.inferenceContext ~> graph ~> T.list Core.term ~> T.either_ Error.error_ Core.term]

termCoder :: TypeDefinition
termCoder = define "TermCoder" $
  doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms." $
  T.forAll "a" $ T.record [
    "type">:
      doc "The Hydra type of encoded terms"
      Core.type_,
    "encode">:
      doc "An encode function from terms to native values" $
      Typing.inferenceContext ~> graph ~> Core.term ~> T.either_ Error.error_ "a",
    "decode">:
      doc "A decode function from native values to terms" $
      Typing.inferenceContext ~> "a" ~> T.either_ Error.error_ Core.term]
