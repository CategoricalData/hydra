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
            moduleMetadata = descriptionMetadata (Just "The extension to graphs of Hydra's core type system (hydra.core)")}
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
      T.map Core.name Core.typeVariableConstraints,
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
    "name">:
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
      doc ("A concrete, host-specific implementation of the primitive function: a mapping from a list of"
        ++ " argument terms to a result term, or an error."
        ++ " Arguments are reduced and stripped of annotations by the interpreter before the implementation is"
        ++ " invoked, so the implementation can pattern-match the argument terms directly; a higher-order primitive"
        ++ " can return an unreduced applicative term and let the outer reducer fold it."
        ++ " The current carrier still threads `InferenceContext` and `Graph` for historical encode/decode and"
        ++ " reducer-callback plumbing; no primitive's compute logic uses them, and they are slated for removal"
        ++ " (https://github.com/CategoricalData/hydra/issues/446), sequenced with the `defaultImplementation`"
        ++ " integration tracked in https://github.com/CategoricalData/hydra/issues/437.") $
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
