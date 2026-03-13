module Hydra.Sources.Kernel.Types.Graph where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Context as Context
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Error as Error


ns :: Namespace
ns = Namespace "hydra.graph"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Compute.ns, Context.ns, Core.ns, Error.ns] [Compute.ns, Context.ns, Core.ns, Error.ns] $
    Just "The extension to graphs of Hydra's core type system (hydra.core)"
  where
    elements = [
      graph,
      primitive,
      termCoder]

graph :: Binding
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

primitive :: Binding
primitive = define "Primitive" $
  doc "A built-in function or constant" $
  T.record [
    "name">:
      doc "The unique name of the primitive function"
      Core.name,
    "type">:
      doc "The type signature of the primitive function"
      Core.typeScheme,
    "implementation">:
      doc ("A concrete implementation of the primitive function."
        ++ " The Context and Graph parameters are needed by higher-order primitives"
        ++ " (e.g. lists.map, lists.foldl, eithers.bind) which must evaluate function arguments"
        ++ " via term reduction; the Graph provides variable and primitive bindings,"
        ++ " while the Context supports tracing and error reporting.") $
      Context.context ~> graph ~> T.list Core.term ~> T.either_ (Context.inContext @@ Error.error_) Core.term]

termCoder :: Binding
termCoder = define "TermCoder" $
  doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms." $
  T.forAll "a" $ T.record [
    "type">:
      doc "The Hydra type of encoded terms"
      Core.type_,
    "encode">:
      doc "An encode function from terms to native values" $
      Context.context ~> graph ~> Core.term ~> T.either_ (Context.inContext @@ Error.error_) "a",
    "decode">:
      doc "A decode function from native values to terms" $
      Context.context ~> "a" ~> T.either_ (Context.inContext @@ Error.error_) Core.term]
