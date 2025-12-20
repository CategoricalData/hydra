module Hydra.Sources.Kernel.Types.Graph where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Compute as Compute


ns :: Namespace
ns = Namespace "hydra.graph"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Compute.ns] [Core.ns] $
    Just "The extension to graphs of Hydra's core type system (hydra.core)"
  where
    elements = [
      graph,
      primitive,
      termCoder]

graph :: Binding
graph = define "Graph" $
  doc "A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph" $
  T.record [
    -- TODO: remove this; replace it with 'environment'
    "elements">:
      doc "All of the elements in the graph" $
      T.map Core.name Core.binding,
    "environment">:
      doc "The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)" $
      T.map Core.name (T.maybe Core.term),
    "types">:
      doc "The typing environment of the graph" $
      T.map Core.name Core.typeScheme,
    "body">:
      doc "The body of the term which generated this context"
      Core.term,
    "primitives">:
      doc "All supported primitive constants and functions, by name" $
      T.map Core.name primitive,
    "schema">:
      doc "The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph." $
      T.maybe graph]

primitive :: Binding
primitive = define "Primitive" $
  doc "A built-in function" $
  T.record [
    "name">:
      doc "The unique name of the primitive function"
      Core.name,
    "type">:
      doc "The type signature of the primitive function"
      Core.typeScheme,
    "implementation">:
      doc "A concrete implementation of the primitive function" $
      T.list Core.term ~> Compute.flow @@ graph @@ Core.term]

termCoder :: Binding
termCoder = define "TermCoder" $
  doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms" $
  T.forAll "a" $ T.record [
    "type">:
      doc "The Hydra type of encoded terms"
      Core.type_,
    "coder">:
      doc "A coder between Hydra terms and instances of the given type" $
      Compute.coder @@ graph @@ graph @@ Core.term @@ "a"]
