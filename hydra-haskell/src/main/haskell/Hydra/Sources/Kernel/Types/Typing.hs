module Hydra.Sources.Kernel.Types.Typing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.typing"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "Types supporting type inference and type reconstruction."
  where
    elements = [
      inferenceContext,
      inferenceResult,
      termSubst,
      typeConstraint,
      typeContext,
      typeSubst]

inferenceContext :: Binding
inferenceContext = define "InferenceContext" $
  doc "The context provided to type inference, including various typing environments." $
  T.record [
    "schemaTypes">:
      doc "A fixed typing environment which is derived from the schema of the graph." $
      T.map Core.name Core.typeScheme,
    "primitiveTypes">:
      doc "A fixed typing environment which is derived from the set of primitives in the graph." $
      T.map Core.name Core.typeScheme,
    "dataTypes">:
      doc ("A mutable typing environment which is specific to the current graph being processed."
        ++ " This environment is (usually) smaller than the schema and primitive typing environments,"
        ++ " and is subject to global substitutions.") $
      T.map Core.name Core.typeScheme,
    "debug">:
      doc "Whether to enable debug output during type inference"
      T.boolean]

inferenceResult :: Binding
inferenceResult = define "InferenceResult" $
  doc "The result of applying inference rules to a term." $
  T.record [
    "term">:
      doc "The term which was inferred"
      Core.term,
    "type">:
      doc "The inferred type of the term"
      Core.type_,
    "subst">:
      doc "The type substitution resulting from unification"
      typeSubst]

termSubst :: Binding
termSubst = define "TermSubst" $
  doc "A substitution of term variables for terms" $
  T.wrap $ T.map Core.name Core.term

typeConstraint :: Binding
typeConstraint = define "TypeConstraint" $
  doc "An assertion that two types can be unified into a single type" $
  T.record [
    "left">:
      doc "The left-hand side of the constraint"
      Core.type_,
    "right">:
      doc "The right-hand side of the constraint"
      Core.type_,
    "comment">:
      doc "A description of the type constraint which may be used for tracing or debugging"
      T.string]

typeContext :: Binding
typeContext = define "TypeContext" $
  doc "A typing environment used for type reconstruction (typeOf) over System F terms" $
  T.record [
    "types">:
      doc "A mapping of lambda- and let-bound variables to their types" $
      T.map Core.name Core.type_,
    "variables">:
      doc "The set of type variables introduced by enclosing type lambdas" $
      T.set Core.name,
    "inferenceContext">:
      doc "The schema types, primitive types, and data types of the graph"
      inferenceContext]

typeSubst :: Binding
typeSubst = define "TypeSubst" $
  doc "A substitution of type variables for types" $
  T.wrap $ T.map Core.name Core.type_
