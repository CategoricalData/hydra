module Hydra.Sources.Kernel.Types.Typing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Context as Context


ns :: Namespace
ns = Namespace "hydra.typing"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.ns, Context.ns] [Core.ns, Context.ns] $
    Just "Types supporting type inference and type reconstruction."
  where
    elements = [
      functionStructure,
      inferenceResult,
      termSubst,
      typeConstraint,
      typeSubst]

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
      typeSubst,
    "classConstraints">:
      doc "Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)" $
      T.map Core.name Core.typeVariableMetadata,
    "context">:
      doc "The updated context after inference (carries fresh variable state)" $
      Context.context]

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

typeSubst :: Binding
typeSubst = define "TypeSubst" $
  doc "A substitution of type variables for types" $
  T.wrap $ T.map Core.name Core.type_

functionStructure :: Binding
functionStructure = define "FunctionStructure" $
  doc ("A structured representation of a function term's components, replacing ad-hoc tuples."
    ++ " This captures all the information extracted from peeling lambdas, type lambdas, lets, and"
    ++ " type applications from a term.") $
  T.forAll "env" $
  T.record [
    "typeParams">:
      doc "Type parameters (from type lambdas)" $
      T.list Core.name,
    "params">:
      doc "Value parameters (from lambdas)" $
      T.list Core.name,
    "bindings">:
      doc "Let bindings accumulated from the term" $
      T.list Core.binding,
    "body">:
      doc "The body term after removing all lambdas, lets, etc."
      Core.term,
    "domains">:
      doc "Domain types of the value parameters" $
      T.list Core.type_,
    "codomain">:
      doc "The return type of the function (if type inference succeeded)" $
      T.optional Core.type_,
    "environment">:
      doc "Updated environment after processing all bindings" $
      T.variable "env"]
