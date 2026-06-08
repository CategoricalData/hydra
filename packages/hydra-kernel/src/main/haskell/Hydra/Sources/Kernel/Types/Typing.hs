module Hydra.Sources.Kernel.Types.Typing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Paths as Paths


ns :: ModuleName
ns = ModuleName "hydra.typing"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, Paths.ns],
            moduleMetadata = descriptionMetadata (Just "Types supporting type inference and type reconstruction.")}
  where
    definitions = [
      functionStructure,
      inferenceContext,
      inferenceResult,
      parameter,
      result,
      termSignature,
      termSubst,
      typeClass,
      typeConstraint,
      typeParameter,
      typeSubst]

functionStructure :: TypeDefinition
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

inferenceContext :: TypeDefinition
inferenceContext = define "InferenceContext" $
  doc ("State threaded through type inference: the fresh type variable counter"
    ++ " and the current subterm-path trace.") $
  T.record [
    "freshTypeVariableCount">:
      doc "Counter used to generate distinct fresh type variables during inference"
      T.int32,
    "trace">:
      doc ("The current subterm-path trace, accumulated backwards (head = most-recently-pushed step,"
        ++ " corresponding to the deepest point in the descent). At the moment an inference error is"
        ++ " constructed, the list is reversed and wrapped into a SubtermPath (root-to-leaf order)"
        ++ " and stamped onto the error.") $
      T.list Paths.subtermStep]

inferenceResult :: TypeDefinition
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
      T.map Core.name Core.typeVariableConstraints,
    "context">:
      doc "The updated InferenceContext after inference (carries fresh-variable counter and trace)" $
      inferenceContext]

parameter :: TypeDefinition
parameter = define "Parameter" $
  doc "A named, typed parameter of a term, with optional human-readable description and a flag indicating whether the parameter requires lazy evaluation by hosts which support it." $
  T.record [
    "name">:
      doc "The name of the parameter"
      Core.name,
    "description">:
      doc "An optional human-readable description of the parameter" $
      T.optional T.string,
    "type">:
      doc "The type of the parameter"
      Core.type_,
    "isLazy">:
      doc "Whether the parameter must be passed lazily (thunked) at call sites in hosts that distinguish strict from lazy evaluation"
      T.boolean]

result :: TypeDefinition
result = define "Result" $
  doc "The result of a term, consisting of a type and an optional human-readable description." $
  T.record [
    "description">:
      doc "An optional human-readable description of the result" $
      T.optional T.string,
    "type">:
      doc "The type of the result"
      Core.type_]

termSignature :: TypeDefinition
termSignature = define "TermSignature" $
  doc "A structured signature for a term: an ordered list of type parameters (with optional class constraints), an ordered list of value parameters, and a result. TermSignature is a richer view of TypeScheme: every TermSignature can be converted to a TypeScheme by erasing parameter names, descriptions, and laziness flags." $
  T.record [
    "typeParameters">:
      doc "The type parameters of the term, in order" $
      T.list typeParameter,
    "parameters">:
      doc "The value parameters of the term, in order" $
      T.list parameter,
    "result">:
      doc "The result of the term"
      result]

termSubst :: TypeDefinition
termSubst = define "TermSubst" $
  doc "A substitution of term variables for terms" $
  T.wrap $ T.map Core.name Core.term

typeClass :: TypeDefinition
typeClass = define "TypeClass" $
  doc ("A type class identifier together with a human-readable description."
    ++ " Type classes are referenced as bare names (e.g. the local name \"equality\") in"
    ++ " TypeVariableConstraints.classes; the canonical definitions live as term bindings"
    ++ " under hydra.classes.") $
  T.record [
    "description">:
      doc "A human-readable description of the type class"
      T.string]

typeConstraint :: TypeDefinition
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

typeParameter :: TypeDefinition
typeParameter = define "TypeParameter" $
  doc "A type parameter of a term, with an optional list of type class constraints" $
  T.record [
    "name">:
      doc "The name of the type parameter"
      Core.name,
    "constraints">:
      doc "Any type class constraints on the type parameter" $
      T.list Core.typeClassConstraint]

typeSubst :: TypeDefinition
typeSubst = define "TypeSubst" $
  doc "A substitution of type variables for types" $
  T.wrap $ T.map Core.name Core.type_
