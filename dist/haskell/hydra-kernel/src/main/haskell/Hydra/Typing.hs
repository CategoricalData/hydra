-- Note: this is an automatically generated file. Do not edit.
-- | Types supporting type inference and type reconstruction.

module Hydra.Typing where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | A structured representation of a function term's components, replacing ad-hoc tuples. This captures all the information extracted from peeling lambdas, type lambdas, lets, and type applications from a term.
data FunctionStructure env =
  FunctionStructure {
    -- | Type parameters (from type lambdas)
    functionStructureTypeParams :: [Core.Name],
    -- | Value parameters (from lambdas)
    functionStructureParams :: [Core.Name],
    -- | Let bindings accumulated from the term
    functionStructureBindings :: [Core.Binding],
    -- | The body term after removing all lambdas, lets, etc.
    functionStructureBody :: Core.Term,
    -- | Domain types of the value parameters
    functionStructureDomains :: [Core.Type],
    -- | The return type of the function (if type inference succeeded)
    functionStructureCodomain :: (Maybe Core.Type),
    -- | Updated environment after processing all bindings
    functionStructureEnvironment :: env}
  deriving (Eq, Ord, Read, Show)
_FunctionStructure = Core.Name "hydra.typing.FunctionStructure"
_FunctionStructure_typeParams = Core.Name "typeParams"
_FunctionStructure_params = Core.Name "params"
_FunctionStructure_bindings = Core.Name "bindings"
_FunctionStructure_body = Core.Name "body"
_FunctionStructure_domains = Core.Name "domains"
_FunctionStructure_codomain = Core.Name "codomain"
_FunctionStructure_environment = Core.Name "environment"
-- | The result of applying inference rules to a term.
data InferenceResult =
  InferenceResult {
    -- | The term which was inferred
    inferenceResultTerm :: Core.Term,
    -- | The inferred type of the term
    inferenceResultType :: Core.Type,
    -- | The type substitution resulting from unification
    inferenceResultSubst :: TypeSubst,
    -- | Class constraints discovered during inference (e.g., Ord constraints from Map.lookup)
    inferenceResultClassConstraints :: (M.Map Core.Name Core.TypeVariableMetadata),
    -- | The updated context after inference (carries fresh variable state)
    inferenceResultContext :: Context.Context}
  deriving (Eq, Ord, Read, Show)
_InferenceResult = Core.Name "hydra.typing.InferenceResult"
_InferenceResult_term = Core.Name "term"
_InferenceResult_type = Core.Name "type"
_InferenceResult_subst = Core.Name "subst"
_InferenceResult_classConstraints = Core.Name "classConstraints"
_InferenceResult_context = Core.Name "context"
-- | A named, typed parameter of a term, with optional human-readable description and a flag indicating whether the parameter requires lazy evaluation by hosts which support it.
data Parameter =
  Parameter {
    -- | The name of the parameter
    parameterName :: Core.Name,
    -- | An optional human-readable description of the parameter
    parameterDescription :: (Maybe String),
    -- | The type of the parameter
    parameterType :: Core.Type,
    -- | Whether the parameter must be passed lazily (thunked) at call sites in hosts that distinguish strict from lazy evaluation
    parameterIsLazy :: Bool}
  deriving (Eq, Ord, Read, Show)
_Parameter = Core.Name "hydra.typing.Parameter"
_Parameter_name = Core.Name "name"
_Parameter_description = Core.Name "description"
_Parameter_type = Core.Name "type"
_Parameter_isLazy = Core.Name "isLazy"
-- | The result of a term, consisting of a type and an optional human-readable description.
data Result =
  Result {
    -- | An optional human-readable description of the result
    resultDescription :: (Maybe String),
    -- | The type of the result
    resultType :: Core.Type}
  deriving (Eq, Ord, Read, Show)
_Result = Core.Name "hydra.typing.Result"
_Result_description = Core.Name "description"
_Result_type = Core.Name "type"
-- | A structured signature for a term: an ordered list of type parameters (with optional class constraints), an ordered list of value parameters, and a result. TermSignature is a richer view of TypeScheme: every TermSignature can be converted to a TypeScheme by erasing parameter names, descriptions, and laziness flags.
data TermSignature =
  TermSignature {
    -- | The type parameters of the term, in order
    termSignatureTypeParameters :: [TypeParameter],
    -- | The value parameters of the term, in order
    termSignatureParameters :: [Parameter],
    -- | The result of the term
    termSignatureResult :: Result}
  deriving (Eq, Ord, Read, Show)
_TermSignature = Core.Name "hydra.typing.TermSignature"
_TermSignature_typeParameters = Core.Name "typeParameters"
_TermSignature_parameters = Core.Name "parameters"
_TermSignature_result = Core.Name "result"
-- | A substitution of term variables for terms
newtype TermSubst =
  TermSubst {
    unTermSubst :: (M.Map Core.Name Core.Term)}
  deriving (Eq, Ord, Read, Show)
_TermSubst = Core.Name "hydra.typing.TermSubst"
-- | A type class identifier together with a human-readable description. Type classes are referenced as bare names (e.g. the local name "equality") in TypeVariableMetadata.classes; the canonical definitions live as term bindings under hydra.classes.
data TypeClass =
  TypeClass {
    -- | A human-readable description of the type class
    typeClassDescription :: String}
  deriving (Eq, Ord, Read, Show)
_TypeClass = Core.Name "hydra.typing.TypeClass"
_TypeClass_description = Core.Name "description"
-- | An assertion that two types can be unified into a single type
data TypeConstraint =
  TypeConstraint {
    -- | The left-hand side of the constraint
    typeConstraintLeft :: Core.Type,
    -- | The right-hand side of the constraint
    typeConstraintRight :: Core.Type,
    -- | A description of the type constraint which may be used for tracing or debugging
    typeConstraintComment :: String}
  deriving (Eq, Ord, Read, Show)
_TypeConstraint = Core.Name "hydra.typing.TypeConstraint"
_TypeConstraint_left = Core.Name "left"
_TypeConstraint_right = Core.Name "right"
_TypeConstraint_comment = Core.Name "comment"
-- | A type parameter of a term, with an optional list of type class constraints
data TypeParameter =
  TypeParameter {
    -- | The name of the type parameter
    typeParameterName :: Core.Name,
    -- | Any type class constraints on the type parameter
    typeParameterConstraints :: [Core.TypeClassConstraint]}
  deriving (Eq, Ord, Read, Show)
_TypeParameter = Core.Name "hydra.typing.TypeParameter"
_TypeParameter_name = Core.Name "name"
_TypeParameter_constraints = Core.Name "constraints"
-- | A substitution of type variables for types
newtype TypeSubst =
  TypeSubst {
    unTypeSubst :: (M.Map Core.Name Core.Type)}
  deriving (Eq, Ord, Read, Show)
_TypeSubst = Core.Name "hydra.typing.TypeSubst"
