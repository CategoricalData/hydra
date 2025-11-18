module Hydra.Dsl.Meta.Typing where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


inferenceContext
  :: TTerm (M.Map Name TypeScheme)
  -> TTerm (M.Map Name TypeScheme)
  -> TTerm (M.Map Name TypeScheme)
  -> TTerm Bool
  -> TTerm InferenceContext
inferenceContext schemaTypes primitiveTypes dataTypes debug = Phantoms.record _InferenceContext [
  _InferenceContext_schemaTypes>>: schemaTypes,
  _InferenceContext_primitiveTypes>>: primitiveTypes,
  _InferenceContext_dataTypes>>: dataTypes,
  _InferenceContext_debug>>: debug]

inferenceContextSchemaTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextSchemaTypes ic = Phantoms.project _InferenceContext _InferenceContext_schemaTypes @@ ic

inferenceContextPrimitiveTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextPrimitiveTypes ic = Phantoms.project _InferenceContext _InferenceContext_primitiveTypes @@ ic

inferenceContextDataTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextDataTypes ic = Phantoms.project _InferenceContext _InferenceContext_dataTypes @@ ic

inferenceContextDebug :: TTerm InferenceContext -> TTerm Bool
inferenceContextDebug ic = Phantoms.project _InferenceContext _InferenceContext_debug @@ ic

inferenceContextWithDataTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme) -> TTerm InferenceContext
inferenceContextWithDataTypes ctx dataTypes = inferenceContext
  (Hydra.Dsl.Meta.Typing.inferenceContextSchemaTypes ctx)
  (Hydra.Dsl.Meta.Typing.inferenceContextPrimitiveTypes ctx)
  dataTypes
  (Hydra.Dsl.Meta.Typing.inferenceContextDebug ctx)

inferenceResult :: TTerm Term -> TTerm Type -> TTerm TypeSubst -> TTerm InferenceResult
inferenceResult term type_ subst = Phantoms.record _InferenceResult [
  _InferenceResult_term>>: term,
  _InferenceResult_type>>: type_,
  _InferenceResult_subst>>: subst]

inferenceResultTerm :: TTerm InferenceResult -> TTerm Term
inferenceResultTerm ir = Phantoms.project _InferenceResult _InferenceResult_term @@ ir

inferenceResultType :: TTerm InferenceResult -> TTerm Type
inferenceResultType ir = Phantoms.project _InferenceResult _InferenceResult_type @@ ir

inferenceResultSubst :: TTerm InferenceResult -> TTerm TypeSubst
inferenceResultSubst ir = Phantoms.project _InferenceResult _InferenceResult_subst @@ ir

termSubst :: TTerm (M.Map Name Term) -> TTerm TermSubst
termSubst = Phantoms.wrap _TermSubst

typeConstraint :: TTerm Type -> TTerm Type -> TTerm String -> TTerm TypeConstraint
typeConstraint t1 t2 comment = Phantoms.record _TypeConstraint [
  _TypeConstraint_left>>: t1,
  _TypeConstraint_right>>: t2,
  _TypeConstraint_comment>>: comment]

typeConstraintLeft :: TTerm TypeConstraint -> TTerm Type
typeConstraintLeft tc = Phantoms.project _TypeConstraint _TypeConstraint_left @@ tc

typeConstraintRight :: TTerm TypeConstraint -> TTerm Type
typeConstraintRight tc = Phantoms.project _TypeConstraint _TypeConstraint_right @@ tc

typeConstraintComment :: TTerm TypeConstraint -> TTerm String
typeConstraintComment tc = Phantoms.project _TypeConstraint _TypeConstraint_comment @@ tc

typeContext :: TTerm (M.Map Name Type) -> TTerm (S.Set Name) -> TTerm InferenceContext -> TTerm TypeContext
typeContext types variables inferenceContext = Phantoms.record _TypeContext [
  _TypeContext_types>>: types,
  _TypeContext_variables>>: variables,
  _TypeContext_inferenceContext>>: inferenceContext]

typeContextTypes :: TTerm TypeContext -> TTerm (M.Map Name Type)
typeContextTypes tc = Phantoms.project _TypeContext _TypeContext_types @@ tc

typeContextVariables :: TTerm TypeContext -> TTerm (S.Set Name)
typeContextVariables tc = Phantoms.project _TypeContext _TypeContext_variables @@ tc

typeContextInferenceContext :: TTerm TypeContext -> TTerm InferenceContext
typeContextInferenceContext tc = Phantoms.project _TypeContext _TypeContext_inferenceContext @@ tc

typeContextWithTypes :: TTerm TypeContext -> TTerm (M.Map Name Type) -> TTerm TypeContext
typeContextWithTypes ctx types = typeContext
  types
  (Hydra.Dsl.Meta.Typing.typeContextVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeContextWithVariables :: TTerm TypeContext -> TTerm (S.Set Name) -> TTerm TypeContext
typeContextWithVariables ctx variables = typeContext
  (Hydra.Dsl.Meta.Typing.typeContextTypes ctx)
  variables
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeSubst :: TTerm (M.Map Name Type) -> TTerm TypeSubst
typeSubst = Phantoms.wrap _TypeSubst

unTermSubst :: TTerm TermSubst -> TTerm (M.Map Name Term)
unTermSubst ts = unwrap _TermSubst @@ ts

unTypeSubst :: TTerm TypeSubst -> TTerm (M.Map Name Type)
unTypeSubst ts = unwrap _TypeSubst @@ ts
