module Hydra.Dsl.Typing where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
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
inferenceContext schemaTypes primitiveTypes dataTypes debug = Base.record _InferenceContext [
  _InferenceContext_schemaTypes>>: schemaTypes,
  _InferenceContext_primitiveTypes>>: primitiveTypes,
  _InferenceContext_dataTypes>>: dataTypes,
  _InferenceContext_debug>>: debug]

inferenceContextSchemaTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextSchemaTypes = Base.project _InferenceContext _InferenceContext_schemaTypes

inferenceContextPrimitiveTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextPrimitiveTypes = Base.project _InferenceContext _InferenceContext_primitiveTypes

inferenceContextDataTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextDataTypes = Base.project _InferenceContext _InferenceContext_dataTypes

inferenceContextDebug :: TTerm (InferenceContext -> Bool)
inferenceContextDebug = Base.project _InferenceContext _InferenceContext_debug

termSubst :: TTerm (M.Map Name Term) -> TTerm TermSubst
termSubst = Base.wrap _TermSubst

typeConstraint :: TTerm Type -> TTerm Type -> TTerm String -> TTerm TypeConstraint
typeConstraint t1 t2 comment = Base.record _TypeConstraint [
  _TypeConstraint_left>>: t1,
  _TypeConstraint_right>>: t2,
  _TypeConstraint_comment>>: comment]

typeConstraintLeft :: TTerm (TypeConstraint -> Type)
typeConstraintLeft = Base.project _TypeConstraint _TypeConstraint_left

typeConstraintRight :: TTerm (TypeConstraint -> Type)
typeConstraintRight = Base.project _TypeConstraint _TypeConstraint_right

typeConstraintComment :: TTerm (TypeConstraint -> String)
typeConstraintComment = Base.project _TypeConstraint _TypeConstraint_comment

typeSubst :: TTerm (M.Map Name Type) -> TTerm TypeSubst
typeSubst = Base.wrap _TypeSubst

unTermSubst :: TTerm (TermSubst -> M.Map Name Term)
unTermSubst = unwrap _TermSubst

unTypeSubst :: TTerm (TypeSubst -> M.Map Name Type)
unTypeSubst = unwrap _TypeSubst

--

inferenceContextWithDataTypes :: TTerm (M.Map Name TypeScheme) -> TTerm InferenceContext -> TTerm InferenceContext
inferenceContextWithDataTypes dataTypes ctx = inferenceContext
  (Hydra.Dsl.Typing.inferenceContextSchemaTypes @@ ctx)
  (Hydra.Dsl.Typing.inferenceContextPrimitiveTypes @@ ctx)
  dataTypes
  (Hydra.Dsl.Typing.inferenceContextDebug @@ ctx)
