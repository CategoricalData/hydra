module Hydra.Dsl.Typing where

import Hydra.Kernel
import Hydra.Dsl.Phantoms as Phantoms
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

inferenceContextSchemaTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextSchemaTypes = Phantoms.project _InferenceContext _InferenceContext_schemaTypes

inferenceContextPrimitiveTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextPrimitiveTypes = Phantoms.project _InferenceContext _InferenceContext_primitiveTypes

inferenceContextDataTypes :: TTerm (InferenceContext -> M.Map Name TypeScheme)
inferenceContextDataTypes = Phantoms.project _InferenceContext _InferenceContext_dataTypes

inferenceContextDebug :: TTerm (InferenceContext -> Bool)
inferenceContextDebug = Phantoms.project _InferenceContext _InferenceContext_debug

termSubst :: TTerm (M.Map Name Term) -> TTerm TermSubst
termSubst = Phantoms.wrap _TermSubst

typeConstraint :: TTerm Type -> TTerm Type -> TTerm String -> TTerm TypeConstraint
typeConstraint t1 t2 comment = Phantoms.record _TypeConstraint [
  _TypeConstraint_left>>: t1,
  _TypeConstraint_right>>: t2,
  _TypeConstraint_comment>>: comment]

typeConstraintLeft :: TTerm (TypeConstraint -> Type)
typeConstraintLeft = Phantoms.project _TypeConstraint _TypeConstraint_left

typeConstraintRight :: TTerm (TypeConstraint -> Type)
typeConstraintRight = Phantoms.project _TypeConstraint _TypeConstraint_right

typeConstraintComment :: TTerm (TypeConstraint -> String)
typeConstraintComment = Phantoms.project _TypeConstraint _TypeConstraint_comment

typeSubst :: TTerm (M.Map Name Type) -> TTerm TypeSubst
typeSubst = Phantoms.wrap _TypeSubst

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
