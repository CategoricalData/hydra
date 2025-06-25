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

inferenceContextSchemaTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextSchemaTypes ic = Phantoms.project _InferenceContext _InferenceContext_schemaTypes @@ ic

inferenceContextPrimitiveTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextPrimitiveTypes ic = Phantoms.project _InferenceContext _InferenceContext_primitiveTypes @@ ic

inferenceContextDataTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextDataTypes ic = Phantoms.project _InferenceContext _InferenceContext_dataTypes @@ ic

inferenceContextDebug :: TTerm InferenceContext -> TTerm Bool
inferenceContextDebug ic = Phantoms.project _InferenceContext _InferenceContext_debug @@ ic

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

typeSubst :: TTerm (M.Map Name Type) -> TTerm TypeSubst
typeSubst = Phantoms.wrap _TypeSubst

unTermSubst :: TTerm TermSubst -> TTerm (M.Map Name Term)
unTermSubst ts = unwrap _TermSubst @@ ts

unTypeSubst :: TTerm TypeSubst -> TTerm (M.Map Name Type)
unTypeSubst ts = unwrap _TypeSubst @@ ts

--

inferenceContextWithDataTypes :: TTerm (M.Map Name TypeScheme) -> TTerm InferenceContext -> TTerm InferenceContext
inferenceContextWithDataTypes dataTypes ctx = inferenceContext
  (Hydra.Dsl.Typing.inferenceContextSchemaTypes ctx)
  (Hydra.Dsl.Typing.inferenceContextPrimitiveTypes ctx)
  dataTypes
  (Hydra.Dsl.Typing.inferenceContextDebug ctx)
