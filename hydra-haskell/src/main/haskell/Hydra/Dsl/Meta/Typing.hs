{-# LANGUAGE FlexibleContexts #-}

-- | Meta-DSL for constructing typing-related terms (TypeScheme, TypeConstraints)

module Hydra.Dsl.Meta.Typing where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
import Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


inferenceResult :: AsTerm t TypeSubst => TTerm Term -> TTerm Type -> t -> TTerm (M.Map Name TypeVariableMetadata) -> TTerm Context -> TTerm InferenceResult
inferenceResult term type_ subst classConstraints context = Phantoms.record _InferenceResult [
  _InferenceResult_term>>: term,
  _InferenceResult_type>>: type_,
  _InferenceResult_subst>>: asTerm subst,
  _InferenceResult_classConstraints>>: classConstraints,
  _InferenceResult_context>>: context]

inferenceResultTerm :: TTerm InferenceResult -> TTerm Term
inferenceResultTerm ir = Phantoms.project _InferenceResult _InferenceResult_term @@ ir

inferenceResultType :: TTerm InferenceResult -> TTerm Type
inferenceResultType ir = Phantoms.project _InferenceResult _InferenceResult_type @@ ir

inferenceResultSubst :: TTerm InferenceResult -> TTerm TypeSubst
inferenceResultSubst ir = Phantoms.project _InferenceResult _InferenceResult_subst @@ ir

inferenceResultClassConstraints :: TTerm InferenceResult -> TTerm (M.Map Name TypeVariableMetadata)
inferenceResultClassConstraints ir = Phantoms.project _InferenceResult _InferenceResult_classConstraints @@ ir

inferenceResultContext :: TTerm InferenceResult -> TTerm Context
inferenceResultContext ir = Phantoms.project _InferenceResult _InferenceResult_context @@ ir

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
