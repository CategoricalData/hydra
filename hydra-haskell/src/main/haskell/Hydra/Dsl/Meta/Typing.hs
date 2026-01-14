{-# LANGUAGE FlexibleContexts #-}

-- | Meta-DSL for constructing typing-related terms (TypeScheme, TypeConstraints)

module Hydra.Dsl.Meta.Typing where

import Hydra.Kernel
import Hydra.Dsl.AsTerm
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
  -> TTerm (M.Map Name TypeVariableMetadata)
  -> TTerm Bool
  -> TTerm InferenceContext
inferenceContext schemaTypes primitiveTypes dataTypes classConstraints debug = Phantoms.record _InferenceContext [
  _InferenceContext_schemaTypes>>: schemaTypes,
  _InferenceContext_primitiveTypes>>: primitiveTypes,
  _InferenceContext_dataTypes>>: dataTypes,
  _InferenceContext_classConstraints>>: classConstraints,
  _InferenceContext_debug>>: debug]

inferenceContextSchemaTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextSchemaTypes ic = Phantoms.project _InferenceContext _InferenceContext_schemaTypes @@ ic

inferenceContextPrimitiveTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextPrimitiveTypes ic = Phantoms.project _InferenceContext _InferenceContext_primitiveTypes @@ ic

inferenceContextDataTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme)
inferenceContextDataTypes ic = Phantoms.project _InferenceContext _InferenceContext_dataTypes @@ ic

inferenceContextClassConstraints :: TTerm InferenceContext -> TTerm (M.Map Name TypeVariableMetadata)
inferenceContextClassConstraints ic = Phantoms.project _InferenceContext _InferenceContext_classConstraints @@ ic

inferenceContextDebug :: TTerm InferenceContext -> TTerm Bool
inferenceContextDebug ic = Phantoms.project _InferenceContext _InferenceContext_debug @@ ic

inferenceContextWithDataTypes :: TTerm InferenceContext -> TTerm (M.Map Name TypeScheme) -> TTerm InferenceContext
inferenceContextWithDataTypes ctx dataTypes = inferenceContext
  (Hydra.Dsl.Meta.Typing.inferenceContextSchemaTypes ctx)
  (Hydra.Dsl.Meta.Typing.inferenceContextPrimitiveTypes ctx)
  dataTypes
  (Hydra.Dsl.Meta.Typing.inferenceContextClassConstraints ctx)
  (Hydra.Dsl.Meta.Typing.inferenceContextDebug ctx)

inferenceContextWithClassConstraints :: TTerm InferenceContext -> TTerm (M.Map Name TypeVariableMetadata) -> TTerm InferenceContext
inferenceContextWithClassConstraints ctx classConstraints = inferenceContext
  (Hydra.Dsl.Meta.Typing.inferenceContextSchemaTypes ctx)
  (Hydra.Dsl.Meta.Typing.inferenceContextPrimitiveTypes ctx)
  (Hydra.Dsl.Meta.Typing.inferenceContextDataTypes ctx)
  classConstraints
  (Hydra.Dsl.Meta.Typing.inferenceContextDebug ctx)

inferenceResult :: AsTerm t TypeSubst => TTerm Term -> TTerm Type -> t -> TTerm (M.Map Name TypeVariableMetadata) -> TTerm InferenceResult
inferenceResult term type_ subst classConstraints = Phantoms.record _InferenceResult [
  _InferenceResult_term>>: term,
  _InferenceResult_type>>: type_,
  _InferenceResult_subst>>: asTerm subst,
  _InferenceResult_classConstraints>>: classConstraints]

inferenceResultTerm :: TTerm InferenceResult -> TTerm Term
inferenceResultTerm ir = Phantoms.project _InferenceResult _InferenceResult_term @@ ir

inferenceResultType :: TTerm InferenceResult -> TTerm Type
inferenceResultType ir = Phantoms.project _InferenceResult _InferenceResult_type @@ ir

inferenceResultSubst :: TTerm InferenceResult -> TTerm TypeSubst
inferenceResultSubst ir = Phantoms.project _InferenceResult _InferenceResult_subst @@ ir

inferenceResultClassConstraints :: TTerm InferenceResult -> TTerm (M.Map Name TypeVariableMetadata)
inferenceResultClassConstraints ir = Phantoms.project _InferenceResult _InferenceResult_classConstraints @@ ir

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

typeContext :: TTerm (M.Map Name Type) -> TTerm (M.Map Name Term) -> TTerm (S.Set Name) -> TTerm (S.Set Name) -> TTerm InferenceContext -> TTerm TypeContext
typeContext types metadata typeVariables lambdaVariables inferenceContext = Phantoms.record _TypeContext [
  _TypeContext_types>>: types,
  _TypeContext_metadata>>: metadata,
  _TypeContext_typeVariables>>: typeVariables,
  _TypeContext_lambdaVariables>>: lambdaVariables,
  _TypeContext_inferenceContext>>: inferenceContext]

typeContextTypes :: TTerm TypeContext -> TTerm (M.Map Name Type)
typeContextTypes tc = Phantoms.project _TypeContext _TypeContext_types @@ tc

typeContextMetadata :: TTerm TypeContext -> TTerm (M.Map Name Term)
typeContextMetadata tc = Phantoms.project _TypeContext _TypeContext_metadata @@ tc

typeContextTypeVariables :: TTerm TypeContext -> TTerm (S.Set Name)
typeContextTypeVariables tc = Phantoms.project _TypeContext _TypeContext_typeVariables @@ tc

typeContextLambdaVariables :: TTerm TypeContext -> TTerm (S.Set Name)
typeContextLambdaVariables tc = Phantoms.project _TypeContext _TypeContext_lambdaVariables @@ tc

typeContextInferenceContext :: TTerm TypeContext -> TTerm InferenceContext
typeContextInferenceContext tc = Phantoms.project _TypeContext _TypeContext_inferenceContext @@ tc

typeContextWithTypes :: TTerm TypeContext -> TTerm (M.Map Name Type) -> TTerm TypeContext
typeContextWithTypes ctx types = typeContext
  types
  (Hydra.Dsl.Meta.Typing.typeContextMetadata ctx)
  (Hydra.Dsl.Meta.Typing.typeContextTypeVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextLambdaVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeContextWithMetadata :: TTerm TypeContext -> TTerm (M.Map Name Term) -> TTerm TypeContext
typeContextWithMetadata ctx metadata = typeContext
  (Hydra.Dsl.Meta.Typing.typeContextTypes ctx)
  metadata
  (Hydra.Dsl.Meta.Typing.typeContextTypeVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextLambdaVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeContextWithTypeVariables :: TTerm TypeContext -> TTerm (S.Set Name) -> TTerm TypeContext
typeContextWithTypeVariables ctx typeVariables = typeContext
  (Hydra.Dsl.Meta.Typing.typeContextTypes ctx)
  (Hydra.Dsl.Meta.Typing.typeContextMetadata ctx)
  typeVariables
  (Hydra.Dsl.Meta.Typing.typeContextLambdaVariables ctx)
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeContextWithLambdaVariables :: TTerm TypeContext -> TTerm (S.Set Name) -> TTerm TypeContext
typeContextWithLambdaVariables ctx lambdaVariables = typeContext
  (Hydra.Dsl.Meta.Typing.typeContextTypes ctx)
  (Hydra.Dsl.Meta.Typing.typeContextMetadata ctx)
  (Hydra.Dsl.Meta.Typing.typeContextTypeVariables ctx)
  lambdaVariables
  (Hydra.Dsl.Meta.Typing.typeContextInferenceContext ctx)

typeSubst :: TTerm (M.Map Name Type) -> TTerm TypeSubst
typeSubst = Phantoms.wrap _TypeSubst

unTermSubst :: TTerm TermSubst -> TTerm (M.Map Name Term)
unTermSubst ts = unwrap _TermSubst @@ ts

unTypeSubst :: TTerm TypeSubst -> TTerm (M.Map Name Type)
unTypeSubst ts = unwrap _TypeSubst @@ ts
