module Hydra.Dsl.Meta.Variants where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Variants

import qualified Data.Map as M
import qualified Data.Maybe as Y


eliminationVariant :: EliminationVariant -> TTerm EliminationVariant
eliminationVariant v = injectUnit _EliminationVariant $ case v of
  EliminationVariantRecord -> _EliminationVariant_record
  EliminationVariantUnion -> _EliminationVariant_union
  EliminationVariantWrap -> _EliminationVariant_wrap

eliminationVariantRecord :: TTerm EliminationVariant
eliminationVariantRecord = injectUnit _EliminationVariant _EliminationVariant_record

eliminationVariantUnion :: TTerm EliminationVariant
eliminationVariantUnion = injectUnit _EliminationVariant _EliminationVariant_union

eliminationVariantWrap :: TTerm EliminationVariant
eliminationVariantWrap = injectUnit _EliminationVariant _EliminationVariant_wrap

functionVariant :: FunctionVariant -> TTerm FunctionVariant
functionVariant v = injectUnit _FunctionVariant $ case v of
  FunctionVariantElimination -> _FunctionVariant_elimination
  FunctionVariantLambda -> _FunctionVariant_lambda
  FunctionVariantPrimitive -> _FunctionVariant_primitive

functionVariantElimination :: TTerm FunctionVariant
functionVariantElimination = injectUnit _FunctionVariant _FunctionVariant_elimination

functionVariantLambda :: TTerm FunctionVariant
functionVariantLambda = injectUnit _FunctionVariant _FunctionVariant_lambda

functionVariantPrimitive :: TTerm FunctionVariant
functionVariantPrimitive = injectUnit _FunctionVariant _FunctionVariant_primitive

literalVariant :: LiteralVariant -> TTerm LiteralVariant
literalVariant v = injectUnit _LiteralVariant $ case v of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

literalVariantBinary :: TTerm LiteralVariant
literalVariantBinary = injectUnit _LiteralVariant _LiteralVariant_binary

literalVariantBoolean :: TTerm LiteralVariant
literalVariantBoolean = injectUnit _LiteralVariant _LiteralVariant_boolean

literalVariantFloat :: TTerm LiteralVariant
literalVariantFloat = injectUnit _LiteralVariant _LiteralVariant_float

literalVariantInteger :: TTerm LiteralVariant
literalVariantInteger = injectUnit _LiteralVariant _LiteralVariant_integer

literalVariantString :: TTerm LiteralVariant
literalVariantString = injectUnit _LiteralVariant _LiteralVariant_string

termVariant :: TermVariant -> TTerm TermVariant
termVariant v = injectUnit _TermVariant $ case v of
  TermVariantAnnotated -> _TermVariant_annotated
  TermVariantApplication -> _TermVariant_application
  TermVariantEither -> _TermVariant_either
  TermVariantFunction -> _TermVariant_function
  TermVariantLet -> _TermVariant_let
  TermVariantList -> _TermVariant_list
  TermVariantLiteral -> _TermVariant_literal
  TermVariantMap -> _TermVariant_map
  TermVariantMaybe -> _TermVariant_maybe
  TermVariantPair -> _TermVariant_pair
  TermVariantRecord -> _TermVariant_record
  TermVariantSet -> _TermVariant_set
  TermVariantTypeLambda -> _TermVariant_typeLambda
  TermVariantTypeApplication -> _TermVariant_typeApplication
  TermVariantUnion -> _TermVariant_union
  TermVariantUnit -> _TermVariant_unit
  TermVariantVariable -> _TermVariant_variable
  TermVariantWrap -> _TermVariant_wrap

termVariantAnnotated :: TTerm TermVariant
termVariantAnnotated = injectUnit _TermVariant _TermVariant_annotated

termVariantApplication :: TTerm TermVariant
termVariantApplication = injectUnit _TermVariant _TermVariant_application

termVariantEither :: TTerm TermVariant
termVariantEither = injectUnit _TermVariant _TermVariant_either

termVariantFunction :: TTerm TermVariant
termVariantFunction = injectUnit _TermVariant _TermVariant_function

termVariantLet :: TTerm TermVariant
termVariantLet = injectUnit _TermVariant _TermVariant_let

termVariantList :: TTerm TermVariant
termVariantList = injectUnit _TermVariant _TermVariant_list

termVariantLiteral :: TTerm TermVariant
termVariantLiteral = injectUnit _TermVariant _TermVariant_literal

termVariantMap :: TTerm TermVariant
termVariantMap = injectUnit _TermVariant _TermVariant_map

termVariantMaybe :: TTerm TermVariant
termVariantMaybe = injectUnit _TermVariant _TermVariant_maybe

termVariantPair :: TTerm TermVariant
termVariantPair = injectUnit _TermVariant _TermVariant_pair

termVariantRecord :: TTerm TermVariant
termVariantRecord = injectUnit _TermVariant _TermVariant_record

termVariantSet :: TTerm TermVariant
termVariantSet = injectUnit _TermVariant _TermVariant_set

termVariantTypeLambda :: TTerm TermVariant
termVariantTypeLambda = injectUnit _TermVariant _TermVariant_typeLambda

termVariantTypeApplication :: TTerm TermVariant
termVariantTypeApplication = injectUnit _TermVariant _TermVariant_typeApplication

termVariantUnion :: TTerm TermVariant
termVariantUnion = injectUnit _TermVariant _TermVariant_union

termVariantUnit :: TTerm TermVariant
termVariantUnit = injectUnit _TermVariant _TermVariant_unit

termVariantVariable :: TTerm TermVariant
termVariantVariable = injectUnit _TermVariant _TermVariant_variable

termVariantWrap :: TTerm TermVariant
termVariantWrap = injectUnit _TermVariant _TermVariant_wrap

typeVariant :: TypeVariant -> TTerm TypeVariant
typeVariant v = injectUnit _TypeVariant $ case v of
  TypeVariantAnnotated -> _TypeVariant_annotated
  TypeVariantApplication -> _TypeVariant_application
  TypeVariantEither -> _TypeVariant_either
  TypeVariantFunction -> _TypeVariant_function
  TypeVariantForall -> _TypeVariant_forall
  TypeVariantList -> _TypeVariant_list
  TypeVariantLiteral -> _TypeVariant_literal
  TypeVariantMap -> _TypeVariant_map
  TypeVariantMaybe -> _TypeVariant_maybe
  TypeVariantPair -> _TypeVariant_pair
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUnit -> _TypeVariant_unit
  TypeVariantVariable -> _TypeVariant_variable
  TypeVariantWrap -> _TypeVariant_wrap

typeVariantAnnotated :: TTerm TypeVariant
typeVariantAnnotated = injectUnit _TypeVariant _TypeVariant_annotated

typeVariantApplication :: TTerm TypeVariant
typeVariantApplication = injectUnit _TypeVariant _TypeVariant_application

typeVariantEither :: TTerm TypeVariant
typeVariantEither = injectUnit _TypeVariant _TypeVariant_either

typeVariantFunction :: TTerm TypeVariant
typeVariantFunction = injectUnit _TypeVariant _TypeVariant_function

typeVariantForall :: TTerm TypeVariant
typeVariantForall = injectUnit _TypeVariant _TypeVariant_forall

typeVariantList :: TTerm TypeVariant
typeVariantList = injectUnit _TypeVariant _TypeVariant_list

typeVariantLiteral :: TTerm TypeVariant
typeVariantLiteral = injectUnit _TypeVariant _TypeVariant_literal

typeVariantMap :: TTerm TypeVariant
typeVariantMap = injectUnit _TypeVariant _TypeVariant_map

typeVariantMaybe :: TTerm TypeVariant
typeVariantMaybe = injectUnit _TypeVariant _TypeVariant_maybe

typeVariantPair :: TTerm TypeVariant
typeVariantPair = injectUnit _TypeVariant _TypeVariant_pair

typeVariantRecord :: TTerm TypeVariant
typeVariantRecord = injectUnit _TypeVariant _TypeVariant_record

typeVariantSet :: TTerm TypeVariant
typeVariantSet = injectUnit _TypeVariant _TypeVariant_set

typeVariantUnion :: TTerm TypeVariant
typeVariantUnion = injectUnit _TypeVariant _TypeVariant_union

typeVariantUnit :: TTerm TypeVariant
typeVariantUnit = injectUnit _TypeVariant _TypeVariant_unit

typeVariantVariable :: TTerm TypeVariant
typeVariantVariable = injectUnit _TypeVariant _TypeVariant_variable

typeVariantWrap :: TTerm TypeVariant
typeVariantWrap = injectUnit _TypeVariant _TypeVariant_wrap
