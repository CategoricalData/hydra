module Hydra.Dsl.Variants where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms
import Hydra.Variants

import qualified Data.Map as M
import qualified Data.Maybe as Y


eliminationVariant :: EliminationVariant -> TTerm EliminationVariant
eliminationVariant v = unitVariant _EliminationVariant $ case v of
  EliminationVariantProduct -> _EliminationVariant_product
  EliminationVariantRecord -> _EliminationVariant_record
  EliminationVariantUnion -> _EliminationVariant_union
  EliminationVariantWrap -> _EliminationVariant_wrap

eliminationVariantProduct :: TTerm EliminationVariant
eliminationVariantProduct = unitVariant _EliminationVariant _EliminationVariant_product

eliminationVariantRecord :: TTerm EliminationVariant
eliminationVariantRecord = unitVariant _EliminationVariant _EliminationVariant_record

eliminationVariantUnion :: TTerm EliminationVariant
eliminationVariantUnion = unitVariant _EliminationVariant _EliminationVariant_union

eliminationVariantWrap :: TTerm EliminationVariant
eliminationVariantWrap = unitVariant _EliminationVariant _EliminationVariant_wrap

functionVariant :: FunctionVariant -> TTerm FunctionVariant
functionVariant v = unitVariant _FunctionVariant $ case v of
  FunctionVariantElimination -> _FunctionVariant_elimination
  FunctionVariantLambda -> _FunctionVariant_lambda
  FunctionVariantPrimitive -> _FunctionVariant_primitive

functionVariantElimination :: TTerm FunctionVariant
functionVariantElimination = unitVariant _FunctionVariant _FunctionVariant_elimination

functionVariantLambda :: TTerm FunctionVariant
functionVariantLambda = unitVariant _FunctionVariant _FunctionVariant_lambda

functionVariantPrimitive :: TTerm FunctionVariant
functionVariantPrimitive = unitVariant _FunctionVariant _FunctionVariant_primitive

literalVariant :: LiteralVariant -> TTerm LiteralVariant
literalVariant v = unitVariant _LiteralVariant $ case v of
  LiteralVariantBinary -> _LiteralVariant_binary
  LiteralVariantBoolean -> _LiteralVariant_boolean
  LiteralVariantFloat -> _LiteralVariant_float
  LiteralVariantInteger -> _LiteralVariant_integer
  LiteralVariantString -> _LiteralVariant_string

literalVariantBinary :: TTerm LiteralVariant
literalVariantBinary = unitVariant _LiteralVariant _LiteralVariant_binary

literalVariantBoolean :: TTerm LiteralVariant
literalVariantBoolean = unitVariant _LiteralVariant _LiteralVariant_boolean

literalVariantFloat :: TTerm LiteralVariant
literalVariantFloat = unitVariant _LiteralVariant _LiteralVariant_float

literalVariantInteger :: TTerm LiteralVariant
literalVariantInteger = unitVariant _LiteralVariant _LiteralVariant_integer

literalVariantString :: TTerm LiteralVariant
literalVariantString = unitVariant _LiteralVariant _LiteralVariant_string

termVariant :: TermVariant -> TTerm TermVariant
termVariant v = unitVariant _TermVariant $ case v of
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
  TermVariantProduct -> _TermVariant_product
  TermVariantRecord -> _TermVariant_record
  TermVariantSet -> _TermVariant_set
  TermVariantSum -> _TermVariant_sum
  TermVariantTypeLambda -> _TermVariant_typeLambda
  TermVariantTypeApplication -> _TermVariant_typeApplication
  TermVariantUnion -> _TermVariant_union
  TermVariantUnit -> _TermVariant_unit
  TermVariantVariable -> _TermVariant_variable
  TermVariantWrap -> _TermVariant_wrap

termVariantAnnotated :: TTerm TermVariant
termVariantAnnotated = unitVariant _TermVariant _TermVariant_annotated

termVariantApplication :: TTerm TermVariant
termVariantApplication = unitVariant _TermVariant _TermVariant_application

termVariantEither :: TTerm TermVariant
termVariantEither = unitVariant _TermVariant _TermVariant_either

termVariantFunction :: TTerm TermVariant
termVariantFunction = unitVariant _TermVariant _TermVariant_function

termVariantLet :: TTerm TermVariant
termVariantLet = unitVariant _TermVariant _TermVariant_let

termVariantList :: TTerm TermVariant
termVariantList = unitVariant _TermVariant _TermVariant_list

termVariantLiteral :: TTerm TermVariant
termVariantLiteral = unitVariant _TermVariant _TermVariant_literal

termVariantMap :: TTerm TermVariant
termVariantMap = unitVariant _TermVariant _TermVariant_map

termVariantMaybe :: TTerm TermVariant
termVariantMaybe = unitVariant _TermVariant _TermVariant_maybe

termVariantPair :: TTerm TermVariant
termVariantPair = unitVariant _TermVariant _TermVariant_pair

termVariantProduct :: TTerm TermVariant
termVariantProduct = unitVariant _TermVariant _TermVariant_product

termVariantRecord :: TTerm TermVariant
termVariantRecord = unitVariant _TermVariant _TermVariant_record

termVariantSet :: TTerm TermVariant
termVariantSet = unitVariant _TermVariant _TermVariant_set

termVariantSum :: TTerm TermVariant
termVariantSum = unitVariant _TermVariant _TermVariant_sum

termVariantTypeLambda :: TTerm TermVariant
termVariantTypeLambda = unitVariant _TermVariant _TermVariant_typeLambda

termVariantTypeApplication :: TTerm TermVariant
termVariantTypeApplication = unitVariant _TermVariant _TermVariant_typeApplication

termVariantUnion :: TTerm TermVariant
termVariantUnion = unitVariant _TermVariant _TermVariant_union

termVariantUnit :: TTerm TermVariant
termVariantUnit = unitVariant _TermVariant _TermVariant_unit

termVariantVariable :: TTerm TermVariant
termVariantVariable = unitVariant _TermVariant _TermVariant_variable

termVariantWrap :: TTerm TermVariant
termVariantWrap = unitVariant _TermVariant _TermVariant_wrap

typeVariant :: TypeVariant -> TTerm TypeVariant
typeVariant v = unitVariant _TypeVariant $ case v of
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
  TypeVariantProduct -> _TypeVariant_product
  TypeVariantRecord -> _TypeVariant_record
  TypeVariantSet -> _TypeVariant_set
  TypeVariantUnion -> _TypeVariant_union
  TypeVariantUnit -> _TypeVariant_unit
  TypeVariantVariable -> _TypeVariant_variable
  TypeVariantWrap -> _TypeVariant_wrap

typeVariantAnnotated :: TTerm TypeVariant
typeVariantAnnotated = unitVariant _TypeVariant _TypeVariant_annotated

typeVariantApplication :: TTerm TypeVariant
typeVariantApplication = unitVariant _TypeVariant _TypeVariant_application

typeVariantEither :: TTerm TypeVariant
typeVariantEither = unitVariant _TypeVariant _TypeVariant_either

typeVariantFunction :: TTerm TypeVariant
typeVariantFunction = unitVariant _TypeVariant _TypeVariant_function

typeVariantForall :: TTerm TypeVariant
typeVariantForall = unitVariant _TypeVariant _TypeVariant_forall

typeVariantList :: TTerm TypeVariant
typeVariantList = unitVariant _TypeVariant _TypeVariant_list

typeVariantLiteral :: TTerm TypeVariant
typeVariantLiteral = unitVariant _TypeVariant _TypeVariant_literal

typeVariantMap :: TTerm TypeVariant
typeVariantMap = unitVariant _TypeVariant _TypeVariant_map

typeVariantMaybe :: TTerm TypeVariant
typeVariantMaybe = unitVariant _TypeVariant _TypeVariant_maybe

typeVariantPair :: TTerm TypeVariant
typeVariantPair = unitVariant _TypeVariant _TypeVariant_pair

typeVariantProduct :: TTerm TypeVariant
typeVariantProduct = unitVariant _TypeVariant _TypeVariant_product

typeVariantRecord :: TTerm TypeVariant
typeVariantRecord = unitVariant _TypeVariant _TypeVariant_record

typeVariantSet :: TTerm TypeVariant
typeVariantSet = unitVariant _TypeVariant _TypeVariant_set

typeVariantSum :: TTerm TypeVariant
typeVariantSum = unitVariant _TypeVariant _TypeVariant_sum

typeVariantUnion :: TTerm TypeVariant
typeVariantUnion = unitVariant _TypeVariant _TypeVariant_union

typeVariantUnit :: TTerm TypeVariant
typeVariantUnit = unitVariant _TypeVariant _TypeVariant_unit

typeVariantVariable :: TTerm TypeVariant
typeVariantVariable = unitVariant _TypeVariant _TypeVariant_variable

typeVariantWrap :: TTerm TypeVariant
typeVariantWrap = unitVariant _TypeVariant _TypeVariant_wrap
