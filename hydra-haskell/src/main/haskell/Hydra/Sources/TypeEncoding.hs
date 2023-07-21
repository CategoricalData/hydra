{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.TypeEncoding (typeEncodingModule) where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.TermEncoding
import Hydra.Dsl.Terms
import Hydra.Sources.Libraries
import Hydra.Sources.TermEncoding
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Types as Types


typeEncodingModule :: Module Kv
typeEncodingModule = Module (Namespace "hydra/typeEncoding") elements [hydraCoreModule] $
    Just "Implementation of LambdaGraph's epsilon encoding, which maps types to terms and back"
  where
   elements = [
     Base.el epsilonEncodeAnnotatedTypeDef,
     Base.el epsilonEncodeApplicationTypeDef,
     Base.el epsilonEncodeFieldNameDef, -- TODO: reuse sigma encoding?
     Base.el epsilonEncodeFieldTypeDef,
     Base.el epsilonEncodeFloatTypeDef,
     Base.el epsilonEncodeFunctionTypeDef,
     Base.el epsilonEncodeIntegerTypeDef,
     Base.el epsilonEncodeLambdaTypeDef,
     Base.el epsilonEncodeLiteralTypeDef,
     Base.el epsilonEncodeMapTypeDef,
     Base.el epsilonEncodeNameDef, -- TODO: reuse sigma encoding?
     Base.el epsilonEncodeNominalTypeDef,
     Base.el epsilonEncodeRowTypeDef,
     Base.el epsilonEncodeTypeDef]

typeEncodingDefinition :: String -> Type Kv -> Term Kv -> Definition x
typeEncodingDefinition label dom term = Base.definitionInModule typeEncodingModule ("epsilonEncode" ++ label) $
  Base.function dom termA $ Datum term

annotatedTypeA = Types.apply (Types.apply (TypeVariable _Annotated) typeA) (Types.var "a") :: Type a
applicationTypeA = Types.apply (TypeVariable _ApplicationType) (Types.var "a") :: Type a
fieldTypeA = Types.apply (TypeVariable _FieldType) (Types.var "a") :: Type a
functionTypeA = Types.apply (TypeVariable _FunctionType) (Types.var "a") :: Type a
lambdaTypeA = Types.apply (TypeVariable _LambdaType) (Types.var "a") :: Type a
mapTypeA = Types.apply (TypeVariable _MapType) (Types.var "a") :: Type a
nominalTypeA = Types.apply (TypeVariable _Nominal) typeA :: Type a
rowTypeA = Types.apply (TypeVariable _RowType) (Types.var "a") :: Type a

epsilonEncodeAnnotatedTypeDef :: Definition (Annotated (Type a) a -> Term a)
epsilonEncodeAnnotatedTypeDef = typeEncodingDefinition "AnnotatedType" annotatedTypeA $
  lambda "at" $ variant _Term _Term_annotated $ record _Annotated [
    Field _Annotated_subject $ ref epsilonEncodeTypeDef @@ (project _Annotated _Annotated_subject @@ var "at"),
    Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "at"]

epsilonEncodeApplicationTypeDef :: Definition (ApplicationType a -> Term a)
epsilonEncodeApplicationTypeDef = typeEncodingDefinition "ApplicationType" applicationTypeA $
  lambda "at" $ encodedRecord _ApplicationType [
    (_ApplicationType_function, ref epsilonEncodeTypeDef @@ (project _ApplicationType _ApplicationType_function @@ var "at")),
    (_ApplicationType_argument, ref epsilonEncodeTypeDef @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))]

epsilonEncodeFieldNameDef :: Definition (FieldName -> Term a)
epsilonEncodeFieldNameDef = typeEncodingDefinition "FieldName" (TypeVariable _FieldName) $
    lambda "fn" $ encodedString $ (unwrap _FieldName @@ var "fn")

epsilonEncodeFieldTypeDef :: Definition (FieldType a -> Term a)
epsilonEncodeFieldTypeDef = typeEncodingDefinition "FieldType" fieldTypeA $
  lambda "ft" $ encodedRecord _FieldType [
    (_FieldType_name, ref epsilonEncodeFieldNameDef @@ (project _FieldType _FieldType_name @@ var "ft")),
    (_FieldType_type, ref epsilonEncodeTypeDef @@ (project _FieldType _FieldType_type @@ var "ft"))]

epsilonEncodeFloatTypeDef :: Definition (FloatType -> Term a)
epsilonEncodeFloatTypeDef = typeEncodingDefinition "FloatType" (TypeVariable _FloatType) $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = Field fname $ constant $ sigmaEncodeTerm $ unitVariant _FloatType fname

epsilonEncodeFunctionTypeDef :: Definition (FunctionType a -> Term a)
epsilonEncodeFunctionTypeDef = typeEncodingDefinition "FunctionType" functionTypeA $
  lambda "ft" $ encodedRecord _FunctionType [
    (_FunctionType_domain, ref epsilonEncodeTypeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft")),
    (_FunctionType_codomain, ref epsilonEncodeTypeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft"))]

epsilonEncodeIntegerTypeDef :: Definition (IntegerType -> Term a)
epsilonEncodeIntegerTypeDef = typeEncodingDefinition "IntegerType" (TypeVariable _IntegerType) $
    match _IntegerType Nothing (cs <$> [
      _IntegerType_bigint,
      _IntegerType_int8,
      _IntegerType_int16,
      _IntegerType_int32,
      _IntegerType_int64,
      _IntegerType_uint8,
      _IntegerType_uint16,
      _IntegerType_uint32,
      _IntegerType_uint64])
  where
    cs fname = Field fname $ constant $ sigmaEncodeTerm $ unitVariant _IntegerType fname

epsilonEncodeLambdaTypeDef :: Definition (LambdaType a -> Term a)
epsilonEncodeLambdaTypeDef = typeEncodingDefinition "LambdaType" lambdaTypeA $
  lambda "lt" $ encodedRecord _LambdaType [
    (_LambdaType_parameter, ref epsilonEncodeNameDef @@ (project _LambdaType _LambdaType_parameter @@ var "lt")),
    (_LambdaType_body, ref epsilonEncodeTypeDef @@ (project _LambdaType _LambdaType_body @@ var "lt"))]

epsilonEncodeLiteralTypeDef :: Definition (LiteralType -> Term a)
epsilonEncodeLiteralTypeDef = typeEncodingDefinition "LiteralType" (TypeVariable _LiteralType) $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float epsilonEncodeFloatTypeDef,
    cs _LiteralType_integer epsilonEncodeIntegerTypeDef,
    csunit _LiteralType_string]
  where
    cs fname fun = Field fname $ lambda "v" $ encodedVariant _LiteralType fname (ref fun @@ var "v")
    csunit fname = Field fname $ constant $ sigmaEncodeTerm $ variant _LiteralType fname unit

epsilonEncodeMapTypeDef :: Definition (MapType a -> Term a)
epsilonEncodeMapTypeDef = typeEncodingDefinition "MapType" mapTypeA $
    lambda "mt" $ encodedRecord _MapType [
      (_MapType_keys, ref epsilonEncodeTypeDef @@ (project _MapType _MapType_keys @@ var "mt")),
      (_MapType_values, ref epsilonEncodeTypeDef @@ (project _MapType _MapType_values @@ var "mt"))]

epsilonEncodeNameDef :: Definition (Name -> Term a)
epsilonEncodeNameDef = typeEncodingDefinition "Name" (TypeVariable _Name) $
    lambda "name" $ encodedString $ (unwrap _Name @@ var "name")

epsilonEncodeNominalTypeDef :: Definition (Nominal (Type a) -> Term a)
epsilonEncodeNominalTypeDef = typeEncodingDefinition "NominalType" nominalTypeA $
  lambda "nt" $ encodedRecord _Nominal [
    (_Nominal_typeName, ref epsilonEncodeNameDef @@ (project _Nominal _Nominal_typeName @@ var "nt")),
    (_Nominal_object, ref epsilonEncodeTypeDef @@ (project _Nominal _Nominal_object @@ var "nt"))]

epsilonEncodeRowTypeDef :: Definition (RowType a -> Term a)
epsilonEncodeRowTypeDef = typeEncodingDefinition "RowType" rowTypeA $
  lambda "rt" $ encodedRecord _RowType [
    (_RowType_typeName, ref epsilonEncodeNameDef @@ (project _RowType _RowType_typeName @@ var "rt")),
    (_RowType_extends, encodedOptional (primitive _optionals_map @@ ref epsilonEncodeNameDef @@ (project _RowType _RowType_extends @@ var "rt"))),
    (_RowType_fields, encodedList (primitive _lists_map @@ ref epsilonEncodeFieldTypeDef @@ (project _RowType _RowType_fields @@ var "rt")))]

epsilonEncodeTypeDef :: Definition (Type a -> Term a)
epsilonEncodeTypeDef = typeEncodingDefinition "Type" typeA $
  match _Type Nothing [
    Field _Type_annotated $ lambda "v" $ variant _Term _Term_annotated $ record _Annotated [
      Field _Annotated_subject $ ref epsilonEncodeTypeDef @@ (project _Annotated _Annotated_subject @@ var "v"),
      Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "v"],
    csref _Type_application epsilonEncodeApplicationTypeDef,
    csref _Type_function epsilonEncodeFunctionTypeDef,
    csref _Type_lambda epsilonEncodeLambdaTypeDef,
    csref _Type_list epsilonEncodeTypeDef,
    csref _Type_literal epsilonEncodeLiteralTypeDef,
    csref _Type_map epsilonEncodeMapTypeDef,
    csref _Type_optional epsilonEncodeTypeDef,
    cs _Type_product $ encodedList $ primitive _lists_map @@ ref epsilonEncodeTypeDef @@ var "v",
    csref _Type_record epsilonEncodeRowTypeDef,
    csref _Type_set epsilonEncodeTypeDef,
    csref _Type_stream epsilonEncodeTypeDef,
    cs _Type_sum $ encodedList $ primitive _lists_map @@ ref epsilonEncodeTypeDef @@ var "v",
    csref _Type_union epsilonEncodeRowTypeDef,
    csref _Type_variable epsilonEncodeNameDef,
    csref _Type_wrap epsilonEncodeNominalTypeDef]
  where
    cs fname term = Field fname $ lambda "v" $ encodedVariant _Type fname term
    csref fname fun = cs fname (ref fun @@ var "v")
