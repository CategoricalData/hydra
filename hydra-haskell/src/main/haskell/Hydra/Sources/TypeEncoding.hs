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
     Base.el epsilonEncodeFloatTypeDef,
     Base.el epsilonEncodeIntegerTypeDef
--      Base.el epsilonEncodeNameDef,
--      el epsilonEncodeLiteralTypeDef
--      el epsilonEncodeTypeDef,


     ]


typeEncodingDefinition :: String -> Type Kv -> Term Kv -> Definition x
typeEncodingDefinition label dom term = Base.definitionInModule typeEncodingModule ("eencode" ++ label) $
  Base.function dom termA $ Datum term









-- encodedFieldName :: FieldName -> Term a
-- encodedFieldName = TermWrap . Nominal _FieldName . TermLiteral . LiteralString . unFieldName
--
-- encodedName :: Name -> Term a
-- encodedName = TermWrap . Nominal _Name . TermLiteral . LiteralString . unName
--
-- -- epsilonEncodeNameDef :: Definition (Name -> Term a)
-- -- epsilonEncodeNameDef = typeEncodingDefinition "Name" $
-- --   lambda "n" $
--
-- encodedField :: Field a -> Term a
-- encodedField (Field fname term) = record _Field [
--   Field _Field_name $ encodedFieldName fname,
--   Field _Field_term $ encodedTerm term]
--
--
-- encodedTerm :: Term a -> Term a
-- encodedTerm term = inject _Term $ case term of
--   TermRecord (Record tname fields) -> Field _Term_record $ record _Record [
--     Field _Record_typeName $ encodedName tname,
--     Field _Record_fields $ TermList (encodedField <$> fields)]
--   TermUnion (Injection tname field) -> Field _Term_union $
--     record _Injection [
--       Field _Injection_typeName $ encodedName tname,
--       Field _Injection_field $ encodedField field]
--   -- ...
--




-- epsilonEncodeApplicationType :: ApplicationType a -> Term a
-- epsilonEncodeApplicationType (ApplicationType lhs rhs) = record _ApplicationType [
--   Field _ApplicationType_function $ epsilonEncodeType lhs,
--   Field _ApplicationType_argument $ epsilonEncodeType rhs]
--







-- epsilonEncodeFieldType :: FieldType a -> Term a
-- epsilonEncodeFieldType (FieldType (FieldName fname) t) = record _FieldType [
--   Field _FieldType_name $ string fname,
--   Field _FieldType_type $ epsilonEncodeType t]

-- epsilonEncodeFieldTypeDef :: Definition (FieldType a -> Term a)
-- epsilonEncodeFieldTypeDef = typeEncodingDefinition "FieldType" $
--   function (Types.apply (TypeVariable _FieldType) (Types.var "a")) (Types.apply (TypeVariable _Term) (Types.var "a")) $
--   lambda "field" $ encodedTerm $ record _FieldType [
--     Field _FieldType_name $ string fname,
--     Field _FieldType_type $ epsilonEncodeType t]






epsilonEncodeFloatTypeDef :: Definition (FloatType -> Term a)
epsilonEncodeFloatTypeDef = typeEncodingDefinition "FloatType" (TypeVariable _FloatType) $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = Field fname $ constant $ sigmaEncodeTerm $ unitVariant _FloatType fname






-- epsilonEncodeFunctionType :: FunctionType a -> Term a
-- epsilonEncodeFunctionType (FunctionType dom cod) = record _FunctionType [
--   Field _FunctionType_domain $ epsilonEncodeType dom,
--   Field _FunctionType_codomain $ epsilonEncodeType cod]

-- epsilonEncodeFunctionTypeDef :: Definition (FunctionType -> Term a)
-- epsilonEncodeFunctionTypeDef = typeEncodingDefinition

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


-- epsilonEncodeLambdaType :: LambdaType a -> Term a
-- epsilonEncodeLambdaType (LambdaType var body) = record _LambdaType [
--   Field _LambdaType_parameter $ epsilonEncodeName var,
--   Field _LambdaType_body $ epsilonEncodeType body]
--
-- epsilonEncodeLiteralType :: LiteralType -> Term a
-- epsilonEncodeLiteralType at = case at of
--   LiteralTypeBinary -> unitVariant _LiteralType _LiteralType_binary
--   LiteralTypeBoolean -> unitVariant _LiteralType _LiteralType_boolean
--   LiteralTypeFloat ft -> variant _LiteralType _LiteralType_float $ epsilonEncodeFloatType ft
--   LiteralTypeInteger it -> variant _LiteralType _LiteralType_integer $ epsilonEncodeIntegerType it
--   LiteralTypeString -> unitVariant _LiteralType _LiteralType_string

-- epsilonEncodeLiteralTypeDef :: Definition (LiteralType -> Term a)
-- epsilonEncodeLiteralTypeDef = typeEncodingDefinition "LiteralType" $
--   function (TypeVariable _LiteralType) termA $
-- --   match _LiteralType Nothing [
-- --     Case _LiteralType_binary --> Datum $ constant $ sigmaEncodeTerm $ unitVariant _LiteralType _LiteralType_binary,
-- --     Case _LiteralType_boolean --> Datum $ constant $ sigmaEncodeTerm $ unitVariant _LiteralType _LiteralType_boolean,
-- -- --     Case _LiteralType_float --> Datum $
-- --     -- integer
-- --     Case _LiteralType_string --> Datum $ constant $ sigmaEncodeTerm $ unitVariant _LiteralType _LiteralType_string
-- --     ]
--   Datum $ sigmaEncodeTerm $ match _LiteralType Nothing [
-- --     Field _LiteralType_binary $ constant $ unitVariant _LiteralType _LiteralType_binary,
-- --     Field _LiteralType_boolean $ constant $ unitVariant _LiteralType _LiteralType_boolean,
-- -- --     Case _LiteralType_float --> Datum $
-- --     -- integer
--     Field _LiteralType_string $ constant $ unitVariant _LiteralType _LiteralType_string
--     ]


-- epsilonEncodeMapType :: MapType a -> Term a
-- epsilonEncodeMapType (MapType kt vt) = record _MapType [
--   Field _MapType_keys $ epsilonEncodeType kt,
--   Field _MapType_values $ epsilonEncodeType vt]
--
-- epsilonEncodeName :: Name -> Term a
-- epsilonEncodeName name = string $ unName name


epsilonEncodeNameDef :: Definition (Name -> Term a)
epsilonEncodeNameDef = typeEncodingDefinition "Name" (TypeVariable _Name) $
    lambda "name" $ (unwrap _Name @@ var "name")



-- epsilonEncodeNominal :: (x -> Term a) -> Nominal x -> Term a
-- epsilonEncodeNominal mapping (Nominal name obj) = record _Nominal [
--   Field _Nominal_typeName $ epsilonEncodeName name,
--   Field _Nominal_object $ mapping obj]
--
-- epsilonEncodeRowType :: RowType a -> Term a
-- epsilonEncodeRowType (RowType name extends fields) = record _RowType [
--   Field _RowType_typeName $ string (unName name),
--   Field _RowType_extends $ optional (string . unName <$> extends),
--   Field _RowType_fields $ list $ epsilonEncodeFieldType <$> fields]
--
-- epsilonEncodeType :: Type a -> Term a
-- epsilonEncodeType typ = case typ of
--     TypeAnnotated (Annotated t ann) -> TermAnnotated (Annotated (epsilonEncodeType t) ann)
--     TypeApplication a -> tvar _Type_application $ epsilonEncodeApplicationType a
--     TypeFunction ft -> tvar _Type_function $ epsilonEncodeFunctionType ft
--     TypeLambda ut -> tvar _Type_lambda $ epsilonEncodeLambdaType ut
--     TypeList t -> tvar _Type_list $ epsilonEncodeType t
--     TypeLiteral at -> tvar _Type_literal $ epsilonEncodeLiteralType at
--     TypeMap mt -> tvar _Type_map $ epsilonEncodeMapType mt
--     TypeOptional t -> tvar _Type_optional $ epsilonEncodeType t
--     TypeProduct types -> tvar _Type_product $ list (epsilonEncodeType <$> types)
--     TypeRecord rt -> tvar _Type_record $ epsilonEncodeRowType rt
--     TypeSet t -> tvar _Type_set $ epsilonEncodeType t
--     TypeSum types -> tvar _Type_sum $ list (epsilonEncodeType <$> types)
--     TypeUnion rt -> tvar _Type_union $ epsilonEncodeRowType rt
--     TypeVariable name -> tvar _Type_variable $ epsilonEncodeName name
--     TypeWrap n -> tvar _Type_wrap $ epsilonEncodeNominal epsilonEncodeType n
--   where
--     tvar = variant _Type

-- epsilonEncodeTypeDef :: Definition (Type a -> Term a)
-- epsilonEncodeTypeDef = typeEncodingDefinition "Type" $
--   function typeA termA $
--   match _Type Nothing [
--     -- ...
--     Case _Type_literal --> ref epsilonEncodeLiteralTypeDef
--     --- ...
--   ]
