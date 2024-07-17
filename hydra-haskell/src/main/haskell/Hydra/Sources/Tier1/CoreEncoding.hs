{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.CoreEncoding where

-- TODO: use standard Tier-1 imports
import Hydra.Sources.Tier0.All
import Hydra.Dsl.Terms
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Base as Base
import Hydra.Sources.Libraries


coreEncodingModule :: Module Kv
coreEncodingModule = Module (Namespace "hydra/coreEncoding") elements [] tier0Modules $
    Just ("Mapping of hydra/core constructs in a host language like Haskell or Java "
      ++ " to their native Hydra counterparts as terms. "
      ++ " This includes an implementation of LambdaGraph's epsilon encoding (types to terms).")
  where
   elements = [
     Base.el coreEncodeAnnotatedTermDef,
     Base.el coreEncodeAnnotatedTypeDef,
     Base.el coreEncodeApplicationDef,
     Base.el coreEncodeApplicationTypeDef,
     Base.el coreEncodeCaseStatementDef,
     Base.el coreEncodeEliminationDef,
     Base.el coreEncodeFieldDef,
     Base.el coreEncodeFieldNameDef,
     Base.el coreEncodeFieldTypeDef,
     Base.el coreEncodeFloatTypeDef,
     Base.el coreEncodeFloatValueDef,
     Base.el coreEncodeFunctionDef,
     Base.el coreEncodeFunctionTypeDef,
     Base.el coreEncodeInjectionDef,
     Base.el coreEncodeIntegerTypeDef,
     Base.el coreEncodeIntegerValueDef,
     Base.el coreEncodeLambdaDef,
     Base.el coreEncodeLambdaTypeDef,
     --Base.el coreEncodeLetDef,
     Base.el coreEncodeLiteralDef,
     Base.el coreEncodeLiteralTypeDef,
     Base.el coreEncodeMapTypeDef,
     Base.el coreEncodeNameDef,
     Base.el coreEncodeNominalTermDef,
     Base.el coreEncodeNominalTypeDef,
     Base.el coreEncodeOptionalCasesDef,
     Base.el coreEncodeProjectionDef,
     Base.el coreEncodeRecordDef,
     Base.el coreEncodeRowTypeDef,
     Base.el coreEncodeSumDef,
     Base.el coreEncodeTermDef,
     Base.el coreEncodeTupleProjectionDef,
     Base.el coreEncodeTypeDef]

coreEncodingDefinition :: String -> Type Kv -> Term Kv -> Definition x
coreEncodingDefinition label dom term = Base.definitionInModule coreEncodingModule ("coreEncode" ++ label) $
  Base.function dom termA $ Datum term

ref :: Definition a -> Term Kv
ref (Definition name _) = TermVariable name

encodedBinary :: Term Kv -> Term Kv
encodedBinary = encodedLiteral . variant _Literal _Literal_binary

encodedBoolean :: Term Kv -> Term Kv
encodedBoolean = encodedLiteral . variant _Literal _Literal_boolean

encodedCase :: Name -> FieldName -> Term Kv -> Field Kv
encodedCase tname fname fun = Field fname $ lambda "v" $ encodedVariant tname fname (fun @@ var "v")

encodedField :: FieldName -> Term Kv -> Term Kv
encodedField fname = encodedFieldRaw (encodedFieldName fname)

encodedFieldRaw :: Term Kv -> Term Kv -> Term Kv
encodedFieldRaw fname term = record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFieldName :: FieldName -> Term Kv
encodedFieldName = wrap _FieldName . string . unFieldName

encodedFloatValue :: Term Kv -> Term Kv
encodedFloatValue = encodedLiteral . variant _Literal _Literal_float

encodedInjection :: Name -> FieldName -> Term Kv -> Term Kv
encodedInjection tname fname term = record _Injection [
  Field _Injection_typeName $ encodedName tname,
  Field _Injection_field $ encodedField fname term]

encodedInt32 :: Term Kv -> Term Kv
encodedInt32 = encodedIntegerValue . variant _IntegerValue _IntegerValue_int32

encodedIntegerValue :: Term Kv -> Term Kv
encodedIntegerValue = encodedLiteral . variant _Literal _Literal_integer

encodedList :: Term Kv -> Term Kv
encodedList = variant _Term _Term_list

encodedLiteral :: Term Kv -> Term Kv
encodedLiteral = variant _Term _Term_literal

encodedMap :: Term Kv -> Term Kv
encodedMap = variant _Term _Term_map

encodedName :: Name -> Term Kv
encodedName = wrap _Name . string . unName

encodedNominal :: Name -> Term Kv -> Term Kv
encodedNominal name = encodedNominalRaw (encodedName name)

encodedNominalRaw :: Term Kv -> Term Kv -> Term Kv
encodedNominalRaw name term = variant _Term _Term_wrap $ record _Nominal [
  Field _Nominal_typeName name,
  Field _Nominal_object term]

encodedOptional :: Term Kv -> Term Kv
encodedOptional = variant _Term _Term_optional

encodedRecord :: Name -> [(FieldName, Term Kv)] -> Term Kv
encodedRecord tname pairs = variant _Term _Term_record $ record _Record [
    Field _Record_typeName $ encodedName tname,
    Field _Record_fields $ list (encField <$> pairs)]
  where
    encField (fname, term) = encodedField fname term

encodedSet :: Term Kv -> Term Kv
encodedSet = variant _Term _Term_set

encodedString :: Term Kv -> Term Kv
encodedString = encodedLiteral . variant _Literal _Literal_string

encodedUnion :: Term Kv -> Term Kv
encodedUnion = variant _Term _Term_union

encodedVariant :: Name -> FieldName -> Term Kv -> Term Kv
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

coreEncodeAnnotatedTermDef :: Definition (Annotated (Term Kv) a -> Term Kv)
coreEncodeAnnotatedTermDef = coreEncodingDefinition "AnnotatedTerm" annotatedTermA $
  lambda "a" $ variant _Term _Term_annotated $ record _Annotated [
    Field _Annotated_subject $ ref coreEncodeTermDef @@ (project _Annotated _Annotated_subject @@ var "a"),
    Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "a"]

coreEncodeAnnotatedTypeDef :: Definition (Annotated (Type Kv) a -> Term Kv)
coreEncodeAnnotatedTypeDef = coreEncodingDefinition "AnnotatedType" annotatedTypeA $
  lambda "at" $ variant _Term _Term_annotated $ record _Annotated [
    Field _Annotated_subject $ ref coreEncodeTypeDef @@ (project _Annotated _Annotated_subject @@ var "at"),
    Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "at"]

coreEncodeApplicationDef :: Definition (Application Kv -> Term Kv)
coreEncodeApplicationDef = coreEncodingDefinition "Application" applicationA $
  lambda "app" $ encodedRecord _Application [
    (_Application_function, ref coreEncodeTermDef @@ (project _Application _Application_function @@ var "app")),
    (_Application_argument, ref coreEncodeTermDef @@ (project _Application _Application_argument @@ var "app"))]

coreEncodeApplicationTypeDef :: Definition (ApplicationType Kv -> Term Kv)
coreEncodeApplicationTypeDef = coreEncodingDefinition "ApplicationType" applicationTypeA $
  lambda "at" $ encodedRecord _ApplicationType [
    (_ApplicationType_function, ref coreEncodeTypeDef @@ (project _ApplicationType _ApplicationType_function @@ var "at")),
    (_ApplicationType_argument, ref coreEncodeTypeDef @@ (project _ApplicationType _ApplicationType_argument @@ var "at"))]

coreEncodeCaseStatementDef :: Definition (CaseStatement Kv -> Term Kv)
coreEncodeCaseStatementDef = coreEncodingDefinition "CaseStatement" caseStatementA $
  lambda "cs" $ encodedRecord _CaseStatement [
    (_CaseStatement_typeName, ref coreEncodeNameDef @@ (project _CaseStatement _CaseStatement_typeName @@ var "cs")),
    (_CaseStatement_default, encodedOptional
      (primitive _optionals_map @@ ref coreEncodeTermDef @@ (project _CaseStatement _CaseStatement_default @@ var "cs"))),
    (_CaseStatement_cases, encodedList
      (primitive _lists_map @@ ref coreEncodeFieldDef @@ (project _CaseStatement _CaseStatement_cases @@ var "cs")))]

coreEncodeEliminationDef :: Definition (Elimination Kv -> Term Kv)
coreEncodeEliminationDef = coreEncodingDefinition "Elimination" eliminationA $
    match _Elimination Nothing [
      ecase _Elimination_list coreEncodeTermDef,
      ecase _Elimination_optional coreEncodeOptionalCasesDef,
      ecase _Elimination_product coreEncodeTupleProjectionDef,
      ecase _Elimination_record coreEncodeProjectionDef,
      ecase _Elimination_union coreEncodeCaseStatementDef,
      ecase _Elimination_wrap coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Elimination fname (ref funname)

coreEncodeFieldDef :: Definition (Field Kv -> Term Kv)
coreEncodeFieldDef = coreEncodingDefinition "Field" fieldA $
  lambda "f" $ encodedRecord _Field [
    (_Field_name, encodedNominal _FieldName $ encodedString $ (unwrap _FieldName @@ (project _Field _Field_name @@ var "f"))),
    (_Field_term, ref coreEncodeTermDef @@ (project _Field _Field_term @@ var "f"))]

coreEncodeFieldNameDef :: Definition (FieldName -> Term Kv)
coreEncodeFieldNameDef = coreEncodingDefinition "FieldName" (TypeVariable _FieldName) $
  lambda "fn" $ encodedNominal _FieldName $ encodedString (unwrap _FieldName @@ var "fn")

coreEncodeFieldTypeDef :: Definition (FieldType Kv -> Term Kv)
coreEncodeFieldTypeDef = coreEncodingDefinition "FieldType" fieldTypeA $
  lambda "ft" $ encodedRecord _FieldType [
    (_FieldType_name, ref coreEncodeFieldNameDef @@ (project _FieldType _FieldType_name @@ var "ft")),
    (_FieldType_type, ref coreEncodeTypeDef @@ (project _FieldType _FieldType_type @@ var "ft"))]

coreEncodeFloatTypeDef :: Definition (FloatType -> Term Kv)
coreEncodeFloatTypeDef = coreEncodingDefinition "FloatType" (TypeVariable _FloatType) $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = Field fname $ constant $ coreEncodeTerm $ unitVariant _FloatType fname

coreEncodeFloatValueDef :: Definition (FloatValue -> Term Kv)
coreEncodeFloatValueDef = coreEncodingDefinition "FloatValue" (TypeVariable _FloatValue) $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = Field fname $ lambda "v" $ encodedVariant _FloatValue fname $ encodedFloatValue $
      variant _FloatValue fname $ var "v"

coreEncodeFunctionDef :: Definition (Function Kv -> Term Kv)
coreEncodeFunctionDef = coreEncodingDefinition "Function" functionA $
    match _Function Nothing [
      ecase _Function_elimination coreEncodeEliminationDef,
      ecase _Function_lambda coreEncodeLambdaDef,
      ecase _Function_primitive coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Function fname (ref funname)

coreEncodeFunctionTypeDef :: Definition (FunctionType Kv -> Term Kv)
coreEncodeFunctionTypeDef = coreEncodingDefinition "FunctionType" functionTypeA $
  lambda "ft" $ encodedRecord _FunctionType [
    (_FunctionType_domain, ref coreEncodeTypeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft")),
    (_FunctionType_codomain, ref coreEncodeTypeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft"))]

coreEncodeInjectionDef :: Definition (Injection a -> Term Kv)
coreEncodeInjectionDef = coreEncodingDefinition "Injection" injectionA $
  lambda "i" $ encodedRecord _Injection [
    (_Injection_typeName, ref coreEncodeNameDef @@ (project _Injection _Injection_typeName @@ var "i")),
    (_Injection_field, ref coreEncodeFieldDef @@ (project _Injection _Injection_field @@ var "i"))]

coreEncodeIntegerTypeDef :: Definition (IntegerType -> Term Kv)
coreEncodeIntegerTypeDef = coreEncodingDefinition "IntegerType" (TypeVariable _IntegerType) $
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
    cs fname = Field fname $ constant $ coreEncodeTerm $ unitVariant _IntegerType fname

coreEncodeIntegerValueDef :: Definition (IntegerValue -> Term Kv)
coreEncodeIntegerValueDef = coreEncodingDefinition "IntegerValue" (TypeVariable _IntegerValue) $
  match _IntegerValue Nothing (varField <$> [
    _IntegerValue_bigint,
    _IntegerValue_int8,
    _IntegerValue_int16,
    _IntegerValue_int32,
    _IntegerValue_int64,
    _IntegerValue_uint8,
    _IntegerValue_uint16,
    _IntegerValue_uint32,
    _IntegerValue_uint64])
  where
    varField fname = Field fname $ lambda "v" $ encodedVariant _IntegerValue fname $ encodedIntegerValue $
      variant _IntegerValue fname $ var "v"

coreEncodeLambdaDef :: Definition (Lambda Kv -> Term Kv)
coreEncodeLambdaDef = coreEncodingDefinition "Lambda" lambdaA $
  lambda "l" $ encodedRecord _Lambda [
    (_Lambda_parameter, ref coreEncodeNameDef @@ (project _Lambda _Lambda_parameter @@ var "l")),
    (_Lambda_body, ref coreEncodeTermDef @@ (project _Lambda _Lambda_body @@ var "l"))]

coreEncodeLambdaTypeDef :: Definition (LambdaType Kv -> Term Kv)
coreEncodeLambdaTypeDef = coreEncodingDefinition "LambdaType" lambdaTypeA $
  lambda "lt" $ encodedRecord _LambdaType [
    (_LambdaType_parameter, ref coreEncodeNameDef @@ (project _LambdaType _LambdaType_parameter @@ var "lt")),
    (_LambdaType_body, ref coreEncodeTypeDef @@ (project _LambdaType _LambdaType_body @@ var "lt"))]

-- coreEncodeLetDef :: Definition (Let Kv -> Term Kv)
-- coreEncodeLetDef = coreEncodingDefinition "Let" letA $
--   lambda "l" $ encodedRecord _Let [
--     (_Let_bindings, encodedMap
--       (primitive _maps_mapKeys @@ ref coreEncodeNameDef @@
--         (primitive _maps_map @@ ref coreEncodeTermDef @@ (project _Let _Let_bindings @@ var "l")))),
--     (_Let_environment, ref coreEncodeTermDef @@ (project _Let _Let_environment @@ var "l"))]

coreEncodeLiteralDef :: Definition (Literal -> Term Kv)
coreEncodeLiteralDef = coreEncodingDefinition "Literal" (TypeVariable _Literal) $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (ref coreEncodeFloatValueDef @@ var "v"),
    varField _Literal_integer (ref coreEncodeIntegerValueDef @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = Field fname . lambda "v" . encodedVariant _Literal fname

coreEncodeLiteralTypeDef :: Definition (LiteralType -> Term Kv)
coreEncodeLiteralTypeDef = coreEncodingDefinition "LiteralType" (TypeVariable _LiteralType) $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float coreEncodeFloatTypeDef,
    cs _LiteralType_integer coreEncodeIntegerTypeDef,
    csunit _LiteralType_string]
  where
    cs fname fun = Field fname $ lambda "v" $ encodedVariant _LiteralType fname (ref fun @@ var "v")
    csunit fname = Field fname $ constant $ coreEncodeTerm $ variant _LiteralType fname unit

coreEncodeMapTypeDef :: Definition (MapType Kv -> Term Kv)
coreEncodeMapTypeDef = coreEncodingDefinition "MapType" mapTypeA $
    lambda "mt" $ encodedRecord _MapType [
      (_MapType_keys, ref coreEncodeTypeDef @@ (project _MapType _MapType_keys @@ var "mt")),
      (_MapType_values, ref coreEncodeTypeDef @@ (project _MapType _MapType_values @@ var "mt"))]

coreEncodeNameDef :: Definition (Name -> Term Kv)
coreEncodeNameDef = coreEncodingDefinition "Name" (TypeVariable _Name) $
  lambda "fn" $ encodedNominal _Name $ encodedString (unwrap _Name @@ var "fn")

coreEncodeNominalTermDef :: Definition (Nominal (Term Kv) -> Term Kv)
coreEncodeNominalTermDef = coreEncodingDefinition "NominalTerm" nominalTermA $
  lambda "n" $ encodedRecord _Nominal [
    (_Nominal_typeName, ref coreEncodeNameDef @@ (project _Nominal _Nominal_typeName @@ var "n")),
    (_Nominal_object, ref coreEncodeTermDef @@ (project _Nominal _Nominal_object @@ var "n"))]

coreEncodeNominalTypeDef :: Definition (Nominal (Type Kv) -> Term Kv)
coreEncodeNominalTypeDef = coreEncodingDefinition "NominalType" nominalTypeA $
  lambda "nt" $ encodedRecord _Nominal [
    (_Nominal_typeName, ref coreEncodeNameDef @@ (project _Nominal _Nominal_typeName @@ var "nt")),
    (_Nominal_object, ref coreEncodeTypeDef @@ (project _Nominal _Nominal_object @@ var "nt"))]

coreEncodeOptionalCasesDef :: Definition (OptionalCases Kv -> Term Kv)
coreEncodeOptionalCasesDef = coreEncodingDefinition "OptionalCases" optionalCasesA $
  lambda "oc" $ encodedRecord _OptionalCases [
    (_OptionalCases_nothing, ref coreEncodeTermDef @@ (project _OptionalCases _OptionalCases_nothing @@ var "oc")),
    (_OptionalCases_just, ref coreEncodeTermDef @@ (project _OptionalCases _OptionalCases_just @@ var "oc"))]

coreEncodeProjectionDef :: Definition (Projection -> Term Kv)
coreEncodeProjectionDef = coreEncodingDefinition "Projection" (TypeVariable _Projection) $
  lambda "p" $ encodedRecord _Projection [
    (_Projection_typeName, ref coreEncodeNameDef @@ (project _Projection _Projection_typeName @@ var "p")),
    (_Projection_field, ref coreEncodeFieldNameDef @@ (project _Projection _Projection_field @@ var "p"))]

coreEncodeRecordDef :: Definition (Record a -> Term Kv)
coreEncodeRecordDef = coreEncodingDefinition "Record" recordA $
  lambda "r" $ encodedRecord _Record [
    (_Record_typeName, ref coreEncodeNameDef @@ (project _Record _Record_typeName @@ var "r")),
    (_Record_fields, encodedList (primitive _lists_map @@ (ref coreEncodeFieldDef) @@ (project _Record _Record_fields @@ var "r")))]

coreEncodeRowTypeDef :: Definition (RowType Kv -> Term Kv)
coreEncodeRowTypeDef = coreEncodingDefinition "RowType" rowTypeA $
  lambda "rt" $ encodedRecord _RowType [
    (_RowType_typeName, ref coreEncodeNameDef @@ (project _RowType _RowType_typeName @@ var "rt")),
    (_RowType_extends, encodedOptional (primitive _optionals_map @@ ref coreEncodeNameDef @@ (project _RowType _RowType_extends @@ var "rt"))),
    (_RowType_fields, encodedList (primitive _lists_map @@ ref coreEncodeFieldTypeDef @@ (project _RowType _RowType_fields @@ var "rt")))]

coreEncodeSumDef :: Definition (Sum a -> Term Kv)
coreEncodeSumDef = coreEncodingDefinition "Sum" sumA $
  lambda "s" $ encodedRecord _Sum [
    (_Sum_index, encodedInt32 $ project _Sum _Sum_index @@ var "s"),
    (_Sum_size, encodedInt32 $ project _Sum _Sum_size @@ var "s"),
    (_Sum_term, ref coreEncodeTermDef @@ (project _Sum _Sum_term @@ var "s"))]

coreEncodeTermDef :: Definition (Term Kv -> Term Kv)
coreEncodeTermDef = coreEncodingDefinition "Term" termA $
  match _Term (Just $ encodedString $ string "not implemented") [
    ecase _Term_annotated (ref coreEncodeAnnotatedTermDef),
    ecase _Term_application (ref coreEncodeApplicationDef),
    ecase _Term_function (ref coreEncodeFunctionDef),
    -- TODO: restore let constructor after finding a way to infer "Ord a =>" for Haskell
    -- ecase _Term_let (ref coreEncodeLetDef),
    ecase _Term_literal (ref coreEncodeLiteralDef),
    ecase' _Term_list $ encodedList (primitive _lists_map @@ (ref coreEncodeTermDef) @@ var "v"),
    -- TODO: restore map and set constructors after finding a way to infer "Ord a =>" for Haskell
    -- _Term_map,
    ecase' _Term_optional $ encodedOptional (primitive _optionals_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase' _Term_product $ encodedList (primitive _lists_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase _Term_record (ref coreEncodeRecordDef),
    -- TODO: restore map and set constructors after finding a way to infer "Ord a =>" for Haskell
    -- ecase' _Term_set $ encodedSet (primitive _sets_map @@ (ref coreEncodeTermDef) @@ var "v")
    ecase _Term_sum (ref coreEncodeSumDef),
    -- TODO: determine whether streams have a sigma encoding
    -- _ Term_stream
    ecase _Term_union (ref coreEncodeInjectionDef),
    ecase _Term_variable (ref coreEncodeNameDef),
    ecase _Term_wrap (ref coreEncodeNominalTermDef)]
  where
    ecase = encodedCase _Term
    ecase' fname = Field fname . lambda "v" . encodedVariant _Term fname

coreEncodeTupleProjectionDef :: Definition (TupleProjection -> Term Kv)
coreEncodeTupleProjectionDef = coreEncodingDefinition "TupleProjection" (TypeVariable _TupleProjection) $
  lambda "tp" $ encodedRecord _TupleProjection [
    (_TupleProjection_arity, encodedInt32 $ project _TupleProjection _TupleProjection_arity @@ var "tp"),
    (_TupleProjection_index, encodedInt32 $ project _TupleProjection _TupleProjection_index @@ var "tp")]

coreEncodeTypeDef :: Definition (Type Kv -> Term Kv)
coreEncodeTypeDef = coreEncodingDefinition "Type" typeA $
  match _Type Nothing [
    Field _Type_annotated $ lambda "v" $ variant _Term _Term_annotated $ record _Annotated [
      Field _Annotated_subject $ ref coreEncodeTypeDef @@ (project _Annotated _Annotated_subject @@ var "v"),
      Field _Annotated_annotation $ project _Annotated _Annotated_annotation @@ var "v"],
    csref _Type_application coreEncodeApplicationTypeDef,
    csref _Type_function coreEncodeFunctionTypeDef,
    csref _Type_lambda coreEncodeLambdaTypeDef,
    csref _Type_list coreEncodeTypeDef,
    csref _Type_literal coreEncodeLiteralTypeDef,
    csref _Type_map coreEncodeMapTypeDef,
    csref _Type_optional coreEncodeTypeDef,
    cs _Type_product $ encodedList $ primitive _lists_map @@ ref coreEncodeTypeDef @@ var "v",
    csref _Type_record coreEncodeRowTypeDef,
    csref _Type_set coreEncodeTypeDef,
    csref _Type_stream coreEncodeTypeDef,
    cs _Type_sum $ encodedList $ primitive _lists_map @@ ref coreEncodeTypeDef @@ var "v",
    csref _Type_union coreEncodeRowTypeDef,
    csref _Type_variable coreEncodeNameDef,
    csref _Type_wrap coreEncodeNominalTypeDef]
  where
    cs fname term = Field fname $ lambda "v" $ encodedVariant _Type fname term
    csref fname fun = cs fname (ref fun @@ var "v")
