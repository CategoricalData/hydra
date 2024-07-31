{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.CoreEncoding where

-- TODO: use standard Tier-1 imports
import Hydra.Sources.Tier0.All
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.ShorthandTypes
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Set as S


coreEncodingModule :: Module
coreEncodingModule = Module (Namespace "hydra/coreEncoding") elements [] tier0Modules $
    Just ("Mapping of hydra/core constructs in a host language like Haskell or Java "
      ++ " to their native Hydra counterparts as terms. "
      ++ " This includes an implementation of LambdaGraph's epsilon encoding (types to terms).")
  where
   elements = [
     el coreEncodeAnnotatedTermDef,
     el coreEncodeAnnotatedTypeDef,
     el coreEncodeApplicationDef,
     el coreEncodeApplicationTypeDef,
     el coreEncodeCaseStatementDef,
     el coreEncodeEliminationDef,
     el coreEncodeFieldDef,
     el coreEncodeFieldTypeDef,
     el coreEncodeFloatTypeDef,
     el coreEncodeFloatValueDef,
     el coreEncodeFunctionDef,
     el coreEncodeFunctionTypeDef,
     el coreEncodeInjectionDef,
     el coreEncodeIntegerTypeDef,
     el coreEncodeIntegerValueDef,
     el coreEncodeLambdaDef,
     el coreEncodeLambdaTypeDef,
     el coreEncodeLetDef,
     el coreEncodeLetBindingDef,
     el coreEncodeLiteralDef,
     el coreEncodeLiteralTypeDef,
     el coreEncodeMapTypeDef,
     el coreEncodeNameDef,
     el coreEncodeOptionalCasesDef,
     el coreEncodeProjectionDef,
     el coreEncodeRecordDef,
     el coreEncodeRowTypeDef,
     el coreEncodeSumDef,
     el coreEncodeTermDef,
     el coreEncodeTupleProjectionDef,
     el coreEncodeTypeDef,
     el coreEncodeTypeSchemeDef,
     el coreEncodeWrappedTermDef,
     el coreEncodeWrappedTypeDef]

coreEncodingDefinition :: String -> Type -> Datum x -> Definition x
coreEncodingDefinition label dom datum = definitionInModule coreEncodingModule ("coreEncode" ++ label) $
  function dom termT datum

encodedBinary :: Datum String -> Datum Term
encodedBinary = encodedLiteral . Core.literalBinary

encodedBoolean :: Datum Bool -> Datum Term
encodedBoolean = encodedLiteral . Core.literalBoolean

encodedCase :: Name -> Name -> Datum (a -> Term) -> Field
encodedCase tname fname enc = field fname $ lambda "v" $ encodedVariant tname fname (enc @@ var "v")

encodedField :: Name -> Datum Term -> Datum Term
encodedField fname term = encodedFieldRaw (encodedName fname) term

encodedFieldRaw :: Datum Name -> Datum Term -> Datum Term
encodedFieldRaw (Datum fname) (Datum term) = Datum $ Terms.record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFloatValue :: Datum FloatValue -> Datum Term
encodedFloatValue = encodedLiteral . Core.literalFloat

encodedInjection :: Name -> Name -> Datum Term -> Datum Term
encodedInjection tname fname term = Datum $ Terms.record _Injection [
  field _Injection_typeName $ encodedName tname,
  field _Injection_field $ encodedField fname term]

encodedInt32 :: Datum Int -> Datum Term
encodedInt32 v = encodedIntegerValue $ variant _IntegerValue _IntegerValue_int32 v

encodedIntegerValue :: Datum IntegerValue -> Datum Term
encodedIntegerValue = encodedLiteral . Core.literalInteger

encodedList :: Datum [a] -> Datum Term
encodedList = variant _Term _Term_list

encodedLiteral :: Datum Literal -> Datum Term
encodedLiteral = variant _Term _Term_literal

encodedMap :: Datum (M.Map k v) -> Datum Term
encodedMap = variant _Term _Term_map

encodedName :: Name -> Datum Name
encodedName = wrap _Name . string . unName

encodedWrappedTerm :: Name -> Datum Term -> Datum Term
encodedWrappedTerm name = encodedWrappedTermRaw (encodedName name)

encodedWrappedTermRaw :: Datum Name -> Datum Term -> Datum Term
encodedWrappedTermRaw (Datum name) (Datum term) = Datum $ Terms.variant _Term _Term_wrap $ Terms.record _WrappedTerm [
  Field _WrappedTerm_typeName name,
  Field _WrappedTerm_object term]

encodedOptional :: Datum (Maybe a) -> Datum Term
encodedOptional = variant _Term _Term_optional

encodedRecord :: Name -> [Field] -> Datum Term
encodedRecord tname fields = Datum $ Terms.variant _Term _Term_record $ Terms.record _Record [
    field _Record_typeName $ encodedName tname,
    field _Record_fields $ list (encField <$> fields)]
  where
    encField (Field fname term) = encodedField fname $ Datum term

encodedSet :: Datum (S.Set a) -> Datum Term
encodedSet = variant _Term _Term_set

encodedString :: Datum String -> Datum Term
encodedString = encodedLiteral . variant _Literal _Literal_string

encodedUnion :: Datum Term -> Datum Term
encodedUnion = variant _Term _Term_union

encodedVariant :: Name -> Name -> Datum Term -> Datum Term
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

coreEncodeAnnotatedTermDef :: Definition (AnnotatedTerm -> Term)
coreEncodeAnnotatedTermDef = coreEncodingDefinition "AnnotatedTerm" annotatedTermT $
  lambda "a" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref coreEncodeTermDef @@ (Core.annotatedTermSubject @@ var "a"),
    field _AnnotatedTerm_annotation $ Core.annotatedTermAnnotation @@ var "a"]

coreEncodeAnnotatedTypeDef :: Definition (AnnotatedType -> Term)
coreEncodeAnnotatedTypeDef = coreEncodingDefinition "AnnotatedType" annotatedTypeT $
  lambda "at" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref coreEncodeTypeDef @@ (Core.annotatedTypeSubject @@ var "at"),
    field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation @@ var "at"]

coreEncodeApplicationDef :: Definition (Application -> Term)
coreEncodeApplicationDef = coreEncodingDefinition "Application" applicationT $
  lambda "app" $ encodedRecord _Application [
    field _Application_function $ ref coreEncodeTermDef @@ (Core.applicationFunction @@ var "app"),
    field _Application_argument $ ref coreEncodeTermDef @@ (Core.applicationArgument @@ var "app")]

coreEncodeApplicationTypeDef :: Definition (ApplicationType -> Term)
coreEncodeApplicationTypeDef = coreEncodingDefinition "ApplicationType" applicationTypeT $
  lambda "at" $ encodedRecord _ApplicationType [
    field _ApplicationType_function $ ref coreEncodeTypeDef @@ (Core.applicationTypeFunction @@ var "at"),
    field _ApplicationType_argument $ ref coreEncodeTypeDef @@ (Core.applicationTypeArgument @@ var "at")]

coreEncodeCaseStatementDef :: Definition (CaseStatement -> Term)
coreEncodeCaseStatementDef = coreEncodingDefinition "CaseStatement" caseStatementT $
  lambda "cs" $ encodedRecord _CaseStatement [
    field _CaseStatement_typeName $ ref coreEncodeNameDef @@ (Core.caseStatementTypeName @@ var "cs"),
    field _CaseStatement_default $ encodedOptional
      (primitive _optionals_map @@ ref coreEncodeTermDef @@ (Core.caseStatementDefault @@ var "cs")),
    field _CaseStatement_cases $ encodedList
      (primitive _lists_map @@ ref coreEncodeFieldDef @@ (Core.caseStatementCases @@ var "cs"))]

coreEncodeEliminationDef :: Definition (Elimination -> Term)
coreEncodeEliminationDef = coreEncodingDefinition "Elimination" eliminationT $
    match _Elimination Nothing [
      ecase _Elimination_list coreEncodeTermDef,
      ecase _Elimination_optional coreEncodeOptionalCasesDef,
      ecase _Elimination_product coreEncodeTupleProjectionDef,
      ecase _Elimination_record coreEncodeProjectionDef,
      ecase _Elimination_union coreEncodeCaseStatementDef,
      ecase _Elimination_wrap coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Elimination fname (ref funname)

coreEncodeFieldDef :: Definition (Field -> Term)
coreEncodeFieldDef = coreEncodingDefinition "Field" fieldT $
  lambda "f" $ encodedRecord _Field [
    field _Field_name $ encodedWrappedTerm _Name $ encodedString $ (unwrap _Name @@ (Core.fieldName @@ var "f")),
    field _Field_term $ ref coreEncodeTermDef @@ (Core.fieldTerm @@ var "f")]

coreEncodeFieldTypeDef :: Definition (FieldType -> Term)
coreEncodeFieldTypeDef = coreEncodingDefinition "FieldType" fieldTypeT $
  lambda "ft" $ encodedRecord _FieldType [
    field _FieldType_name $ ref coreEncodeNameDef @@ (Core.fieldTypeName @@ var "ft"),
    field _FieldType_type $ ref coreEncodeTypeDef @@ (Core.fieldTypeType @@ var "ft")]

coreEncodeFloatTypeDef :: Definition (FloatType -> Term)
coreEncodeFloatTypeDef = coreEncodingDefinition "FloatType" floatTypeT $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = field fname $ constant $ Datum $ coreEncodeTerm $ unDatum $ unitVariant _FloatType fname

coreEncodeFloatValueDef :: Definition (FloatValue -> Term)
coreEncodeFloatValueDef = coreEncodingDefinition "FloatValue" floatValueT $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = field fname $ lambda "v" $ encodedVariant _FloatValue fname $ encodedFloatValue $
      variant _FloatValue fname $ var "v"

coreEncodeFunctionDef :: Definition (Function -> Term)
coreEncodeFunctionDef = coreEncodingDefinition "Function" functionT $
    match _Function Nothing [
      ecase _Function_elimination coreEncodeEliminationDef,
      ecase _Function_lambda coreEncodeLambdaDef,
      ecase _Function_primitive coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Function fname (ref funname)

coreEncodeFunctionTypeDef :: Definition (FunctionType -> Term)
coreEncodeFunctionTypeDef = coreEncodingDefinition "FunctionType" functionTypeT $
  lambda "ft" $ encodedRecord _FunctionType [
    field _FunctionType_domain $ ref coreEncodeTypeDef @@ (Core.functionTypeDomain @@ var "ft"),
    field _FunctionType_codomain $ ref coreEncodeTypeDef @@ (Core.functionTypeCodomain @@ var "ft")]

coreEncodeInjectionDef :: Definition (Injection -> Term)
coreEncodeInjectionDef = coreEncodingDefinition "Injection" injectionT $
  lambda "i" $ encodedRecord _Injection [
    field _Injection_typeName $ ref coreEncodeNameDef @@ (Core.injectionTypeName @@ var "i"),
    field _Injection_field $ ref coreEncodeFieldDef @@ (Core.injectionField @@ var "i")]

coreEncodeIntegerTypeDef :: Definition (IntegerType -> Term)
coreEncodeIntegerTypeDef = coreEncodingDefinition "IntegerType" integerTypeT $
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
    cs fname = field fname $ constant $ Datum $ coreEncodeTerm $ unDatum $ unitVariant _IntegerType fname

coreEncodeIntegerValueDef :: Definition (IntegerValue -> Term)
coreEncodeIntegerValueDef = coreEncodingDefinition "IntegerValue" integerValueT $
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
    varField fname = field fname $ lambda "v" $ encodedVariant _IntegerValue fname $ encodedIntegerValue $
      variant _IntegerValue fname $ var "v"

coreEncodeLambdaDef :: Definition (Lambda -> Term)
coreEncodeLambdaDef = coreEncodingDefinition "Lambda" lambdaT $
  lambda "l" $ encodedRecord _Lambda [
    field _Lambda_parameter $ ref coreEncodeNameDef @@ (Core.lambdaParameter @@ var "l"),
    field _Lambda_body $ ref coreEncodeTermDef @@ (Core.lambdaBody @@ var "l")]

coreEncodeLambdaTypeDef :: Definition (LambdaType -> Term)
coreEncodeLambdaTypeDef = coreEncodingDefinition "LambdaType" lambdaTypeT $
  lambda "lt" $ encodedRecord _LambdaType [
    field _LambdaType_parameter $ ref coreEncodeNameDef @@ (Core.lambdaTypeParameter @@ var "lt"),
    field _LambdaType_body $ ref coreEncodeTypeDef @@ (Core.lambdaTypeBody @@ var "lt")]

coreEncodeLetDef :: Definition (Let -> Term)
coreEncodeLetDef = coreEncodingDefinition "Let" letT $
  lambda "l" $ encodedRecord _Let [
    field _Let_bindings $ encodedList (primitive _lists_map @@ ref coreEncodeLetBindingDef @@ (Core.letBindings @@ var "l")),
    field _Let_environment $ ref coreEncodeTermDef @@ (Core.letEnvironment @@ var "l")]

coreEncodeLetBindingDef :: Definition (LetBinding -> Term)
coreEncodeLetBindingDef = coreEncodingDefinition "LetBinding" letBindingT $
  lambda "b" $ encodedRecord _LetBinding [
    field _LetBinding_name $ ref coreEncodeNameDef @@ (Core.letBindingName @@ var "b"),
    field _LetBinding_term $ ref coreEncodeTermDef @@ (Core.letBindingTerm @@ var "b"),
    field _LetBinding_type $ encodedOptional $ primitive _optionals_map @@ ref coreEncodeTypeSchemeDef @@ (Core.letBindingType @@ var "b")]

coreEncodeLiteralDef :: Definition (Literal -> Term)
coreEncodeLiteralDef = coreEncodingDefinition "Literal" literalT $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (ref coreEncodeFloatValueDef @@ var "v"),
    varField _Literal_integer (ref coreEncodeIntegerValueDef @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = field fname . lambda "v" . encodedVariant _Literal fname

coreEncodeLiteralTypeDef :: Definition (LiteralType -> Term)
coreEncodeLiteralTypeDef = coreEncodingDefinition "LiteralType" literalTypeT $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float coreEncodeFloatTypeDef,
    cs _LiteralType_integer coreEncodeIntegerTypeDef,
    csunit _LiteralType_string]
  where
    cs fname fun = field fname $ lambda "v" $ encodedVariant _LiteralType fname (ref fun @@ var "v")
    csunit fname = field fname $ constant $ Datum $ coreEncodeTerm $ unDatum $ variant _LiteralType fname unit

coreEncodeMapTypeDef :: Definition (MapType -> Term)
coreEncodeMapTypeDef = coreEncodingDefinition "MapType" mapTypeT $
    lambda "mt" $ encodedRecord _MapType [
      field _MapType_keys $ ref coreEncodeTypeDef @@ (Core.mapTypeKeys @@ var "mt"),
      field _MapType_values $ ref coreEncodeTypeDef @@ (Core.mapTypeValues @@ var "mt")]

coreEncodeNameDef :: Definition (Name -> Term)
coreEncodeNameDef = coreEncodingDefinition "Name" nameT $
  lambda "fn" $ encodedWrappedTerm _Name $ encodedString $ unwrap _Name @@ var "fn"

coreEncodeOptionalCasesDef :: Definition (OptionalCases -> Term)
coreEncodeOptionalCasesDef = coreEncodingDefinition "OptionalCases" optionalCasesT $
  lambda "oc" $ encodedRecord _OptionalCases [
    field _OptionalCases_nothing $ ref coreEncodeTermDef @@ (Core.optionalCasesNothing @@ var "oc"),
    field _OptionalCases_just $ ref coreEncodeTermDef @@ (Core.optionalCasesJust @@ var "oc")]

coreEncodeProjectionDef :: Definition (Projection -> Term)
coreEncodeProjectionDef = coreEncodingDefinition "Projection" projectionT $
  lambda "p" $ encodedRecord _Projection [
    field _Projection_typeName $ ref coreEncodeNameDef @@ (Core.projectionTypeName @@ var "p"),
    field _Projection_field $ ref coreEncodeNameDef @@ (Core.projectionField @@ var "p")]

coreEncodeRecordDef :: Definition (Record -> Term)
coreEncodeRecordDef = coreEncodingDefinition "Record" recordT $
  lambda "r" $ encodedRecord _Record [
    field _Record_typeName $ ref coreEncodeNameDef @@ (Core.recordTypeName @@ var "r"),
    field _Record_fields $ encodedList (primitive _lists_map @@ (ref coreEncodeFieldDef) @@ (Core.recordFields @@ var "r"))]

coreEncodeRowTypeDef :: Definition (RowType -> Term)
coreEncodeRowTypeDef = coreEncodingDefinition "RowType" rowTypeT $
  lambda "rt" $ encodedRecord _RowType [
    field _RowType_typeName $ ref coreEncodeNameDef @@ (Core.rowTypeTypeName @@ var "rt"),
    field _RowType_extends $ encodedOptional (primitive _optionals_map @@ ref coreEncodeNameDef @@ (Core.rowTypeExtends @@ var "rt")),
    field _RowType_fields $ encodedList (primitive _lists_map @@ ref coreEncodeFieldTypeDef @@ (Core.rowTypeFields @@ var "rt"))]

coreEncodeSumDef :: Definition (Sum -> Term)
coreEncodeSumDef = coreEncodingDefinition "Sum" sumT $
  lambda "s" $ encodedRecord _Sum [
    field _Sum_index $ encodedInt32 $ Core.sumIndex @@ var "s",
    field _Sum_size $ encodedInt32 $ Core.sumSize @@ var "s",
    field _Sum_term $ ref coreEncodeTermDef @@ (Core.sumTerm @@ var "s")]

coreEncodeTermDef :: Definition (Term -> Term)
coreEncodeTermDef = coreEncodingDefinition "Term" termT $
  match _Term (Just $ encodedString $ string "not implemented") [
    ecase _Term_annotated (ref coreEncodeAnnotatedTermDef),
    ecase _Term_application (ref coreEncodeApplicationDef),
    ecase _Term_function (ref coreEncodeFunctionDef),
    ecase _Term_let (ref coreEncodeLetDef),
    ecase _Term_literal (ref coreEncodeLiteralDef),
    ecase' _Term_list $ encodedList $ primitive _lists_map @@ (ref coreEncodeTermDef) @@ var "v",
--     -- TODO: map encoding
--     ecase' _Term_map $ encodedMap
    ecase' _Term_optional $ encodedOptional (primitive _optionals_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase' _Term_product $ encodedList (primitive _lists_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase _Term_record (ref coreEncodeRecordDef),
    ecase' _Term_set $ encodedSet $ primitive _sets_map @@ (ref coreEncodeTermDef) @@ var "v",
    ecase _Term_sum (ref coreEncodeSumDef),
    ecase _Term_union (ref coreEncodeInjectionDef),
    ecase _Term_variable $ ref coreEncodeNameDef,
    ecase _Term_wrap $ ref coreEncodeWrappedTermDef]
  where
    ecase = encodedCase _Term
    ecase' fname = field fname . lambda "v" . encodedVariant _Term fname

coreEncodeTupleProjectionDef :: Definition (TupleProjection -> Term)
coreEncodeTupleProjectionDef = coreEncodingDefinition "TupleProjection" tupleProjectionT $
  lambda "tp" $ encodedRecord _TupleProjection [
    field _TupleProjection_arity $ encodedInt32 $ Core.tupleProjectionArity @@ var "tp",
    field _TupleProjection_index $ encodedInt32 $ Core.tupleProjectionIndex @@ var "tp"]

coreEncodeTypeDef :: Definition (Type -> Term)
coreEncodeTypeDef = coreEncodingDefinition "Type" typeT $
  match _Type Nothing [
    field _Type_annotated $ lambda "v" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
      field _AnnotatedTerm_subject $ ref coreEncodeTypeDef @@ (Core.annotatedTypeSubject @@ var "v"),
      field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation @@ var "v"],
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
    cs _Type_sum $ encodedList $ primitive _lists_map @@ ref coreEncodeTypeDef @@ var "v",
    csref _Type_union coreEncodeRowTypeDef,
    csref _Type_variable coreEncodeNameDef,
    csref _Type_wrap coreEncodeWrappedTypeDef]
  where
    cs fname term = field fname $ lambda "v" $ encodedVariant _Type fname term
    csref fname fun = cs fname (ref fun @@ var "v")

coreEncodeTypeSchemeDef :: Definition (TypeScheme -> Term)
coreEncodeTypeSchemeDef = coreEncodingDefinition "TypeScheme" typeSchemeT $
  lambda "ts" $ encodedRecord _TypeScheme [
    field _TypeScheme_variables $ encodedList (primitive _lists_map @@ ref coreEncodeNameDef @@ (Core.typeSchemeVariables @@ var "ts")),
    field _TypeScheme_type $ ref coreEncodeTypeDef @@ (Core.typeSchemeType @@ var "ts")]

coreEncodeWrappedTermDef :: Definition (WrappedTerm -> Term)
coreEncodeWrappedTermDef = coreEncodingDefinition "WrappedTerm" wrappedTermT $
  lambda "n" $ encodedRecord _WrappedTerm [
    field _WrappedTerm_typeName $ ref coreEncodeNameDef @@ (Core.wrappedTermTypeName @@ var "n"),
    field _WrappedTerm_object $ ref coreEncodeTermDef @@ (Core.wrappedTermObject @@ var "n")]

coreEncodeWrappedTypeDef :: Definition (WrappedType -> Term)
coreEncodeWrappedTypeDef = coreEncodingDefinition "WrappedType" wrappedTypeT $
  lambda "nt" $ encodedRecord _WrappedType [
    field _WrappedType_typeName $ ref coreEncodeNameDef @@ (Core.wrappedTypeTypeName @@ var "nt"),
    field _WrappedType_object $ ref coreEncodeTypeDef @@ (Core.wrappedTypeObject @@ var "nt")]
