{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.CoreEncoding where

-- Standard term-level Tier-1 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Tier1.Strip as Strip


coreEncodingModule :: Module
coreEncodingModule = Module (Namespace "hydra.coreEncoding") elements
    [Strip.hydraStripModule]
    [hydraCoreModule] $
    Just ("Mapping of hydra.core constructs in a host language like Haskell or Java "
      <> " to their native Hydra counterparts as terms. "
      <> " This includes an implementation of LambdaGraph's epsilon encoding (types to terms).")
  where
    elements = encodingElements <> extraElements
    encodingElements = [
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
      el coreEncodeForallTypeDef,
      el coreEncodeLetDef,
      el coreEncodeLetBindingDef,
      el coreEncodeLiteralDef,
      el coreEncodeLiteralTypeDef,
      el coreEncodeMapTypeDef,
      el coreEncodeNameDef,
      el coreEncodeProjectionDef,
      el coreEncodeRecordDef,
      el coreEncodeRowTypeDef,
      el coreEncodeSumDef,
      el coreEncodeTermDef,
      el coreEncodeTupleProjectionDef,
      el coreEncodeTypeDef,
      el coreEncodeTypeAbstractionDef,
      el coreEncodeTypeSchemeDef,
      el coreEncodeTypedTermDef,
      el coreEncodeWrappedTermDef,
      el coreEncodeWrappedTypeDef]
    extraElements = [
      el isEncodedTypeDef,
      el isTypeDef,
      el isUnitTermDef,
      el isUnitTypeDef]

coreEncodingDefinition :: String -> TTerm x -> TElement x
coreEncodingDefinition label = definitionInModule coreEncodingModule ("coreEncode" <> label)

coreEncodingExtrasDefinition :: String -> TTerm a -> TElement a
coreEncodingExtrasDefinition = definitionInModule coreEncodingModule

encodedBinary :: TTerm String -> TTerm Term
encodedBinary = encodedLiteral . Core.literalBinary

encodedBoolean :: TTerm Bool -> TTerm Term
encodedBoolean = encodedLiteral . Core.literalBoolean

encodedCase :: Name -> Name -> TTerm (a -> Term) -> Field
encodedCase tname fname enc = field fname $ lambda "v" $ encodedVariant tname fname (enc @@ var "v")

encodedField :: Name -> TTerm Term -> TTerm Term
encodedField fname term = encodedFieldRaw (encodedName fname) term

encodedFieldRaw :: TTerm Name -> TTerm Term -> TTerm Term
encodedFieldRaw (TTerm fname) (TTerm term) = TTerm $ Terms.record _Field [
  Field _Field_name fname,
  Field _Field_term term]

encodedFloatValue :: TTerm FloatValue -> TTerm Term
encodedFloatValue = encodedLiteral . Core.literalFloat

encodedInjection :: Name -> Name -> TTerm Term -> TTerm Term
encodedInjection tname fname term = TTerm $ Terms.record _Injection [
  field _Injection_typeName $ encodedName tname,
  field _Injection_field $ encodedField fname term]

encodedInt32 :: TTerm Int -> TTerm Term
encodedInt32 v = encodedIntegerValue $ variant _IntegerValue _IntegerValue_int32 v

encodedIntegerValue :: TTerm IntegerValue -> TTerm Term
encodedIntegerValue = encodedLiteral . Core.literalInteger

encodedList :: TTerm [a] -> TTerm Term
encodedList = variant _Term _Term_list

encodedLiteral :: TTerm Literal -> TTerm Term
encodedLiteral = variant _Term _Term_literal

encodedMap :: TTerm (M.Map k v) -> TTerm Term
encodedMap = variant _Term _Term_map

encodedName :: Name -> TTerm Name
encodedName = wrap _Name . string . unName

encodedWrappedTerm :: Name -> TTerm Term -> TTerm Term
encodedWrappedTerm name = encodedWrappedTermRaw (encodedName name)

encodedWrappedTermRaw :: TTerm Name -> TTerm Term -> TTerm Term
encodedWrappedTermRaw (TTerm name) (TTerm term) = TTerm $ Terms.variant _Term _Term_wrap $ Terms.record _WrappedTerm [
  Field _WrappedTerm_typeName name,
  Field _WrappedTerm_object term]

encodedOptional :: TTerm (Maybe a) -> TTerm Term
encodedOptional = variant _Term _Term_optional

encodedRecord :: Name -> [Field] -> TTerm Term
encodedRecord tname fields = TTerm $ Terms.variant _Term _Term_record $ Terms.record _Record [
    field _Record_typeName $ encodedName tname,
    field _Record_fields $ list (encField <$> fields)]
  where
    encField (Field fname term) = encodedField fname $ TTerm term

encodedSet :: TTerm (S.Set a) -> TTerm Term
encodedSet = variant _Term _Term_set

encodedString :: TTerm String -> TTerm Term
encodedString = encodedLiteral . variant _Literal _Literal_string

encodedUnion :: TTerm Term -> TTerm Term
encodedUnion = variant _Term _Term_union

encodedVariant :: Name -> Name -> TTerm Term -> TTerm Term
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

coreEncodeAnnotatedTermDef :: TElement (AnnotatedTerm -> Term)
coreEncodeAnnotatedTermDef = coreEncodingDefinition "AnnotatedTerm" $
  lambda "a" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref coreEncodeTermDef @@ (Core.annotatedTermSubject $ var "a"),
    field _AnnotatedTerm_annotation $ Core.annotatedTermAnnotation $ var "a"]

coreEncodeAnnotatedTypeDef :: TElement (AnnotatedType -> Term)
coreEncodeAnnotatedTypeDef = coreEncodingDefinition "AnnotatedType" $
  lambda "at" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref coreEncodeTypeDef @@ (Core.annotatedTypeSubject $ var "at"),
    field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation $ var "at"]

coreEncodeApplicationDef :: TElement (Application -> Term)
coreEncodeApplicationDef = coreEncodingDefinition "Application" $
  lambda "app" $ encodedRecord _Application [
    field _Application_function $ ref coreEncodeTermDef @@ (Core.applicationFunction $ var "app"),
    field _Application_argument $ ref coreEncodeTermDef @@ (Core.applicationArgument $ var "app")]

coreEncodeApplicationTypeDef :: TElement (ApplicationType -> Term)
coreEncodeApplicationTypeDef = coreEncodingDefinition "ApplicationType" $
  lambda "at" $ encodedRecord _ApplicationType [
    field _ApplicationType_function $ ref coreEncodeTypeDef @@ (Core.applicationTypeFunction $ var "at"),
    field _ApplicationType_argument $ ref coreEncodeTypeDef @@ (Core.applicationTypeArgument $ var "at")]

coreEncodeCaseStatementDef :: TElement (CaseStatement -> Term)
coreEncodeCaseStatementDef = coreEncodingDefinition "CaseStatement" $
  lambda "cs" $ encodedRecord _CaseStatement [
    field _CaseStatement_typeName $ ref coreEncodeNameDef @@ (Core.caseStatementTypeName $ var "cs"),
    field _CaseStatement_default $ encodedOptional
      (primitive _optionals_map @@ ref coreEncodeTermDef @@ (Core.caseStatementDefault $ var "cs")),
    field _CaseStatement_cases $ encodedList
      (primitive _lists_map @@ ref coreEncodeFieldDef @@ (Core.caseStatementCases $ var "cs"))]

coreEncodeEliminationDef :: TElement (Elimination -> Term)
coreEncodeEliminationDef = coreEncodingDefinition "Elimination" $
    match _Elimination Nothing [
      ecase _Elimination_product coreEncodeTupleProjectionDef,
      ecase _Elimination_record coreEncodeProjectionDef,
      ecase _Elimination_union coreEncodeCaseStatementDef,
      ecase _Elimination_wrap coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Elimination fname (ref funname)

coreEncodeFieldDef :: TElement (Field -> Term)
coreEncodeFieldDef = coreEncodingDefinition "Field" $
  lambda "f" $ encodedRecord _Field [
    field _Field_name $ encodedWrappedTerm _Name $ encodedString $ (unwrap _Name @@ (Core.fieldName $ var "f")),
    field _Field_term $ ref coreEncodeTermDef @@ (Core.fieldTerm $ var "f")]

coreEncodeFieldTypeDef :: TElement (FieldType -> Term)
coreEncodeFieldTypeDef = coreEncodingDefinition "FieldType" $
  lambda "ft" $ encodedRecord _FieldType [
    field _FieldType_name $ ref coreEncodeNameDef @@ (Core.fieldTypeName $ var "ft"),
    field _FieldType_type $ ref coreEncodeTypeDef @@ (Core.fieldTypeType $ var "ft")]

coreEncodeFloatTypeDef :: TElement (FloatType -> Term)
coreEncodeFloatTypeDef = coreEncodingDefinition "FloatType" $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = field fname $ constant $ TTerm $ coreEncodeTerm $ unTTerm $ unitVariant _FloatType fname

coreEncodeFloatValueDef :: TElement (FloatValue -> Term)
coreEncodeFloatValueDef = coreEncodingDefinition "FloatValue" $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = field fname $ lambda "v" $ encodedVariant _FloatValue fname $ encodedFloatValue $
      variant _FloatValue fname $ var "v"

coreEncodeFunctionDef :: TElement (Function -> Term)
coreEncodeFunctionDef = coreEncodingDefinition "Function" $
    match _Function Nothing [
      ecase _Function_elimination coreEncodeEliminationDef,
      ecase _Function_lambda coreEncodeLambdaDef,
      ecase _Function_primitive coreEncodeNameDef]
  where
    ecase fname funname = encodedCase _Function fname (ref funname)

coreEncodeFunctionTypeDef :: TElement (FunctionType -> Term)
coreEncodeFunctionTypeDef = coreEncodingDefinition "FunctionType" $
  lambda "ft" $ encodedRecord _FunctionType [
    field _FunctionType_domain $ ref coreEncodeTypeDef @@ (Core.functionTypeDomain $ var "ft"),
    field _FunctionType_codomain $ ref coreEncodeTypeDef @@ (Core.functionTypeCodomain $ var "ft")]

coreEncodeInjectionDef :: TElement (Injection -> Term)
coreEncodeInjectionDef = coreEncodingDefinition "Injection" $
  lambda "i" $ encodedRecord _Injection [
    field _Injection_typeName $ ref coreEncodeNameDef @@ (Core.injectionTypeName $ var "i"),
    field _Injection_field $ ref coreEncodeFieldDef @@ (Core.injectionField $ var "i")]

coreEncodeIntegerTypeDef :: TElement (IntegerType -> Term)
coreEncodeIntegerTypeDef = coreEncodingDefinition "IntegerType" $
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
    cs fname = field fname $ constant $ TTerm $ coreEncodeTerm $ unTTerm $ unitVariant _IntegerType fname

coreEncodeIntegerValueDef :: TElement (IntegerValue -> Term)
coreEncodeIntegerValueDef = coreEncodingDefinition "IntegerValue" $
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

coreEncodeLambdaDef :: TElement (Lambda -> Term)
coreEncodeLambdaDef = coreEncodingDefinition "Lambda" $
  lambda "l" $ encodedRecord _Lambda [
    field _Lambda_parameter $ ref coreEncodeNameDef @@ (Core.lambdaParameter $ var "l"),
    field _Lambda_domain $ encodedOptional $ primitive _optionals_map @@ ref coreEncodeTypeDef @@ (Core.lambdaDomain $ var "l"),
    field _Lambda_body $ ref coreEncodeTermDef @@ (Core.lambdaBody $ var "l")]

coreEncodeForallTypeDef :: TElement (ForallType -> Term)
coreEncodeForallTypeDef = coreEncodingDefinition "ForallType" $
  lambda "lt" $ encodedRecord _ForallType [
    field _ForallType_parameter $ ref coreEncodeNameDef @@ (Core.forallTypeParameter $ var "lt"),
    field _ForallType_body $ ref coreEncodeTypeDef @@ (Core.forallTypeBody $ var "lt")]

coreEncodeLetDef :: TElement (Let -> Term)
coreEncodeLetDef = coreEncodingDefinition "Let" $
  lambda "l" $ encodedRecord _Let [
    field _Let_bindings $ encodedList (primitive _lists_map @@ ref coreEncodeLetBindingDef @@ (Core.letBindings $ var "l")),
    field _Let_environment $ ref coreEncodeTermDef @@ (Core.letEnvironment $ var "l")]

coreEncodeLetBindingDef :: TElement (LetBinding -> Term)
coreEncodeLetBindingDef = coreEncodingDefinition "LetBinding" $
  lambda "b" $ encodedRecord _LetBinding [
    field _LetBinding_name $ ref coreEncodeNameDef @@ (Core.letBindingName $ var "b"),
    field _LetBinding_term $ ref coreEncodeTermDef @@ (Core.letBindingTerm $ var "b"),
    field _LetBinding_type $ encodedOptional $ primitive _optionals_map @@ ref coreEncodeTypeSchemeDef @@ (Core.letBindingType $ var "b")]

coreEncodeLiteralDef :: TElement (Literal -> Term)
coreEncodeLiteralDef = coreEncodingDefinition "Literal" $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (ref coreEncodeFloatValueDef @@ var "v"),
    varField _Literal_integer (ref coreEncodeIntegerValueDef @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = field fname . lambda "v" . encodedVariant _Literal fname

coreEncodeLiteralTypeDef :: TElement (LiteralType -> Term)
coreEncodeLiteralTypeDef = coreEncodingDefinition "LiteralType" $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float coreEncodeFloatTypeDef,
    cs _LiteralType_integer coreEncodeIntegerTypeDef,
    csunit _LiteralType_string]
  where
    cs fname fun = field fname $ lambda "v" $ encodedVariant _LiteralType fname (ref fun @@ var "v")
    csunit fname = field fname $ constant $ TTerm $ coreEncodeTerm $ unTTerm $ variant _LiteralType fname unit

coreEncodeMapTypeDef :: TElement (MapType -> Term)
coreEncodeMapTypeDef = coreEncodingDefinition "MapType" $
    lambda "mt" $ encodedRecord _MapType [
      field _MapType_keys $ ref coreEncodeTypeDef @@ (Core.mapTypeKeys $ var "mt"),
      field _MapType_values $ ref coreEncodeTypeDef @@ (Core.mapTypeValues $ var "mt")]

coreEncodeNameDef :: TElement (Name -> Term)
coreEncodeNameDef = coreEncodingDefinition "Name" $
  lambda "fn" $ encodedWrappedTerm _Name $ encodedString $ unwrap _Name @@ var "fn"

coreEncodeProjectionDef :: TElement (Projection -> Term)
coreEncodeProjectionDef = coreEncodingDefinition "Projection" $
  lambda "p" $ encodedRecord _Projection [
    field _Projection_typeName $ ref coreEncodeNameDef @@ (Core.projectionTypeName $ var "p"),
    field _Projection_field $ ref coreEncodeNameDef @@ (Core.projectionField $ var "p")]

coreEncodeRecordDef :: TElement (Record -> Term)
coreEncodeRecordDef = coreEncodingDefinition "Record" $
  lambda "r" $ encodedRecord _Record [
    field _Record_typeName $ ref coreEncodeNameDef @@ (Core.recordTypeName $ var "r"),
    field _Record_fields $ encodedList (primitive _lists_map @@ (ref coreEncodeFieldDef) @@ (Core.recordFields $ var "r"))]

coreEncodeRowTypeDef :: TElement (RowType -> Term)
coreEncodeRowTypeDef = coreEncodingDefinition "RowType" $
  lambda "rt" $ encodedRecord _RowType [
    field _RowType_typeName $ ref coreEncodeNameDef @@ (Core.rowTypeTypeName $ var "rt"),
    field _RowType_fields $ encodedList (primitive _lists_map @@ ref coreEncodeFieldTypeDef @@ (Core.rowTypeFields $ var "rt"))]

coreEncodeSumDef :: TElement (Sum -> Term)
coreEncodeSumDef = coreEncodingDefinition "Sum" $
  lambda "s" $ encodedRecord _Sum [
    field _Sum_index $ encodedInt32 $ Core.sumIndex $ var "s",
    field _Sum_size $ encodedInt32 $ Core.sumSize $ var "s",
    field _Sum_term $ ref coreEncodeTermDef @@ (Core.sumTerm $ var "s")]

coreEncodeTermDef :: TElement (Term -> Term)
coreEncodeTermDef = coreEncodingDefinition "Term" $
  match _Term Nothing [
    ecase _Term_annotated (ref coreEncodeAnnotatedTermDef),
    ecase _Term_application (ref coreEncodeApplicationDef),
    ecase _Term_function (ref coreEncodeFunctionDef),
    ecase _Term_let (ref coreEncodeLetDef),
    ecase _Term_literal (ref coreEncodeLiteralDef),
    ecase2 _Term_list $ encodedList $ primitive _lists_map @@ (ref coreEncodeTermDef) @@ var "v",
    ecase2 _Term_map $ encodedMap (primitive _maps_bimap @@ ref coreEncodeTermDef @@ ref coreEncodeTermDef @@ var "v"),
    ecase2 _Term_optional $ encodedOptional (primitive _optionals_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase2 _Term_product $ encodedList (primitive _lists_map @@ ref coreEncodeTermDef @@ var "v"),
    ecase _Term_record (ref coreEncodeRecordDef),
    ecase2 _Term_set $ encodedSet $ primitive _sets_map @@ (ref coreEncodeTermDef) @@ var "v",
    ecase _Term_sum (ref coreEncodeSumDef),
    ecase _Term_typeAbstraction $ ref coreEncodeTypeAbstractionDef,
    ecase _Term_typeApplication $ ref coreEncodeTypedTermDef,
    ecase _Term_typed $ ref coreEncodeTypedTermDef,
    ecase _Term_union (ref coreEncodeInjectionDef),
    ecase _Term_variable $ ref coreEncodeNameDef,
    ecase _Term_wrap $ ref coreEncodeWrappedTermDef]
  where
    ecase = encodedCase _Term
    ecase2 fname = field fname . lambda "v" . encodedVariant _Term fname

coreEncodeTupleProjectionDef :: TElement (TupleProjection -> Term)
coreEncodeTupleProjectionDef = coreEncodingDefinition "TupleProjection" $
  lets [
    "encodeTypes">: lambda "types" $ encodedList $ primitive _lists_map @@ ref coreEncodeTypeDef @@ var "types"] $
    lambda "tp" $ encodedRecord _TupleProjection [
      field _TupleProjection_arity $ encodedInt32 $ Core.tupleProjectionArity $ var "tp",
      field _TupleProjection_index $ encodedInt32 $ Core.tupleProjectionIndex $ var "tp",
      field _TupleProjection_domain $ encodedOptional $ primitive _optionals_map @@ var "encodeTypes" @@ (Core.tupleProjectionDomain $ var "tp")]

coreEncodeTypeDef :: TElement (Type -> Term)
coreEncodeTypeDef = coreEncodingDefinition "Type" $
  match _Type Nothing [
    field _Type_annotated $ lambda "v" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
      field _AnnotatedTerm_subject $ ref coreEncodeTypeDef @@ (Core.annotatedTypeSubject $ var "v"),
      field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation $ var "v"],
    csref _Type_application coreEncodeApplicationTypeDef,
    csref _Type_function coreEncodeFunctionTypeDef,
    csref _Type_forall coreEncodeForallTypeDef,
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

coreEncodeTypeAbstractionDef :: TElement (TypeAbstraction -> Term)
coreEncodeTypeAbstractionDef = coreEncodingDefinition "TypeAbstraction" $
  lambda "l" $ encodedRecord _TypeAbstraction [
    field _TypeAbstraction_parameter $ ref coreEncodeNameDef @@ (project _TypeAbstraction _TypeAbstraction_parameter @@ var "l"),
    field _TypeAbstraction_body $ ref coreEncodeTermDef @@ (project _TypeAbstraction _TypeAbstraction_body @@ var "l")]

coreEncodeTypeSchemeDef :: TElement (TypeScheme -> Term)
coreEncodeTypeSchemeDef = coreEncodingDefinition "TypeScheme" $
  lambda "ts" $ encodedRecord _TypeScheme [
    field _TypeScheme_variables $ encodedList (primitive _lists_map @@ ref coreEncodeNameDef @@ (Core.typeSchemeVariables $ var "ts")),
    field _TypeScheme_type $ ref coreEncodeTypeDef @@ (Core.typeSchemeType $ var "ts")]

coreEncodeTypedTermDef :: TElement (TypedTerm -> Term)
coreEncodeTypedTermDef = coreEncodingDefinition "TypedTerm" $
  lambda "tt" $ encodedRecord _TypedTerm [
    field _TypedTerm_term $ ref coreEncodeTermDef @@ (project _TypedTerm _TypedTerm_term @@ var "tt"),
    field _TypedTerm_type $ ref coreEncodeTypeDef @@ (project _TypedTerm _TypedTerm_type @@ var "tt")]

coreEncodeWrappedTermDef :: TElement (WrappedTerm -> Term)
coreEncodeWrappedTermDef = coreEncodingDefinition "WrappedTerm" $
  lambda "n" $ encodedRecord _WrappedTerm [
    field _WrappedTerm_typeName $ ref coreEncodeNameDef @@ (Core.wrappedTermTypeName $ var "n"),
    field _WrappedTerm_object $ ref coreEncodeTermDef @@ (Core.wrappedTermObject $ var "n")]

coreEncodeWrappedTypeDef :: TElement (WrappedType -> Term)
coreEncodeWrappedTypeDef = coreEncodingDefinition "WrappedType" $
  lambda "nt" $ encodedRecord _WrappedType [
    field _WrappedType_typeName $ ref coreEncodeNameDef @@ (Core.wrappedTypeTypeName $ var "nt"),
    field _WrappedType_object $ ref coreEncodeTypeDef @@ (Core.wrappedTypeObject $ var "nt")]

-- Extra elements

isEncodedTypeDef :: TElement (Term -> Bool)
isEncodedTypeDef = coreEncodingExtrasDefinition "isEncodedType" $
  lambda "t" $ (match _Term (Just false) [
      TCase _Term_application --> lambda "a" $
        ref isEncodedTypeDef @@ (Core.applicationFunction $ var "a"),
      TCase _Term_union       --> lambda "i" $
        Equality.equalString (string $ unName _Type) (Core.unName $ (Core.injectionTypeName $ var "i"))
    ]) @@ (ref Strip.stripTermDef @@ var "t")

isTypeDef :: TElement (Type -> Bool)
isTypeDef = coreEncodingExtrasDefinition "isType" $
  lambda "t" $ (match _Type (Just false) [
      TCase _Type_application --> lambda "a" $
        ref isTypeDef @@ (Core.applicationTypeFunction $ var "a"),
      TCase _Type_forall --> lambda "l" $
        ref isTypeDef @@ (Core.forallTypeBody $ var "l"),
      TCase _Type_union --> lambda "rt" $
        Equality.equalString (string $ unName _Type) (Core.unName $ (Core.rowTypeTypeName $ var "rt"))
--      TCase _Type_variable --> constant true
    ]) @@ (ref Strip.stripTypeDef @@ var "t")

isUnitTermDef :: TElement (Term -> Bool)
isUnitTermDef = coreEncodingExtrasDefinition "isUnitTerm" $
  lambda "t" $ Equality.equalTerm (ref Strip.fullyStripTermDef @@ var "t") $ TTerm (coreEncodeTerm Terms.unit)

isUnitTypeDef :: TElement (Type -> Bool)
isUnitTypeDef = coreEncodingExtrasDefinition "isUnitType" $
  lambda "t" $ Equality.equalType (ref Strip.stripTypeDef @@ var "t") $ TTerm (coreEncodeType Types.unit)
