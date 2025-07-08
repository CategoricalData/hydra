{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Encode.Core where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting

import qualified Hydra.Encode.Core as EncodeCore


module_ :: Module
module_ = Module (Namespace "hydra.encode.core") elements
    [Rewriting.module_]
    [KernelTypes.hydraCoreModule] $
    Just ("Mapping of hydra.core constructs in a host language like Haskell or Java "
      <> " to their native Hydra counterparts as terms. "
      <> " This includes an implementation of LambdaGraph's epsilon encoding (types to terms).")
  where
    elements = encodingElements <> extraElements
    encodingElements = [
      el annotatedTermDef,
      el annotatedTypeDef,
      el applicationDef,
      el applicationTypeDef,
      el caseStatementDef,
      el eliminationDef,
      el fieldDef,
      el fieldTypeDef,
      el floatTypeDef,
      el floatValueDef,
      el functionDef,
      el functionTypeDef,
      el injectionDef,
      el integerTypeDef,
      el integerValueDef,
      el lambdaDef,
      el forallTypeDef,
      el letDef,
      el letBindingDef,
      el literalDef,
      el literalTypeDef,
      el mapTypeDef,
      el nameDef,
      el projectionDef,
      el recordDef,
      el rowTypeDef,
      el sumDef,
      el termDef,
      el tupleProjectionDef,
      el typeDef,
      el typeAbstractionDef,
      el typeSchemeDef,
      el typedTermDef,
      el wrappedTermDef,
      el wrappedTypeDef]
    -- TODO: move these into another module
    extraElements = [
      el isEncodedTypeDef,
      el isTypeDef,
      el isUnitTermDef,
      el isUnitTypeDef]

define :: String -> TTerm x -> TElement x
define label = definitionInModule module_ (decapitalize label)

coreEncodingExtrasDefinition :: String -> TTerm a -> TElement a
coreEncodingExtrasDefinition = definitionInModule module_

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

annotatedTermDef :: TElement (AnnotatedTerm -> Term)
annotatedTermDef = define "AnnotatedTerm" $
  lambda "a" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref termDef @@ (Core.annotatedTermSubject $ var "a"),
    field _AnnotatedTerm_annotation $ Core.annotatedTermAnnotation $ var "a"]

annotatedTypeDef :: TElement (AnnotatedType -> Term)
annotatedTypeDef = define "AnnotatedType" $
  lambda "at" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
    field _AnnotatedTerm_subject $ ref typeDef @@ (Core.annotatedTypeSubject $ var "at"),
    field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation $ var "at"]

applicationDef :: TElement (Application -> Term)
applicationDef = define "Application" $
  lambda "app" $ encodedRecord _Application [
    field _Application_function $ ref termDef @@ (Core.applicationFunction $ var "app"),
    field _Application_argument $ ref termDef @@ (Core.applicationArgument $ var "app")]

applicationTypeDef :: TElement (ApplicationType -> Term)
applicationTypeDef = define "ApplicationType" $
  lambda "at" $ encodedRecord _ApplicationType [
    field _ApplicationType_function $ ref typeDef @@ (Core.applicationTypeFunction $ var "at"),
    field _ApplicationType_argument $ ref typeDef @@ (Core.applicationTypeArgument $ var "at")]

caseStatementDef :: TElement (CaseStatement -> Term)
caseStatementDef = define "CaseStatement" $
  lambda "cs" $ encodedRecord _CaseStatement [
    field _CaseStatement_typeName $ ref nameDef @@ (Core.caseStatementTypeName $ var "cs"),
    field _CaseStatement_default $ encodedOptional
      (primitive _optionals_map @@ ref termDef @@ (Core.caseStatementDefault $ var "cs")),
    field _CaseStatement_cases $ encodedList
      (primitive _lists_map @@ ref fieldDef @@ (Core.caseStatementCases $ var "cs"))]

eliminationDef :: TElement (Elimination -> Term)
eliminationDef = define "Elimination" $
    match _Elimination Nothing [
      ecase _Elimination_product tupleProjectionDef,
      ecase _Elimination_record projectionDef,
      ecase _Elimination_union caseStatementDef,
      ecase _Elimination_wrap nameDef]
  where
    ecase fname funname = encodedCase _Elimination fname (ref funname)

fieldDef :: TElement (Field -> Term)
fieldDef = define "Field" $
  lambda "f" $ encodedRecord _Field [
    field _Field_name $ encodedWrappedTerm _Name $ encodedString $ (unwrap _Name @@ (Core.fieldName $ var "f")),
    field _Field_term $ ref termDef @@ (Core.fieldTerm $ var "f")]

fieldTypeDef :: TElement (FieldType -> Term)
fieldTypeDef = define "FieldType" $
  lambda "ft" $ encodedRecord _FieldType [
    field _FieldType_name $ ref nameDef @@ (Core.fieldTypeName $ var "ft"),
    field _FieldType_type $ ref typeDef @@ (Core.fieldTypeType $ var "ft")]

floatTypeDef :: TElement (FloatType -> Term)
floatTypeDef = define "FloatType" $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ unitVariant _FloatType fname

floatValueDef :: TElement (FloatValue -> Term)
floatValueDef = define "FloatValue" $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = field fname $ lambda "v" $ encodedVariant _FloatValue fname $ encodedFloatValue $
      variant _FloatValue fname $ var "v"

functionDef :: TElement (Function -> Term)
functionDef = define "Function" $
    match _Function Nothing [
      ecase _Function_elimination eliminationDef,
      ecase _Function_lambda lambdaDef,
      ecase _Function_primitive nameDef]
  where
    ecase fname funname = encodedCase _Function fname (ref funname)

functionTypeDef :: TElement (FunctionType -> Term)
functionTypeDef = define "FunctionType" $
  lambda "ft" $ encodedRecord _FunctionType [
    field _FunctionType_domain $ ref typeDef @@ (Core.functionTypeDomain $ var "ft"),
    field _FunctionType_codomain $ ref typeDef @@ (Core.functionTypeCodomain $ var "ft")]

injectionDef :: TElement (Injection -> Term)
injectionDef = define "Injection" $
  lambda "i" $ encodedRecord _Injection [
    field _Injection_typeName $ ref nameDef @@ (Core.injectionTypeName $ var "i"),
    field _Injection_field $ ref fieldDef @@ (Core.injectionField $ var "i")]

integerTypeDef :: TElement (IntegerType -> Term)
integerTypeDef = define "IntegerType" $
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
    cs fname = field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ unitVariant _IntegerType fname

integerValueDef :: TElement (IntegerValue -> Term)
integerValueDef = define "IntegerValue" $
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

lambdaDef :: TElement (Lambda -> Term)
lambdaDef = define "Lambda" $
  lambda "l" $ encodedRecord _Lambda [
    field _Lambda_parameter $ ref nameDef @@ (Core.lambdaParameter $ var "l"),
    field _Lambda_domain $ encodedOptional $ primitive _optionals_map @@ ref typeDef @@ (Core.lambdaDomain $ var "l"),
    field _Lambda_body $ ref termDef @@ (Core.lambdaBody $ var "l")]

forallTypeDef :: TElement (ForallType -> Term)
forallTypeDef = define "ForallType" $
  lambda "lt" $ encodedRecord _ForallType [
    field _ForallType_parameter $ ref nameDef @@ (Core.forallTypeParameter $ var "lt"),
    field _ForallType_body $ ref typeDef @@ (Core.forallTypeBody $ var "lt")]

letDef :: TElement (Let -> Term)
letDef = define "Let" $
  lambda "l" $ encodedRecord _Let [
    field _Let_bindings $ encodedList (primitive _lists_map @@ ref letBindingDef @@ (Core.letBindings $ var "l")),
    field _Let_environment $ ref termDef @@ (Core.letEnvironment $ var "l")]

letBindingDef :: TElement (LetBinding -> Term)
letBindingDef = define "LetBinding" $
  lambda "b" $ encodedRecord _LetBinding [
    field _LetBinding_name $ ref nameDef @@ (Core.letBindingName $ var "b"),
    field _LetBinding_term $ ref termDef @@ (Core.letBindingTerm $ var "b"),
    field _LetBinding_type $ encodedOptional $ primitive _optionals_map @@ ref typeSchemeDef @@ (Core.letBindingType $ var "b")]

literalDef :: TElement (Literal -> Term)
literalDef = define "Literal" $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (ref floatValueDef @@ var "v"),
    varField _Literal_integer (ref integerValueDef @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = field fname . lambda "v" . encodedVariant _Literal fname

literalTypeDef :: TElement (LiteralType -> Term)
literalTypeDef = define "LiteralType" $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float floatTypeDef,
    cs _LiteralType_integer integerTypeDef,
    csunit _LiteralType_string]
  where
    cs fname fun = field fname $ lambda "v" $ encodedVariant _LiteralType fname (ref fun @@ var "v")
    csunit fname = field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ variant _LiteralType fname unit

mapTypeDef :: TElement (MapType -> Term)
mapTypeDef = define "MapType" $
    lambda "mt" $ encodedRecord _MapType [
      field _MapType_keys $ ref typeDef @@ (Core.mapTypeKeys $ var "mt"),
      field _MapType_values $ ref typeDef @@ (Core.mapTypeValues $ var "mt")]

nameDef :: TElement (Name -> Term)
nameDef = define "Name" $
  lambda "fn" $ encodedWrappedTerm _Name $ encodedString $ unwrap _Name @@ var "fn"

projectionDef :: TElement (Projection -> Term)
projectionDef = define "Projection" $
  lambda "p" $ encodedRecord _Projection [
    field _Projection_typeName $ ref nameDef @@ (Core.projectionTypeName $ var "p"),
    field _Projection_field $ ref nameDef @@ (Core.projectionField $ var "p")]

recordDef :: TElement (Record -> Term)
recordDef = define "Record" $
  lambda "r" $ encodedRecord _Record [
    field _Record_typeName $ ref nameDef @@ (Core.recordTypeName $ var "r"),
    field _Record_fields $ encodedList (primitive _lists_map @@ (ref fieldDef) @@ (Core.recordFields $ var "r"))]

rowTypeDef :: TElement (RowType -> Term)
rowTypeDef = define "RowType" $
  lambda "rt" $ encodedRecord _RowType [
    field _RowType_typeName $ ref nameDef @@ (Core.rowTypeTypeName $ var "rt"),
    field _RowType_fields $ encodedList (primitive _lists_map @@ ref fieldTypeDef @@ (Core.rowTypeFields $ var "rt"))]

sumDef :: TElement (Sum -> Term)
sumDef = define "Sum" $
  lambda "s" $ encodedRecord _Sum [
    field _Sum_index $ encodedInt32 $ Core.sumIndex $ var "s",
    field _Sum_size $ encodedInt32 $ Core.sumSize $ var "s",
    field _Sum_term $ ref termDef @@ (Core.sumTerm $ var "s")]

termDef :: TElement (Term -> Term)
termDef = define "Term" $
  match _Term Nothing [
    ecase _Term_annotated (ref annotatedTermDef),
    ecase _Term_application (ref applicationDef),
    ecase _Term_function (ref functionDef),
    ecase _Term_let (ref letDef),
    ecase _Term_literal (ref literalDef),
    ecase2 _Term_list $ encodedList $ primitive _lists_map @@ (ref termDef) @@ var "v",
    ecase2 _Term_map $ encodedMap (primitive _maps_bimap @@ ref termDef @@ ref termDef @@ var "v"),
    ecase2 _Term_optional $ encodedOptional (primitive _optionals_map @@ ref termDef @@ var "v"),
    ecase2 _Term_product $ encodedList (primitive _lists_map @@ ref termDef @@ var "v"),
    ecase _Term_record (ref recordDef),
    ecase2 _Term_set $ encodedSet $ primitive _sets_map @@ (ref termDef) @@ var "v",
    ecase _Term_sum (ref sumDef),
    ecase _Term_typeAbstraction $ ref typeAbstractionDef,
    ecase _Term_typeApplication $ ref typedTermDef,
    ecase _Term_union (ref injectionDef),
    ecase _Term_unit $ constant Core.termUnit,
    ecase _Term_variable $ ref nameDef,
    ecase _Term_wrap $ ref wrappedTermDef]
  where
    ecase = encodedCase _Term
    ecase2 fname = field fname . lambda "v" . encodedVariant _Term fname

tupleProjectionDef :: TElement (TupleProjection -> Term)
tupleProjectionDef = define "TupleProjection" $
  lets [
    "encodeTypes">: lambda "types" $ encodedList $ primitive _lists_map @@ ref typeDef @@ var "types"] $
    lambda "tp" $ encodedRecord _TupleProjection [
      field _TupleProjection_arity $ encodedInt32 $ Core.tupleProjectionArity $ var "tp",
      field _TupleProjection_index $ encodedInt32 $ Core.tupleProjectionIndex $ var "tp",
      field _TupleProjection_domain $ encodedOptional $ primitive _optionals_map @@ var "encodeTypes" @@ (Core.tupleProjectionDomain $ var "tp")]

typeDef :: TElement (Type -> Term)
typeDef = define "Type" $
  match _Type Nothing [
    field _Type_annotated $ lambda "v" $ variant _Term _Term_annotated $ record _AnnotatedTerm [
      field _AnnotatedTerm_subject $ ref typeDef @@ (Core.annotatedTypeSubject $ var "v"),
      field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation $ var "v"],
    csref _Type_application applicationTypeDef,
    csref _Type_function functionTypeDef,
    csref _Type_forall forallTypeDef,
    csref _Type_list typeDef,
    csref _Type_literal literalTypeDef,
    csref _Type_map mapTypeDef,
    csref _Type_optional typeDef,
    cs _Type_product $ encodedList $ primitive _lists_map @@ ref typeDef @@ var "v",
    csref _Type_record rowTypeDef,
    csref _Type_set typeDef,
    cs _Type_sum $ encodedList $ primitive _lists_map @@ ref typeDef @@ var "v",
    csref _Type_union rowTypeDef,
    field _Type_unit $ constant $ encodedVariant _Type _Type_unit Core.termUnit,
    csref _Type_variable nameDef,
    csref _Type_wrap wrappedTypeDef]
  where
    cs fname term = field fname $ lambda "v" $ encodedVariant _Type fname term
    csref fname fun = cs fname (ref fun @@ var "v")

typeAbstractionDef :: TElement (TypeAbstraction -> Term)
typeAbstractionDef = define "TypeAbstraction" $
  lambda "l" $ encodedRecord _TypeAbstraction [
    field _TypeAbstraction_parameter $ ref nameDef @@ (project _TypeAbstraction _TypeAbstraction_parameter @@ var "l"),
    field _TypeAbstraction_body $ ref termDef @@ (project _TypeAbstraction _TypeAbstraction_body @@ var "l")]

typeSchemeDef :: TElement (TypeScheme -> Term)
typeSchemeDef = define "TypeScheme" $
  lambda "ts" $ encodedRecord _TypeScheme [
    field _TypeScheme_variables $ encodedList (primitive _lists_map @@ ref nameDef @@ (Core.typeSchemeVariables $ var "ts")),
    field _TypeScheme_type $ ref typeDef @@ (Core.typeSchemeType $ var "ts")]

typedTermDef :: TElement (TypedTerm -> Term)
typedTermDef = define "TypedTerm" $
  lambda "tt" $ encodedRecord _TypedTerm [
    field _TypedTerm_term $ ref termDef @@ (project _TypedTerm _TypedTerm_term @@ var "tt"),
    field _TypedTerm_type $ ref typeDef @@ (project _TypedTerm _TypedTerm_type @@ var "tt")]

wrappedTermDef :: TElement (WrappedTerm -> Term)
wrappedTermDef = define "WrappedTerm" $
  lambda "n" $ encodedRecord _WrappedTerm [
    field _WrappedTerm_typeName $ ref nameDef @@ (Core.wrappedTermTypeName $ var "n"),
    field _WrappedTerm_object $ ref termDef @@ (Core.wrappedTermObject $ var "n")]

wrappedTypeDef :: TElement (WrappedType -> Term)
wrappedTypeDef = define "WrappedType" $
  lambda "nt" $ encodedRecord _WrappedType [
    field _WrappedType_typeName $ ref nameDef @@ (Core.wrappedTypeTypeName $ var "nt"),
    field _WrappedType_object $ ref typeDef @@ (Core.wrappedTypeObject $ var "nt")]

-- TODO: move these into another module

isEncodedTypeDef :: TElement (Term -> Bool)
isEncodedTypeDef = coreEncodingExtrasDefinition "isEncodedType" $
  doc "Determines whether a given term is an encoded type" $
  lambda "t" $ cases _Term (ref Rewriting.deannotateTermDef @@ var "t") (Just false) [
    _Term_application>>: lambda "a" $
      ref isEncodedTypeDef @@ (Core.applicationFunction $ var "a"),
    _Term_union>>: lambda "i" $
      Equality.equal (string $ unName _Type) (Core.unName $ (Core.injectionTypeName $ var "i"))]

isTypeDef :: TElement (Type -> Bool)
isTypeDef = coreEncodingExtrasDefinition "isType" $
  lambda "t" $ cases _Type (ref Rewriting.deannotateTypeDef @@ var "t") (Just false) [
    _Type_application>>: lambda "a" $
      ref isTypeDef @@ (Core.applicationTypeFunction $ var "a"),
    _Type_forall>>: lambda "l" $
      ref isTypeDef @@ (Core.forallTypeBody $ var "l"),
    _Type_union>>: lambda "rt" $
      Equality.equal (string $ unName _Type) (Core.unName $ (Core.rowTypeTypeName $ var "rt")),
    _Type_variable>>: lambda "v" $ Equality.equal (var "v") (Core.nameLift _Type)]

isUnitTermDef :: TElement (Term -> Bool)
isUnitTermDef = coreEncodingExtrasDefinition "isUnitTerm" $
  match _Term (Just false) [_Term_unit>>: constant true]

isUnitTypeDef :: TElement (Type -> Bool)
isUnitTypeDef = coreEncodingExtrasDefinition "isUnitType" $
  match _Type (Just false) [_Type_unit>>: constant true]
