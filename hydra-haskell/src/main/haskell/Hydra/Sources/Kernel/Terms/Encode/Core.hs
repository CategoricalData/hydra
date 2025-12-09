{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Encode.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (literalType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import           Hydra.Dsl.Meta.Phantoms as Phantoms hiding (
  elimination, field, fieldType, floatType, floatValue, function, injection, integerType, integerValue, lambda, literal,
  literalType, record, term, type_, typeScheme, wrap)
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
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
    kernelTypesModules $
    Just ("Mapping of hydra.core constructs in a host language like Haskell or Java "
      <> " to their native Hydra counterparts as terms. "
      <> " This includes an implementation of LambdaGraph's epsilon encoding (types to terms).")
  where
    elements = encodingElements <> extraElements
    encodingElements = [
      toBinding annotatedTerm,
      toBinding annotatedType,
      toBinding application,
      toBinding applicationType,
      toBinding caseStatement,
      toBinding elimination,
      toBinding field,
      toBinding fieldType,
      toBinding floatType,
      toBinding floatValue,
      toBinding eitherType,
      toBinding pairType,
      toBinding function,
      toBinding functionType,
      toBinding injection,
      toBinding integerType,
      toBinding integerValue,
      toBinding lambda,
      toBinding forallType,
      toBinding let_,
      toBinding letBinding,
      toBinding literal,
      toBinding literalType,
      toBinding mapType,
      toBinding name,
      toBinding projection,
      toBinding record,
      toBinding rowType,
      toBinding term,
      toBinding type_,
      toBinding typeLambda,
      toBinding typeScheme,
      toBinding typeApplicationTerm,
      toBinding wrappedTerm,
      toBinding wrappedType]
    -- TODO: move these into another module
    extraElements = [
      toBinding isEncodedType,
      toBinding isType,
      toBinding isUnitTerm,
      toBinding isUnitType]

define :: String -> TTerm x -> TBinding x
define label = definitionInModule module_ (decapitalize label)

coreEncodingExtrasDefinition :: String -> TTerm a -> TBinding a
coreEncodingExtrasDefinition = definitionInModule module_

encodedBinary :: TTerm String -> TTerm Term
encodedBinary = encodedLiteral . Core.literalBinary

encodedBoolean :: TTerm Bool -> TTerm Term
encodedBoolean = encodedLiteral . Core.literalBoolean

encodedCase :: AsTerm t (a -> Term) => Name -> Name -> t -> Field
encodedCase tname fname enc = Phantoms.field fname $ "v" ~> encodedVariant tname fname (enc @@ var "v")

encodedEither :: TTerm (Prelude.Either a b) -> TTerm Term
encodedEither = inject _Term _Term_either

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
  Phantoms.field _Injection_typeName $ encodedName tname,
  Phantoms.field _Injection_field $ encodedField fname term]

encodedInt32 :: TTerm Int -> TTerm Term
encodedInt32 v = encodedIntegerValue $ inject _IntegerValue _IntegerValue_int32 v

encodedIntegerValue :: TTerm IntegerValue -> TTerm Term
encodedIntegerValue = encodedLiteral . Core.literalInteger

encodedList :: TTerm [a] -> TTerm Term
encodedList = inject _Term _Term_list

encodedLiteral :: TTerm Literal -> TTerm Term
encodedLiteral = inject _Term _Term_literal

encodedMap :: TTerm (M.Map k v) -> TTerm Term
encodedMap = inject _Term _Term_map

encodedName :: Name -> TTerm Name
encodedName = Phantoms.wrap _Name . string . unName

encodedUnit :: TTerm Term
encodedUnit = injectUnit _Term _Term_unit

encodedWrappedTerm :: Name -> TTerm Term -> TTerm Term
encodedWrappedTerm name = encodedWrappedTermRaw (encodedName name)

encodedWrappedTermRaw :: TTerm Name -> TTerm Term -> TTerm Term
encodedWrappedTermRaw (TTerm name) (TTerm term) = TTerm $ Terms.inject _Term _Term_wrap $ Terms.record _WrappedTerm [
  Field _WrappedTerm_typeName name,
  Field _WrappedTerm_body term]

encodedOptional :: TTerm (Maybe a) -> TTerm Term
encodedOptional = inject _Term _Term_maybe

encodedPair :: TTerm (a, b) -> TTerm Term
encodedPair = inject _Term _Term_pair

encodedRecord :: Name -> [Field] -> TTerm Term
encodedRecord tname fields = TTerm $ Terms.inject _Term _Term_record $ Terms.record _Record [
    Phantoms.field _Record_typeName $ encodedName tname,
    Phantoms.field _Record_fields $ list (encField <$> fields)]
  where
    encField (Field fname term) = encodedField fname $ TTerm term

encodedSet :: TTerm (S.Set a) -> TTerm Term
encodedSet = inject _Term _Term_set

encodedString :: TTerm String -> TTerm Term
encodedString = encodedLiteral . inject _Literal _Literal_string

encodedUnion :: TTerm Term -> TTerm Term
encodedUnion = inject _Term _Term_union

encodedVariant :: Name -> Name -> TTerm Term -> TTerm Term
encodedVariant tname fname term = encodedUnion $ encodedInjection tname fname term

annotatedTerm :: TBinding (AnnotatedTerm -> Term)
annotatedTerm = define "AnnotatedTerm" $
  doc "Encode an annotated term as a term" $
  "a" ~> inject _Term _Term_annotated (Phantoms.record _AnnotatedTerm [
    Phantoms.field _AnnotatedTerm_body (term @@ (Core.annotatedTermBody (var "a"))),
    Phantoms.field _AnnotatedTerm_annotation (Core.annotatedTermAnnotation (var "a"))])

annotatedType :: TBinding (AnnotatedType -> Term)
annotatedType = define "AnnotatedType" $
  doc "Encode an annotated type as a term" $
  "at" ~> inject _Term _Term_annotated (Phantoms.record _AnnotatedTerm [
    Phantoms.field _AnnotatedTerm_body (type_ @@ (Core.annotatedTypeBody (var "at"))),
    Phantoms.field _AnnotatedTerm_annotation (Core.annotatedTypeAnnotation (var "at"))])

application :: TBinding (Application -> Term)
application = define "Application" $
  doc "Encode an application as a term" $
  "app" ~> encodedRecord _Application [
    Phantoms.field _Application_function (term @@ (Core.applicationFunction (var "app"))),
    Phantoms.field _Application_argument (term @@ (Core.applicationArgument (var "app")))]

applicationType :: TBinding (ApplicationType -> Term)
applicationType = define "ApplicationType" $
  doc "Encode an application type as a term" $
  "at" ~> encodedRecord _ApplicationType [
    Phantoms.field _ApplicationType_function (type_ @@ (Core.applicationTypeFunction (var "at"))),
    Phantoms.field _ApplicationType_argument (type_ @@ (Core.applicationTypeArgument (var "at")))]

caseStatement :: TBinding (CaseStatement -> Term)
caseStatement = define "CaseStatement" $
  doc "Encode a case statement as a term" $
  "cs" ~> encodedRecord _CaseStatement [
    Phantoms.field _CaseStatement_typeName (name @@ (Core.caseStatementTypeName (var "cs"))),
    Phantoms.field _CaseStatement_default (encodedOptional
      (primitive _maybes_map @@ term @@ (Core.caseStatementDefault (var "cs")))),
    Phantoms.field _CaseStatement_cases (encodedList
      (primitive _lists_map @@ field @@ (Core.caseStatementCases (var "cs"))))]

elimination :: TBinding (Elimination -> Term)
elimination = define "Elimination" $
  doc "Encode an elimination as a term" $
    match _Elimination Nothing [
      encodedCase _Elimination _Elimination_record projection,
      encodedCase _Elimination _Elimination_union caseStatement,
      encodedCase _Elimination _Elimination_wrap name]

field :: TBinding (Field -> Term)
field = define "Field" $
  doc "Encode a field as a term" $
  "f" ~> encodedRecord _Field [
    Phantoms.field _Field_name (encodedWrappedTerm _Name (encodedString (unwrap _Name @@ (Core.fieldName (var "f"))))),
    Phantoms.field _Field_term (term @@ (Core.fieldTerm (var "f")))]

fieldType :: TBinding (FieldType -> Term)
fieldType = define "FieldType" $
  doc "Encode a field type as a term" $
  "ft" ~> encodedRecord _FieldType [
    Phantoms.field _FieldType_name (name @@ (Core.fieldTypeName (var "ft"))),
    Phantoms.field _FieldType_type (type_ @@ (Core.fieldTypeType (var "ft")))]

floatType :: TBinding (FloatType -> Term)
floatType = define "FloatType" $
  doc "Encode a floating-point type as a term" $
    match _FloatType Nothing (cs <$> [
      _FloatType_bigfloat,
      _FloatType_float32,
      _FloatType_float64])
  where
    cs fname = Phantoms.field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ injectUnit _FloatType fname

floatValue :: TBinding (FloatValue -> Term)
floatValue = define "FloatValue" $
  doc "Encode a floating-point value as a term" $
  match _FloatValue Nothing (varField <$> [
    _FloatValue_bigfloat,
    _FloatValue_float32,
    _FloatValue_float64])
  where
    varField fname = Phantoms.field fname $ "v" ~> (encodedVariant _FloatValue fname $ encodedFloatValue $
      inject _FloatValue fname $ var "v")

eitherType :: TBinding (EitherType -> Term)
eitherType = define "EitherType" $
  doc "Encode an either type as a term" $
  "et" ~> encodedRecord _EitherType [
    Phantoms.field _EitherType_left (type_ @@ (Core.eitherTypeLeft (var "et"))),
    Phantoms.field _EitherType_right (type_ @@ (Core.eitherTypeRight (var "et")))]

pairType :: TBinding (PairType -> Term)
pairType = define "PairType" $
  doc "Encode a pair type as a term" $
  "pt" ~> encodedRecord _PairType [
    Phantoms.field _PairType_first (type_ @@ (Core.pairTypeFirst (var "pt"))),
    Phantoms.field _PairType_second (type_ @@ (Core.pairTypeSecond (var "pt")))]

function :: TBinding (Function -> Term)
function = define "Function" $
  doc "Encode a function as a term" $
    match _Function Nothing [
      encodedCase _Function _Function_elimination elimination,
      encodedCase _Function _Function_lambda lambda,
      encodedCase _Function _Function_primitive name]

functionType :: TBinding (FunctionType -> Term)
functionType = define "FunctionType" $
  doc "Encode a function type as a term" $
  "ft" ~> encodedRecord _FunctionType [
    Phantoms.field _FunctionType_domain (type_ @@ (Core.functionTypeDomain (var "ft"))),
    Phantoms.field _FunctionType_codomain (type_ @@ (Core.functionTypeCodomain (var "ft")))]

injection :: TBinding (Injection -> Term)
injection = define "Injection" $
  doc "Encode an injection as a term" $
  "i" ~> encodedRecord _Injection [
    Phantoms.field _Injection_typeName (name @@ (Core.injectionTypeName (var "i"))),
    Phantoms.field _Injection_field (field @@ (Core.injectionField (var "i")))]

integerType :: TBinding (IntegerType -> Term)
integerType = define "IntegerType" $
  doc "Encode an integer type as a term" $
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
    cs fname = Phantoms.field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ injectUnit _IntegerType fname

integerValue :: TBinding (IntegerValue -> Term)
integerValue = define "IntegerValue" $
  doc "Encode an integer value as a term" $
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
    varField fname = Phantoms.field fname $ Phantoms.lambda "v" $ encodedVariant _IntegerValue fname $ encodedIntegerValue $
      inject _IntegerValue fname $ var "v"

lambda :: TBinding (Lambda -> Term)
lambda = define "Lambda" $
  doc "Encode a lambda as a term" $
  "l" ~> encodedRecord _Lambda [
    Phantoms.field _Lambda_parameter (name @@ (Core.lambdaParameter (var "l"))),
    Phantoms.field _Lambda_domain (encodedOptional (primitive _maybes_map @@ type_ @@ (Core.lambdaDomain (var "l")))),
    Phantoms.field _Lambda_body (term @@ (Core.lambdaBody (var "l")))]

forallType :: TBinding (ForallType -> Term)
forallType = define "ForallType" $
  doc "Encode a forall type as a term" $
  "lt" ~> encodedRecord _ForallType [
    Phantoms.field _ForallType_parameter (name @@ (Core.forallTypeParameter (var "lt"))),
    Phantoms.field _ForallType_body (type_ @@ (Core.forallTypeBody (var "lt")))]

let_ :: TBinding (Let -> Term)
let_ = define "Let" $
  doc "Encode a let expression as a term" $
  "l" ~> encodedRecord _Let [
    Phantoms.field _Let_bindings (encodedList (primitive _lists_map @@ letBinding @@ (Core.letBindings (var "l")))),
    Phantoms.field _Let_body (term @@ (Core.letBody (var "l")))]

letBinding :: TBinding (Binding -> Term)
letBinding = define "Binding" $
  doc "Encode a let binding as a term" $
  "b" ~> encodedRecord _Binding [
    Phantoms.field _Binding_name (name @@ (Core.bindingName (var "b"))),
    Phantoms.field _Binding_term (term @@ (Core.bindingTerm (var "b"))),
    Phantoms.field _Binding_type (encodedOptional (primitive _maybes_map @@ typeScheme @@ (Core.bindingType (var "b"))))]

literal :: TBinding (Literal -> Term)
literal = define "Literal" $
  doc "Encode a literal as a term" $
  match _Literal Nothing [
    varField _Literal_binary $ encodedBinary $ var "v",
    varField _Literal_boolean $ encodedBoolean $ var "v",
    varField _Literal_float (floatValue @@ var "v"),
    varField _Literal_integer (integerValue @@ var "v"),
    varField _Literal_string $ encodedString $ var "v"]
  where
    varField fname = Phantoms.field fname . Phantoms.lambda "v" . encodedVariant _Literal fname

literalType :: TBinding (LiteralType -> Term)
literalType = define "LiteralType" $
  doc "Encode a literal type as a term" $
  match _LiteralType Nothing [
    csunit _LiteralType_binary,
    csunit _LiteralType_boolean,
    cs _LiteralType_float floatType,
    cs _LiteralType_integer integerType,
    csunit _LiteralType_string]
  where
    cs fname fun = Phantoms.field fname $ Phantoms.lambda "v" $ encodedVariant _LiteralType fname (fun @@ var "v")
    csunit fname = Phantoms.field fname $ constant $ TTerm $ EncodeCore.term $ unTTerm $ inject _LiteralType fname unit

mapType :: TBinding (MapType -> Term)
mapType = define "MapType" $
  doc "Encode a map type as a term" $
  "mt" ~> encodedRecord _MapType [
    Phantoms.field _MapType_keys (type_ @@ (Core.mapTypeKeys (var "mt"))),
    Phantoms.field _MapType_values (type_ @@ (Core.mapTypeValues (var "mt")))]

name :: TBinding (Name -> Term)
name = define "Name" $
  doc "Encode a name as a term" $
  "fn" ~> encodedWrappedTerm _Name (encodedString (unwrap _Name @@ var "fn"))

projection :: TBinding (Projection -> Term)
projection = define "Projection" $
  doc "Encode a projection as a term" $
  "p" ~> encodedRecord _Projection [
    Phantoms.field _Projection_typeName (name @@ (Core.projectionTypeName (var "p"))),
    Phantoms.field _Projection_field (name @@ (Core.projectionField (var "p")))]

record :: TBinding (Record -> Term)
record = define "Record" $
  doc "Encode a record as a term" $
  "r" ~> encodedRecord _Record [
    Phantoms.field _Record_typeName (name @@ (Core.recordTypeName (var "r"))),
    Phantoms.field _Record_fields (encodedList (primitive _lists_map @@ (field) @@ (Core.recordFields (var "r"))))]

rowType :: TBinding (RowType -> Term)
rowType = define "RowType" $
  doc "Encode a row type as a term" $
  "rt" ~> encodedRecord _RowType [
    Phantoms.field _RowType_typeName (name @@ (Core.rowTypeTypeName (var "rt"))),
    Phantoms.field _RowType_fields (encodedList (primitive _lists_map @@ fieldType @@ (Core.rowTypeFields (var "rt"))))]

term :: TBinding (Term -> Term)
term = define "Term" $
  doc "Encode a term as a term (identity encoding)" $
  match _Term Nothing [
    encodedCase _Term _Term_annotated annotatedTerm,
    encodedCase _Term _Term_application application,
    ecase2 _Term_either $ encodedEither (
      primitive _eithers_either
        @@ Phantoms.lambda "l" (left $ term @@ var "l")
        @@ Phantoms.lambda "r" (right $ term @@ var "r")
        @@ var "v"),
    encodedCase _Term _Term_function function,
    encodedCase _Term _Term_let let_,
    ecase2 _Term_list $ encodedList $ primitive _lists_map @@ (term) @@ var "v",
    encodedCase _Term _Term_literal literal,
    ecase2 _Term_map $ encodedMap (primitive _maps_bimap @@ term @@ term @@ var "v"),
    ecase2 _Term_maybe $ encodedOptional (primitive _maybes_map @@ term @@ var "v"),
    ecase2 _Term_pair $ encodedPair (pair (term @@ (Pairs.first $ var "v")) (term @@ (Pairs.second $ var "v"))),
    encodedCase _Term _Term_record record,
    ecase2 _Term_set $ encodedSet $ primitive _sets_map @@ (term) @@ var "v",
    encodedCase _Term _Term_typeApplication typeApplicationTerm,
    encodedCase _Term _Term_typeLambda typeLambda,
    encodedCase _Term _Term_union injection,
    Phantoms.field _Term_unit $ constant $ encodedVariant _Term _Term_unit encodedUnit,
    encodedCase _Term _Term_variable name,
    encodedCase _Term _Term_wrap wrappedTerm]
  where
    ecase2 fname = Phantoms.field fname . Phantoms.lambda "v" . encodedVariant _Term fname

type_ :: TBinding (Type -> Term)
type_ = define "Type" $
  doc "Encode a type as a term (epsilon encoding)" $
  match _Type Nothing [
    Phantoms.field _Type_annotated $ "v" ~> inject _Term _Term_annotated $ Phantoms.record _AnnotatedTerm [
      Phantoms.field _AnnotatedTerm_body $ type_ @@ (Core.annotatedTypeBody $ var "v"),
      Phantoms.field _AnnotatedTerm_annotation $ Core.annotatedTypeAnnotation $ var "v"],
    csref _Type_application applicationType,
    csref _Type_either eitherType,
    csref _Type_function functionType,
    csref _Type_forall forallType,
    csref _Type_list type_,
    csref _Type_literal literalType,
    csref _Type_map mapType,
    csref _Type_maybe type_,
    csref _Type_pair pairType,
    csref _Type_record rowType,
    csref _Type_set type_,
    csref _Type_union rowType,
    Phantoms.field _Type_unit $ constant $ encodedVariant _Type _Type_unit Core.termUnit,
    csref _Type_variable name,
    csref _Type_wrap wrappedType]
  where
    cs fname term = Phantoms.field fname $ "v" ~> encodedVariant _Type fname term
    csref fname fun = cs fname (fun @@ var "v")

typeLambda :: TBinding (TypeLambda -> Term)
typeLambda = define "TypeLambda" $
  doc "Encode a type lambda as a term" $
  "l" ~> encodedRecord _TypeLambda [
    Phantoms.field _TypeLambda_parameter (name @@ (project _TypeLambda _TypeLambda_parameter @@ var "l")),
    Phantoms.field _TypeLambda_body (term @@ (project _TypeLambda _TypeLambda_body @@ var "l"))]

typeScheme :: TBinding (TypeScheme -> Term)
typeScheme = define "TypeScheme" $
  doc "Encode a type scheme as a term" $
  "ts" ~> encodedRecord _TypeScheme [
    Phantoms.field _TypeScheme_variables (encodedList (primitive _lists_map @@ name @@ (Core.typeSchemeVariables (var "ts")))),
    Phantoms.field _TypeScheme_type (type_ @@ (Core.typeSchemeType (var "ts")))]

typeApplicationTerm :: TBinding (TypeApplicationTerm -> Term)
typeApplicationTerm = define "TypeApplicationTerm" $
  doc "Encode a type application term as a term" $
  "tt" ~> encodedRecord _TypeApplicationTerm [
    Phantoms.field _TypeApplicationTerm_body (term @@ (project _TypeApplicationTerm _TypeApplicationTerm_body @@ var "tt")),
    Phantoms.field _TypeApplicationTerm_type (type_ @@ (project _TypeApplicationTerm _TypeApplicationTerm_type @@ var "tt"))]

wrappedTerm :: TBinding (WrappedTerm -> Term)
wrappedTerm = define "WrappedTerm" $
  doc "Encode a wrapped term as a term" $
  "n" ~> encodedRecord _WrappedTerm [
    Phantoms.field _WrappedTerm_typeName (name @@ (Core.wrappedTermTypeName (var "n"))),
    Phantoms.field _WrappedTerm_body (term @@ (Core.wrappedTermBody (var "n")))]

wrappedType :: TBinding (WrappedType -> Term)
wrappedType = define "WrappedType" $
  doc "Encode a wrapped type as a term" $
  "nt" ~> encodedRecord _WrappedType [
    Phantoms.field _WrappedType_typeName (name @@ (Core.wrappedTypeTypeName (var "nt"))),
    Phantoms.field _WrappedType_body (type_ @@ (Core.wrappedTypeBody (var "nt")))]

-- TODO: move these into another module

isEncodedType :: TBinding (Term -> Bool)
isEncodedType = coreEncodingExtrasDefinition "isEncodedType" $
  doc "Determines whether a given term is an encoded type" $
  "t" ~> cases _Term (Rewriting.deannotateTerm @@ var "t") (Just false) [
    _Term_application>>: "a" ~>
      isEncodedType @@ (Core.applicationFunction (var "a")),
    _Term_union>>: "i" ~>
      Equality.equal (string (unName _Type)) (Core.unName (Core.injectionTypeName (var "i")))]

isType :: TBinding (Type -> Bool)
isType = coreEncodingExtrasDefinition "isType" $
  doc "Check whether a type is a type (always true for non-encoded types)" $
  "t" ~> cases _Type (Rewriting.deannotateType @@ var "t") (Just false) [
    _Type_application>>: "a" ~>
      isType @@ (Core.applicationTypeFunction (var "a")),
    _Type_forall>>: "l" ~>
      isType @@ (Core.forallTypeBody (var "l")),
    _Type_union>>: "rt" ~>
      Equality.equal (string (unName _Type)) (Core.unName (Core.rowTypeTypeName (var "rt"))),
    _Type_variable>>: "v" ~> Equality.equal (var "v") (Core.nameLift _Type)]

isUnitTerm :: TBinding (Term -> Bool)
isUnitTerm = coreEncodingExtrasDefinition "isUnitTerm" $
  doc "Check whether a term is the unit term" $
  match _Term (Just false) [_Term_unit>>: constant true]

isUnitType :: TBinding (Type -> Bool)
isUnitType = coreEncodingExtrasDefinition "isUnitType" $
  doc "Check whether a type is the unit type" $
  match _Type (Just false) [_Type_unit>>: constant true]
