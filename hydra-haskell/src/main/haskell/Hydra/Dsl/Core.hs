module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Phantoms as Phantoms
import qualified Hydra.Dsl.Terms as Terms

-- For helpers
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Logic as Logic
import Hydra.Sources.Libraries

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


annotatedTerm :: TTerm Term -> TTerm (M.Map Name Term) -> TTerm AnnotatedTerm
annotatedTerm subject annotation = Phantoms.record _AnnotatedTerm [
  _AnnotatedTerm_subject>>: subject,
  _AnnotatedTerm_annotation>>: annotation]

annotatedTermSubject :: TTerm AnnotatedTerm -> TTerm Term
annotatedTermSubject at = Phantoms.project _AnnotatedTerm _AnnotatedTerm_subject @@ at

annotatedTermAnnotation :: TTerm AnnotatedTerm -> TTerm (M.Map Name Term)
annotatedTermAnnotation at = Phantoms.project _AnnotatedTerm _AnnotatedTerm_annotation @@ at

annotatedType :: TTerm Type -> TTerm (M.Map Name Term) -> TTerm AnnotatedType
annotatedType subject annotation = Phantoms.record _AnnotatedType [
  _AnnotatedType_subject>>: subject,
  _AnnotatedType_annotation>>: annotation]

annotatedTypeSubject :: TTerm AnnotatedType -> TTerm Type
annotatedTypeSubject at = Phantoms.project _AnnotatedType _AnnotatedType_subject @@ at

annotatedTypeAnnotation :: TTerm AnnotatedType -> TTerm (M.Map Name Term)
annotatedTypeAnnotation at = Phantoms.project _AnnotatedType _AnnotatedType_annotation @@ at

application :: TTerm Term -> TTerm Term -> TTerm Application
application function argument = Phantoms.record _Application [
  _Application_function>>: function,
  _Application_argument>>: argument]

applicationFunction :: TTerm Application -> TTerm Term
applicationFunction app = Phantoms.project _Application _Application_function @@ app

applicationArgument :: TTerm Application -> TTerm Term
applicationArgument app = Phantoms.project _Application _Application_argument @@ app

applicationType :: TTerm Type -> TTerm Type -> TTerm ApplicationType
applicationType function argument = Phantoms.record _ApplicationType [
  _ApplicationType_function>>: function,
  _ApplicationType_argument>>: argument]

applicationTypeFunction :: TTerm ApplicationType -> TTerm Type
applicationTypeFunction app = Phantoms.project _ApplicationType _ApplicationType_function @@ app

applicationTypeArgument :: TTerm ApplicationType -> TTerm Type
applicationTypeArgument app = Phantoms.project _ApplicationType _ApplicationType_argument @@ app

caseStatement :: TTerm Name -> TTerm (Maybe Term) -> TTerm [Field] -> TTerm CaseStatement
caseStatement typeName defaultTerm cases = Phantoms.record _CaseStatement [
  _CaseStatement_typeName>>: typeName,
  _CaseStatement_default>>: defaultTerm,
  _CaseStatement_cases>>: cases]

caseStatementTypeName :: TTerm CaseStatement -> TTerm Name
caseStatementTypeName cs = Phantoms.project _CaseStatement _CaseStatement_typeName @@ cs

caseStatementDefault :: TTerm CaseStatement -> TTerm (Maybe Term)
caseStatementDefault cs = Phantoms.project _CaseStatement _CaseStatement_default @@ cs

caseStatementCases :: TTerm CaseStatement -> TTerm [Field]
caseStatementCases cs = Phantoms.project _CaseStatement _CaseStatement_cases @@ cs

eliminationProduct :: TTerm TupleProjection -> TTerm Elimination
eliminationProduct = variant _Elimination _Elimination_product

eliminationRecord :: TTerm Projection -> TTerm Elimination
eliminationRecord = variant _Elimination _Elimination_record

eliminationUnion :: TTerm CaseStatement -> TTerm Elimination
eliminationUnion = variant _Elimination _Elimination_union

eliminationWrap :: TTerm Name -> TTerm Elimination
eliminationWrap = variant _Elimination _Elimination_wrap

field :: TTerm Name -> TTerm Term -> TTerm Field
field name term = Phantoms.record _Field [
  _Field_name>>: name,
  _Field_term>>: term]

fieldName :: TTerm Field -> TTerm Name
fieldName f = Phantoms.project _Field _Field_name @@ f

fieldTerm :: TTerm Field -> TTerm Term
fieldTerm f = Phantoms.project _Field _Field_term @@ f

fieldType :: TTerm Name -> TTerm Type -> TTerm FieldType
fieldType name typ = Phantoms.record _FieldType [
  _FieldType_name>>: name,
  _FieldType_type>>: typ]

fieldTypeName :: TTerm FieldType -> TTerm Name
fieldTypeName ft = Phantoms.project _FieldType _FieldType_name @@ ft

fieldTypeType :: TTerm FieldType -> TTerm Type
fieldTypeType ft = Phantoms.project _FieldType _FieldType_type @@ ft

floatTypeBigfloat :: TTerm FloatType
floatTypeBigfloat = unitVariant _FloatType _FloatType_bigfloat

floatTypeFloat32 :: TTerm FloatType
floatTypeFloat32 = unitVariant _FloatType _FloatType_float32

floatTypeFloat64 :: TTerm FloatType
floatTypeFloat64 = unitVariant _FloatType _FloatType_float64

floatValueBigfloat :: TTerm Double -> TTerm FloatValue
floatValueBigfloat = inject _FloatValue _FloatValue_bigfloat

floatValueFloat32 :: TTerm Float -> TTerm FloatValue
floatValueFloat32 = inject _FloatValue _FloatValue_float32

floatValueFloat64 :: TTerm Double -> TTerm FloatValue
floatValueFloat64 = inject _FloatValue _FloatValue_float64

forallType :: TTerm Name -> TTerm Type -> TTerm ForallType
forallType parameter body = Phantoms.record _ForallType [
  _ForallType_parameter>>: parameter,
  _ForallType_body>>: body]

forallTypeParameter :: TTerm ForallType -> TTerm Name
forallTypeParameter ft = Phantoms.project _ForallType _ForallType_parameter @@ ft

forallTypeBody :: TTerm ForallType -> TTerm Type
forallTypeBody ft = Phantoms.project _ForallType _ForallType_body @@ ft

functionElimination :: TTerm Elimination -> TTerm Function
functionElimination = variant _Function _Function_elimination

functionLambda :: TTerm Lambda -> TTerm Function
functionLambda = variant _Function _Function_lambda

functionPrimitive :: TTerm Name -> TTerm Function
functionPrimitive = variant _Function _Function_primitive

functionType :: TTerm Type -> TTerm Type -> TTerm FunctionType
functionType domain codomain = Phantoms.record _FunctionType [
  _FunctionType_domain>>: domain,
  _FunctionType_codomain>>: codomain]

functionTypeDomain :: TTerm FunctionType -> TTerm Type
functionTypeDomain ft = Phantoms.project _FunctionType _FunctionType_domain @@ ft

functionTypeCodomain :: TTerm FunctionType -> TTerm Type
functionTypeCodomain ft = Phantoms.project _FunctionType _FunctionType_codomain @@ ft

injection :: TTerm Name -> TTerm Field -> TTerm Injection
injection typeName field = Phantoms.record _Injection [
  _Injection_typeName>>: typeName,
  _Injection_field>>: field]

injectionTypeName :: TTerm Injection -> TTerm Name
injectionTypeName inj = Phantoms.project _Injection _Injection_typeName @@ inj

injectionField :: TTerm Injection -> TTerm Field
injectionField inj = Phantoms.project _Injection _Injection_field @@ inj

integerTypeBigint :: TTerm IntegerType
integerTypeBigint = unitVariant _IntegerType _IntegerType_bigint

integerTypeInt8 :: TTerm IntegerType
integerTypeInt8 = unitVariant _IntegerType _IntegerType_int8

integerTypeInt16 :: TTerm IntegerType
integerTypeInt16 = unitVariant _IntegerType _IntegerType_int16

integerTypeInt32 :: TTerm IntegerType
integerTypeInt32 = unitVariant _IntegerType _IntegerType_int32

integerTypeInt64 :: TTerm IntegerType
integerTypeInt64 = unitVariant _IntegerType _IntegerType_int64

integerTypeUint8 :: TTerm IntegerType
integerTypeUint8 = unitVariant _IntegerType _IntegerType_uint8

integerTypeUint16 :: TTerm IntegerType
integerTypeUint16 = unitVariant _IntegerType _IntegerType_uint16

integerTypeUint32 :: TTerm IntegerType
integerTypeUint32 = unitVariant _IntegerType _IntegerType_uint32

integerTypeUint64 :: TTerm IntegerType
integerTypeUint64 = unitVariant _IntegerType _IntegerType_uint64

integerValueBigint :: TTerm Integer -> TTerm IntegerValue
integerValueBigint = inject _IntegerValue _IntegerValue_bigint

integerValueInt8 :: TTerm Int8 -> TTerm IntegerValue
integerValueInt8 = inject _IntegerValue _IntegerValue_int8

integerValueInt16 :: TTerm Int16 -> TTerm IntegerValue
integerValueInt16 = inject _IntegerValue _IntegerValue_int16

integerValueInt32 :: TTerm Int -> TTerm IntegerValue
integerValueInt32 = inject _IntegerValue _IntegerValue_int32

integerValueInt64 :: TTerm Int64 -> TTerm IntegerValue
integerValueInt64 = inject _IntegerValue _IntegerValue_int64

integerValueUint8 :: TTerm Int16 -> TTerm IntegerValue
integerValueUint8 = inject _IntegerValue _IntegerValue_uint8

integerValueUint16 :: TTerm Int -> TTerm IntegerValue
integerValueUint16 = inject _IntegerValue _IntegerValue_uint16

integerValueUint32 :: TTerm Int64 -> TTerm IntegerValue
integerValueUint32 = inject _IntegerValue _IntegerValue_uint32

integerValueUint64 :: TTerm Integer -> TTerm IntegerValue
integerValueUint64 = inject _IntegerValue _IntegerValue_uint64

lambda :: TTerm Name -> TTerm (Maybe Type) -> TTerm Term -> TTerm Lambda
lambda parameter mdom body = Phantoms.record _Lambda [
  _Lambda_parameter>>: parameter,
  _Lambda_domain>>: mdom,
  _Lambda_body>>: body]

lambdaParameter :: TTerm Lambda -> TTerm Name
lambdaParameter l = Phantoms.project _Lambda _Lambda_parameter @@ l

lambdaBody :: TTerm Lambda -> TTerm Term
lambdaBody l = Phantoms.project _Lambda _Lambda_body @@ l

lambdaDomain :: TTerm Lambda -> TTerm (Maybe Type)
lambdaDomain l = Phantoms.project _Lambda _Lambda_domain @@ l

let_ :: TTerm [LetBinding] -> TTerm Term -> TTerm Let
let_ bindings environment = Phantoms.record _Let [
  _Let_bindings>>: bindings,
  _Let_environment>>: environment]

letBinding :: TTerm Name -> TTerm Term -> TTerm (Maybe TypeScheme) -> TTerm LetBinding
letBinding name term mtype = Phantoms.record _LetBinding [
  _LetBinding_name>>: name,
  _LetBinding_term>>: term,
  _LetBinding_type>>: mtype]

letBindings :: TTerm Let -> TTerm [LetBinding]
letBindings l = Phantoms.project _Let _Let_bindings @@ l

letBindingName :: TTerm LetBinding -> TTerm Name
letBindingName lb = Phantoms.project _LetBinding _LetBinding_name @@ lb

letBindingTerm :: TTerm LetBinding -> TTerm Term
letBindingTerm lb = Phantoms.project _LetBinding _LetBinding_term @@ lb

letBindingType :: TTerm LetBinding -> TTerm (Y.Maybe TypeScheme)
letBindingType lb = Phantoms.project _LetBinding _LetBinding_type @@ lb

letEnvironment :: TTerm Let -> TTerm Term
letEnvironment l = Phantoms.project _Let _Let_environment @@ l

literalBinary :: TTerm String -> TTerm Literal
literalBinary = variant _Literal _Literal_binary

literalBoolean :: TTerm Bool -> TTerm Literal
literalBoolean = variant _Literal _Literal_boolean

literalFloat :: TTerm FloatValue -> TTerm Literal
literalFloat = variant _Literal _Literal_float

literalInteger :: TTerm IntegerValue -> TTerm Literal
literalInteger = variant _Literal _Literal_integer

literalString :: TTerm String -> TTerm Literal
literalString = variant _Literal _Literal_string

literalTypeBinary :: TTerm LiteralType
literalTypeBinary = unitVariant _LiteralType _LiteralType_binary

literalTypeBoolean :: TTerm LiteralType
literalTypeBoolean = unitVariant _LiteralType _LiteralType_boolean

literalTypeFloat :: TTerm FloatType -> TTerm LiteralType
literalTypeFloat = variant _LiteralType _LiteralType_float

literalTypeInteger :: TTerm IntegerType -> TTerm LiteralType
literalTypeInteger = variant _LiteralType _LiteralType_integer

literalTypeString :: TTerm LiteralType
literalTypeString = unitVariant _LiteralType _LiteralType_string

mapType :: TTerm Type -> TTerm Type -> TTerm MapType
mapType keys values = Phantoms.record _MapType [
  _MapType_keys>>: keys,
  _MapType_values>>: values]

mapTypeKeys :: TTerm MapType -> TTerm Type
mapTypeKeys mt = Phantoms.project _MapType _MapType_keys @@ mt

mapTypeValues :: TTerm MapType -> TTerm Type
mapTypeValues mt = Phantoms.project _MapType _MapType_values @@ mt

name :: TTerm String -> TTerm Name
name = wrap _Name

nameLift :: Name -> TTerm Name
nameLift (Name n) = wrap _Name $ Phantoms.string n

projection :: TTerm Name -> TTerm Name -> TTerm Projection
projection tname fname = Phantoms.record _Projection [
  _Projection_typeName>>: tname,
  _Projection_field>>: fname]

projectionTypeName :: TTerm Projection -> TTerm Name
projectionTypeName p = Phantoms.project _Projection _Projection_typeName @@ p

projectionField :: TTerm Projection -> TTerm Name
projectionField p = Phantoms.project _Projection _Projection_field @@ p

record :: TTerm Name -> TTerm [Field] -> TTerm Record
record typeName fields = Phantoms.record _Record [
  _Record_typeName>>: typeName,
  _Record_fields>>: fields]

recordTypeName :: TTerm Record -> TTerm Name
recordTypeName r = Phantoms.project _Record _Record_typeName @@ r

recordFields :: TTerm Record -> TTerm [Field]
recordFields r = Phantoms.project _Record _Record_fields @@ r

rowType :: TTerm Name -> TTerm [FieldType] -> TTerm (RowType)
rowType typeName fields = Phantoms.record _RowType [
  _RowType_typeName>>: typeName,
  _RowType_fields>>: fields]

rowTypeTypeName :: TTerm RowType -> TTerm Name
rowTypeTypeName rt = Phantoms.project _RowType _RowType_typeName @@ rt

rowTypeFields :: TTerm RowType -> TTerm [FieldType]
rowTypeFields rt = Phantoms.project _RowType _RowType_fields @@ rt

sum :: TTerm Int -> TTerm Int -> TTerm Term -> TTerm Sum
sum index size term = Phantoms.record _Sum [
  _Sum_index>>: index,
  _Sum_size>>: size,
  _Sum_term>>: term]

sumIndex :: TTerm Sum -> TTerm Int
sumIndex s = Phantoms.project _Sum _Sum_index @@ s

sumSize :: TTerm Sum -> TTerm Int
sumSize s = Phantoms.project _Sum _Sum_size @@ s

sumTerm :: TTerm Sum -> TTerm Term
sumTerm s = Phantoms.project _Sum _Sum_term @@ s

termAnnotated :: TTerm AnnotatedTerm -> TTerm Term
termAnnotated = variant _Term _Term_annotated

termApplication :: TTerm Application -> TTerm Term
termApplication = variant _Term _Term_application

termFunction :: TTerm Function -> TTerm Term
termFunction = variant _Term _Term_function

termLet :: TTerm Let -> TTerm Term
termLet = variant _Term _Term_let

termList :: TTerm [Term] -> TTerm Term
termList = variant _Term _Term_list

termLiteral :: TTerm Literal -> TTerm Term
termLiteral = variant _Term _Term_literal

termMap :: TTerm (M.Map Term Term) -> TTerm Term
termMap = variant _Term _Term_map

termOptional :: TTerm (Maybe Term) -> TTerm Term
termOptional = variant _Term _Term_optional

termProduct :: TTerm [Term] -> TTerm Term
termProduct = variant _Term _Term_product

termRecord :: TTerm Record -> TTerm Term
termRecord = variant _Term _Term_record

termSet :: TTerm (S.Set Term) -> TTerm Term
termSet = variant _Term _Term_set

termSum :: TTerm Sum -> TTerm Term
termSum = variant _Term _Term_sum

termTypeAbstraction :: TTerm TypeAbstraction -> TTerm Term
termTypeAbstraction = variant _Term _Term_typeAbstraction

termTypeApplication :: TTerm TypedTerm -> TTerm Term
termTypeApplication = variant _Term _Term_typeApplication

termUnion :: TTerm Injection -> TTerm Term
termUnion = variant _Term _Term_union

termVariable :: TTerm Name -> TTerm Term
termVariable = variant _Term _Term_variable

termWrap :: TTerm WrappedTerm -> TTerm Term
termWrap = variant _Term _Term_wrap

tupleProjection :: TTerm Int -> TTerm Int -> TTerm (Maybe [Type]) -> TTerm TupleProjection
tupleProjection arity idx mdom = Phantoms.record _TupleProjection [
  _TupleProjection_arity>>: arity,
  _TupleProjection_index>>: idx,
  _TupleProjection_domain>>: mdom]

tupleProjectionArity :: TTerm TupleProjection -> TTerm Int
tupleProjectionArity tp = Phantoms.project _TupleProjection _TupleProjection_arity @@ tp

tupleProjectionIndex :: TTerm TupleProjection -> TTerm Int
tupleProjectionIndex tp = Phantoms.project _TupleProjection _TupleProjection_index @@ tp

tupleProjectionDomain :: TTerm TupleProjection -> TTerm (Maybe [Type])
tupleProjectionDomain tp = Phantoms.project _TupleProjection _TupleProjection_domain @@ tp

typeAbstraction :: TTerm Name -> TTerm Term -> TTerm TypeAbstraction
typeAbstraction parameter body = Phantoms.record _TypeAbstraction [
  _TypeAbstraction_parameter>>: parameter,
  _TypeAbstraction_body>>: body]

typeAbstractionParameter :: TTerm TypeAbstraction -> TTerm Name
typeAbstractionParameter ta = Phantoms.project _TypeAbstraction _TypeAbstraction_parameter @@ ta

typeAbstractionBody :: TTerm TypeAbstraction -> TTerm Term
typeAbstractionBody ta = Phantoms.project _TypeAbstraction _TypeAbstraction_body @@ ta

typeAnnotated :: TTerm AnnotatedType -> TTerm Type
typeAnnotated = variant _Type _Type_annotated

typeApplication :: TTerm ApplicationType -> TTerm Type
typeApplication = variant _Type _Type_application

typeForall :: TTerm ForallType -> TTerm Type
typeForall = variant _Type _Type_forall

typeFunction :: TTerm FunctionType -> TTerm Type
typeFunction = variant _Type _Type_function

typeLambda :: TTerm ForallType -> TTerm Type
typeLambda = variant _Type _Type_forall

typeList :: TTerm Type -> TTerm Type
typeList = variant _Type _Type_list

typeLiteral :: TTerm LiteralType -> TTerm Type
typeLiteral = variant _Type _Type_literal

typeMap :: TTerm MapType -> TTerm Type
typeMap = variant _Type _Type_map

typeOptional :: TTerm Type -> TTerm Type
typeOptional = variant _Type _Type_optional

typeProduct :: TTerm [Type] -> TTerm Type
typeProduct = variant _Type _Type_product

typeRecord :: TTerm RowType -> TTerm Type
typeRecord = variant _Type _Type_record

typeScheme :: TTerm [Name] -> TTerm Type -> TTerm TypeScheme
typeScheme variables body = Phantoms.record _TypeScheme [
  _TypeScheme_variables>>: variables,
  _TypeScheme_type>>: body]

typeSchemeVariables :: TTerm TypeScheme -> TTerm [Name]
typeSchemeVariables ts = Phantoms.project _TypeScheme _TypeScheme_variables @@ ts

typeSchemeType :: TTerm TypeScheme -> TTerm Type
typeSchemeType ts = Phantoms.project _TypeScheme _TypeScheme_type @@ ts

typeSet :: TTerm Type -> TTerm Type
typeSet = variant _Type _Type_set

typeSum :: TTerm [Type] -> TTerm Type
typeSum = variant _Type _Type_sum

typeUnion :: TTerm RowType -> TTerm Type
typeUnion = variant _Type _Type_union

typeVariable :: TTerm Name -> TTerm Type
typeVariable = variant _Type _Type_variable

typeWrap :: TTerm WrappedType -> TTerm Type
typeWrap = variant _Type _Type_wrap

typedTerm :: TTerm Term -> TTerm Type -> TTerm TypedTerm
typedTerm term type_ = Phantoms.record _TypedTerm [
  _TypedTerm_term>>: term,
  _TypedTerm_type>>: type_]

typedTermTerm :: TTerm TypedTerm -> TTerm Term
typedTermTerm tt = Phantoms.project _TypedTerm _TypedTerm_term @@ tt

typedTermType :: TTerm TypedTerm -> TTerm Type
typedTermType tt = Phantoms.project _TypedTerm _TypedTerm_type @@ tt

unName :: TTerm Name -> TTerm String
unName n = unwrap _Name @@ n

unNamespace :: TTerm Namespace -> TTerm String
unNamespace ns = unwrap _Namespace @@ ns

wrappedTerm :: TTerm Name -> TTerm Term -> TTerm WrappedTerm
wrappedTerm typeName object = Phantoms.record _WrappedTerm [
  _WrappedTerm_typeName>>: typeName,
  _WrappedTerm_object>>: object]

wrappedTermTypeName :: TTerm WrappedTerm -> TTerm Name
wrappedTermTypeName wt = Phantoms.project _WrappedTerm _WrappedTerm_typeName @@ wt

wrappedTermObject :: TTerm WrappedTerm -> TTerm Term
wrappedTermObject wt = Phantoms.project _WrappedTerm _WrappedTerm_object @@ wt

wrappedType :: TTerm Name -> TTerm Type -> TTerm WrappedType
wrappedType typeName object = Phantoms.record _WrappedType [
  _WrappedType_typeName>>: typeName,
  _WrappedType_object>>: object]

wrappedTypeTypeName :: TTerm WrappedType -> TTerm Name
wrappedTypeTypeName wt = Phantoms.project _WrappedType _WrappedType_typeName @@ wt

wrappedTypeObject :: TTerm WrappedType -> TTerm Type
wrappedTypeObject wt = Phantoms.project _WrappedType _WrappedType_object @@ wt

----------------------------------------
-- Non-schema helpers

equalName :: TTerm (Name -> Name -> Bool)
equalName = lambdas ["left", "right"] $ primitive _equality_equalString
  @@ (Hydra.Dsl.Core.unName $ var "left")
  @@ (Hydra.Dsl.Core.unName $ var "right")

equalName_ :: TTerm Name -> TTerm Name -> TTerm Bool
equalName_ left right = Equality.equalString (Hydra.Dsl.Core.unName left) (Hydra.Dsl.Core.unName right)

equalNameList :: TTerm ([Name] -> [Name] -> Bool)
equalNameList = lambdas ["lefts", "rights"] $ Logic.and
  (Equality.equalInt32 (Lists.length (var "lefts")) (Lists.length (var "rights")))
  (Logic.ands $ Lists.zipWith equalName (var "lefts") (var "rights"))

equalNameList_ :: TTerm [Name] -> TTerm [Name] -> TTerm Bool
equalNameList_ lefts rights = Logic.and
  (Equality.equalInt32 (Lists.length lefts) (Lists.length rights))
  (Logic.ands $ Lists.zipWith equalName lefts rights)

fieldWithTerm :: TTerm Term -> TTerm Field -> TTerm Field
fieldWithTerm t ft = Hydra.Dsl.Core.field (Hydra.Dsl.Core.fieldName ft) t

fieldTypeWithType :: TTerm FieldType -> TTerm Type -> TTerm FieldType
fieldTypeWithType ft t = Hydra.Dsl.Core.fieldType (Hydra.Dsl.Core.fieldTypeName ft) t

false :: TTerm Term
false = termLiteral $ literalBoolean $ Phantoms.false

int32 :: Int -> TTerm Term
int32 = termLiteral . literalInteger . integerValueInt32 . Phantoms.int32

string :: String -> TTerm Term
string = termLiteral . literalString . Phantoms.string
