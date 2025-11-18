module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Meta.Phantoms as Phantoms
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
annotatedTerm body annotation = Phantoms.record _AnnotatedTerm [
  _AnnotatedTerm_body>>: body,
  _AnnotatedTerm_annotation>>: annotation]

annotatedTermBody :: TTerm AnnotatedTerm -> TTerm Term
annotatedTermBody at = Phantoms.project _AnnotatedTerm _AnnotatedTerm_body @@ at

annotatedTermAnnotation :: TTerm AnnotatedTerm -> TTerm (M.Map Name Term)
annotatedTermAnnotation at = Phantoms.project _AnnotatedTerm _AnnotatedTerm_annotation @@ at

annotatedTermWithBody :: TTerm AnnotatedTerm -> TTerm Term -> TTerm AnnotatedTerm
annotatedTermWithBody at body = annotatedTerm body (Hydra.Dsl.Core.annotatedTermAnnotation at)

annotatedType :: TTerm Type -> TTerm (M.Map Name Term) -> TTerm AnnotatedType
annotatedType body annotation = Phantoms.record _AnnotatedType [
  _AnnotatedType_body>>: body,
  _AnnotatedType_annotation>>: annotation]

annotatedTypeBody :: TTerm AnnotatedType -> TTerm Type
annotatedTypeBody at = Phantoms.project _AnnotatedType _AnnotatedType_body @@ at

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

eitherType :: TTerm Type -> TTerm Type -> TTerm EitherType
eitherType left right = Phantoms.record _EitherType [
  _EitherType_left>>: left,
  _EitherType_right>>: right]

eitherTypeLeft :: TTerm EitherType -> TTerm Type
eitherTypeLeft et = Phantoms.project _EitherType _EitherType_left @@ et

eitherTypeRight :: TTerm EitherType -> TTerm Type
eitherTypeRight et = Phantoms.project _EitherType _EitherType_right @@ et

pairType :: TTerm Type -> TTerm Type -> TTerm PairType
pairType first second = Phantoms.record _PairType [
  _PairType_first>>: first,
  _PairType_second>>: second]

pairTypeFirst :: TTerm PairType -> TTerm Type
pairTypeFirst pt = Phantoms.project _PairType _PairType_first @@ pt

pairTypeSecond :: TTerm PairType -> TTerm Type
pairTypeSecond pt = Phantoms.project _PairType _PairType_second @@ pt

binding :: TTerm Name -> TTerm Term -> TTerm (Maybe TypeScheme) -> TTerm Binding
binding name term mtype = Phantoms.record _Binding [
  _Binding_name>>: name,
  _Binding_term>>: term,
  _Binding_type>>: mtype]

bindingName :: TTerm Binding -> TTerm Name
bindingName lb = Phantoms.project _Binding _Binding_name @@ lb

bindingTerm :: TTerm Binding -> TTerm Term
bindingTerm lb = Phantoms.project _Binding _Binding_term @@ lb

bindingType :: TTerm Binding -> TTerm (Y.Maybe TypeScheme)
bindingType lb = Phantoms.project _Binding _Binding_type @@ lb

bindingWithTerm :: TTerm Binding -> TTerm Term -> TTerm Binding
bindingWithTerm b term = binding (Hydra.Dsl.Core.bindingName b) term (Hydra.Dsl.Core.bindingType b)

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
eliminationProduct = inject _Elimination _Elimination_product

eliminationRecord :: TTerm Projection -> TTerm Elimination
eliminationRecord = inject _Elimination _Elimination_record

eliminationUnion :: TTerm CaseStatement -> TTerm Elimination
eliminationUnion = inject _Elimination _Elimination_union

eliminationWrap :: TTerm Name -> TTerm Elimination
eliminationWrap = inject _Elimination _Elimination_wrap

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
floatTypeBigfloat = injectUnit _FloatType _FloatType_bigfloat

floatTypeFloat32 :: TTerm FloatType
floatTypeFloat32 = injectUnit _FloatType _FloatType_float32

floatTypeFloat64 :: TTerm FloatType
floatTypeFloat64 = injectUnit _FloatType _FloatType_float64

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
functionElimination = inject _Function _Function_elimination

functionLambda :: TTerm Lambda -> TTerm Function
functionLambda = inject _Function _Function_lambda

functionPrimitive :: TTerm Name -> TTerm Function
functionPrimitive = inject _Function _Function_primitive

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
integerTypeBigint = injectUnit _IntegerType _IntegerType_bigint

integerTypeInt8 :: TTerm IntegerType
integerTypeInt8 = injectUnit _IntegerType _IntegerType_int8

integerTypeInt16 :: TTerm IntegerType
integerTypeInt16 = injectUnit _IntegerType _IntegerType_int16

integerTypeInt32 :: TTerm IntegerType
integerTypeInt32 = injectUnit _IntegerType _IntegerType_int32

integerTypeInt64 :: TTerm IntegerType
integerTypeInt64 = injectUnit _IntegerType _IntegerType_int64

integerTypeUint8 :: TTerm IntegerType
integerTypeUint8 = injectUnit _IntegerType _IntegerType_uint8

integerTypeUint16 :: TTerm IntegerType
integerTypeUint16 = injectUnit _IntegerType _IntegerType_uint16

integerTypeUint32 :: TTerm IntegerType
integerTypeUint32 = injectUnit _IntegerType _IntegerType_uint32

integerTypeUint64 :: TTerm IntegerType
integerTypeUint64 = injectUnit _IntegerType _IntegerType_uint64

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

lambdaWithBody :: TTerm Lambda -> TTerm Term -> TTerm Lambda
lambdaWithBody l body = Hydra.Dsl.Core.lambda (Hydra.Dsl.Core.lambdaParameter l) (Hydra.Dsl.Core.lambdaDomain l) body

let_ :: TTerm [Binding] -> TTerm Term -> TTerm Let
let_ bindings body = Phantoms.record _Let [
  _Let_bindings>>: bindings,
  _Let_body>>: body]

letBindings :: TTerm Let -> TTerm [Binding]
letBindings l = Phantoms.project _Let _Let_bindings @@ l

letBody :: TTerm Let -> TTerm Term
letBody l = Phantoms.project _Let _Let_body @@ l

letWithBody :: TTerm Let -> TTerm Term -> TTerm Let
letWithBody l body = let_ (Hydra.Dsl.Core.letBindings l) body

literalBinary :: TTerm String -> TTerm Literal
literalBinary = inject _Literal _Literal_binary

literalBoolean :: TTerm Bool -> TTerm Literal
literalBoolean = inject _Literal _Literal_boolean

literalFloat :: TTerm FloatValue -> TTerm Literal
literalFloat = inject _Literal _Literal_float

literalInteger :: TTerm IntegerValue -> TTerm Literal
literalInteger = inject _Literal _Literal_integer

literalString :: TTerm String -> TTerm Literal
literalString = inject _Literal _Literal_string

literalTypeBinary :: TTerm LiteralType
literalTypeBinary = injectUnit _LiteralType _LiteralType_binary

literalTypeBoolean :: TTerm LiteralType
literalTypeBoolean = injectUnit _LiteralType _LiteralType_boolean

literalTypeFloat :: TTerm FloatType -> TTerm LiteralType
literalTypeFloat = inject _LiteralType _LiteralType_float

literalTypeInteger :: TTerm IntegerType -> TTerm LiteralType
literalTypeInteger = inject _LiteralType _LiteralType_integer

literalTypeString :: TTerm LiteralType
literalTypeString = injectUnit _LiteralType _LiteralType_string

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
termAnnotated = inject _Term _Term_annotated

termApplication :: TTerm Application -> TTerm Term
termApplication = inject _Term _Term_application

termEither :: TTerm (Either Term Term) -> TTerm Term
termEither = inject _Term _Term_either

termFunction :: TTerm Function -> TTerm Term
termFunction = inject _Term _Term_function

termLet :: TTerm Let -> TTerm Term
termLet = inject _Term _Term_let

termList :: TTerm [Term] -> TTerm Term
termList = inject _Term _Term_list

termLiteral :: TTerm Literal -> TTerm Term
termLiteral = inject _Term _Term_literal

termMap :: TTerm (M.Map Term Term) -> TTerm Term
termMap = inject _Term _Term_map

termMaybe :: TTerm (Maybe Term) -> TTerm Term
termMaybe = inject _Term _Term_maybe

termPair :: TTerm (Term, Term) -> TTerm Term
termPair = inject _Term _Term_pair

termProduct :: TTerm [Term] -> TTerm Term
termProduct = inject _Term _Term_product

termRecord :: TTerm Record -> TTerm Term
termRecord = inject _Term _Term_record

termSet :: TTerm (S.Set Term) -> TTerm Term
termSet = inject _Term _Term_set

termSum :: TTerm Sum -> TTerm Term
termSum = inject _Term _Term_sum

termTypeLambda :: TTerm TypeLambda -> TTerm Term
termTypeLambda = inject _Term _Term_typeLambda

termTypeApplication :: TTerm TypeApplicationTerm -> TTerm Term
termTypeApplication = inject _Term _Term_typeApplication

termUnion :: TTerm Injection -> TTerm Term
termUnion = inject _Term _Term_union

termUnit :: TTerm Term
termUnit = injectUnit _Term _Term_unit

termVariable :: TTerm Name -> TTerm Term
termVariable = inject _Term _Term_variable

termWrap :: TTerm WrappedTerm -> TTerm Term
termWrap = inject _Term _Term_wrap

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

typeLambda :: TTerm Name -> TTerm Term -> TTerm TypeLambda
typeLambda parameter body = Phantoms.record _TypeLambda [
  _TypeLambda_parameter>>: parameter,
  _TypeLambda_body>>: body]

typeLambdaParameter :: TTerm TypeLambda -> TTerm Name
typeLambdaParameter ta = Phantoms.project _TypeLambda _TypeLambda_parameter @@ ta

typeLambdaBody :: TTerm TypeLambda -> TTerm Term
typeLambdaBody ta = Phantoms.project _TypeLambda _TypeLambda_body @@ ta

typeAnnotated :: TTerm AnnotatedType -> TTerm Type
typeAnnotated = inject _Type _Type_annotated

typeApplication :: TTerm ApplicationType -> TTerm Type
typeApplication = inject _Type _Type_application

typeEither :: TTerm EitherType -> TTerm Type
typeEither = inject _Type _Type_either

typeForall :: TTerm ForallType -> TTerm Type
typeForall = inject _Type _Type_forall

typeFunction :: TTerm FunctionType -> TTerm Type
typeFunction = inject _Type _Type_function

typeList :: TTerm Type -> TTerm Type
typeList = inject _Type _Type_list

typeLiteral :: TTerm LiteralType -> TTerm Type
typeLiteral = inject _Type _Type_literal

typeMap :: TTerm MapType -> TTerm Type
typeMap = inject _Type _Type_map

typeMaybe :: TTerm Type -> TTerm Type
typeMaybe = inject _Type _Type_maybe

typePair :: TTerm PairType -> TTerm Type
typePair = inject _Type _Type_pair

typeProduct :: TTerm [Type] -> TTerm Type
typeProduct = inject _Type _Type_product

typeRecord :: TTerm RowType -> TTerm Type
typeRecord = inject _Type _Type_record

typeScheme :: TTerm [Name] -> TTerm Type -> TTerm TypeScheme
typeScheme variables body = Phantoms.record _TypeScheme [
  _TypeScheme_variables>>: variables,
  _TypeScheme_type>>: body]

typeSchemeVariables :: TTerm TypeScheme -> TTerm [Name]
typeSchemeVariables ts = Phantoms.project _TypeScheme _TypeScheme_variables @@ ts

typeSchemeType :: TTerm TypeScheme -> TTerm Type
typeSchemeType ts = Phantoms.project _TypeScheme _TypeScheme_type @@ ts

typeSet :: TTerm Type -> TTerm Type
typeSet = inject _Type _Type_set

typeSum :: TTerm [Type] -> TTerm Type
typeSum = inject _Type _Type_sum

typeUnion :: TTerm RowType -> TTerm Type
typeUnion = inject _Type _Type_union

typeUnit :: TTerm Type
typeUnit = injectUnit _Type _Type_unit

typeVariable :: TTerm Name -> TTerm Type
typeVariable = inject _Type _Type_variable

typeWrap :: TTerm WrappedType -> TTerm Type
typeWrap = inject _Type _Type_wrap

typeApplicationTerm :: TTerm Term -> TTerm Type -> TTerm TypeApplicationTerm
typeApplicationTerm body type_ = Phantoms.record _TypeApplicationTerm [
  _TypeApplicationTerm_body>>: body,
  _TypeApplicationTerm_type>>: type_]

typeApplicationTermBody :: TTerm TypeApplicationTerm -> TTerm Term
typeApplicationTermBody tt = Phantoms.project _TypeApplicationTerm _TypeApplicationTerm_body @@ tt

typeApplicationTermType :: TTerm TypeApplicationTerm -> TTerm Type
typeApplicationTermType tt = Phantoms.project _TypeApplicationTerm _TypeApplicationTerm_type @@ tt

unName :: TTerm Name -> TTerm String
unName n = unwrap _Name @@ n

unNamespace :: TTerm Namespace -> TTerm String
unNamespace ns = unwrap _Namespace @@ ns

wrappedTerm :: TTerm Name -> TTerm Term -> TTerm WrappedTerm
wrappedTerm typeName object = Phantoms.record _WrappedTerm [
  _WrappedTerm_typeName>>: typeName,
  _WrappedTerm_body>>: object]

wrappedTermTypeName :: TTerm WrappedTerm -> TTerm Name
wrappedTermTypeName wt = Phantoms.project _WrappedTerm _WrappedTerm_typeName @@ wt

wrappedTermBody :: TTerm WrappedTerm -> TTerm Term
wrappedTermBody wt = Phantoms.project _WrappedTerm _WrappedTerm_body @@ wt

wrappedType :: TTerm Name -> TTerm Type -> TTerm WrappedType
wrappedType typeName object = Phantoms.record _WrappedType [
  _WrappedType_typeName>>: typeName,
  _WrappedType_body>>: object]

wrappedTypeTypeName :: TTerm WrappedType -> TTerm Name
wrappedTypeTypeName wt = Phantoms.project _WrappedType _WrappedType_typeName @@ wt

wrappedTypeBody :: TTerm WrappedType -> TTerm Type
wrappedTypeBody wt = Phantoms.project _WrappedType _WrappedType_body @@ wt

----------------------------------------
-- Non-schema helpers

equalName :: TTerm (Name -> Name -> Bool)
equalName = lambdas ["left", "right"] $ primitive _equality_equal
  @@ (Hydra.Dsl.Core.unName $ var "left")
  @@ (Hydra.Dsl.Core.unName $ var "right")

equalName_ :: TTerm Name -> TTerm Name -> TTerm Bool
equalName_ left right = Equality.equal (Hydra.Dsl.Core.unName left) (Hydra.Dsl.Core.unName right)

equalNameList :: TTerm ([Name] -> [Name] -> Bool)
equalNameList = lambdas ["lefts", "rights"] $ Logic.and
  (Equality.equal (Lists.length (var "lefts")) (Lists.length (var "rights")))
  (Logic.ands $ Lists.zipWith equalName (var "lefts") (var "rights"))

equalNameList_ :: TTerm [Name] -> TTerm [Name] -> TTerm Bool
equalNameList_ lefts rights = Logic.and
  (Equality.equal (Lists.length lefts) (Lists.length rights))
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
