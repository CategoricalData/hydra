module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Terms as Terms

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


annotatedTerm :: TTerm Term -> TTerm (M.Map String Term) -> TTerm AnnotatedTerm
annotatedTerm subject annotation = Base.record _AnnotatedTerm [
  _AnnotatedTerm_subject>>: subject,
  _AnnotatedTerm_annotation>>: annotation]

annotatedTermSubject :: TTerm (AnnotatedTerm -> Term)
annotatedTermSubject = Base.project _AnnotatedTerm _AnnotatedTerm_subject

annotatedTermAnnotation :: TTerm (AnnotatedTerm -> M.Map String Term)
annotatedTermAnnotation = Base.project _AnnotatedTerm _AnnotatedTerm_annotation

annotatedType :: TTerm Type -> TTerm (M.Map String Term) -> TTerm AnnotatedType
annotatedType subject annotation = Base.record _AnnotatedType [
  _AnnotatedType_subject>>: subject,
  _AnnotatedType_annotation>>: annotation]

annotatedTypeSubject :: TTerm (AnnotatedType -> Type)
annotatedTypeSubject = Base.project _AnnotatedType _AnnotatedType_subject

annotatedTypeAnnotation :: TTerm (AnnotatedType -> M.Map String Term)
annotatedTypeAnnotation = Base.project _AnnotatedType _AnnotatedType_annotation

application :: TTerm Term -> TTerm Term -> TTerm Application
application function argument = Base.record _Application [
  _Application_function>>: function,
  _Application_argument>>: argument]

applicationFunction :: TTerm (Application -> Term)
applicationFunction = Base.project _Application _Application_function

applicationArgument :: TTerm (Application -> Term)
applicationArgument = Base.project _Application _Application_argument

applicationType :: TTerm Type -> TTerm Type -> TTerm ApplicationType
applicationType function argument = Base.record _ApplicationType [
  _ApplicationType_function>>: function,
  _ApplicationType_argument>>: argument]

applicationTypeFunction :: TTerm (ApplicationType -> Type)
applicationTypeFunction = Base.project _ApplicationType _ApplicationType_function

applicationTypeArgument :: TTerm (ApplicationType -> Type)
applicationTypeArgument = Base.project _ApplicationType _ApplicationType_argument

caseStatement :: TTerm Name -> TTerm (Maybe Term) -> TTerm [Field] -> TTerm CaseStatement
caseStatement typeName defaultTerm cases = Base.record _CaseStatement [
  _CaseStatement_typeName>>: typeName,
  _CaseStatement_default>>: defaultTerm,
  _CaseStatement_cases>>: cases]

caseStatementTypeName :: TTerm (CaseStatement -> Name)
caseStatementTypeName = Base.project _CaseStatement _CaseStatement_typeName

caseStatementDefault :: TTerm (CaseStatement -> Maybe Term)
caseStatementDefault = Base.project _CaseStatement _CaseStatement_default

caseStatementCases :: TTerm (CaseStatement -> [Field])
caseStatementCases = Base.project _CaseStatement _CaseStatement_cases

eliminationList :: TTerm Term -> TTerm Elimination
eliminationList = variant _Elimination _Elimination_list

eliminationRecord :: TTerm Projection -> TTerm Elimination
eliminationRecord = variant _Elimination _Elimination_record

eliminationUnion :: TTerm CaseStatement -> TTerm Elimination
eliminationUnion = variant _Elimination _Elimination_union

eliminationWrap :: TTerm Name -> TTerm Elimination
eliminationWrap = variant _Elimination _Elimination_wrap

field :: TTerm Name -> TTerm Term -> TTerm Field
field name term = Base.record _Field [
  _Field_name>>: name,
  _Field_term>>: term]

fieldName :: TTerm (Field -> Name)
fieldName = Base.project _Field _Field_name

fieldTerm :: TTerm (Field -> Term)
fieldTerm = Base.project _Field _Field_term

fieldType :: TTerm Name -> TTerm Type -> TTerm FieldType
fieldType name typ = Base.record _FieldType [
  _FieldType_name>>: name,
  _FieldType_type>>: typ]

fieldTypeName :: TTerm (FieldType -> Name)
fieldTypeName = Base.project _FieldType _FieldType_name

fieldTypeType :: TTerm (FieldType -> Type)
fieldTypeType = Base.project _FieldType _FieldType_type

floatValueFloat32 :: TTerm Float -> TTerm FloatValue
floatValueFloat32 = inject _FloatValue _FloatValue_float32

floatValueFloat64 :: TTerm Float -> TTerm FloatValue
floatValueFloat64 = inject _FloatValue _FloatValue_float64

functionElimination :: TTerm Elimination -> TTerm Function
functionElimination = variant _Function _Function_elimination

functionLambda :: TTerm Lambda -> TTerm Function
functionLambda = variant _Function _Function_lambda

functionPrimitive :: TTerm Name -> TTerm Function
functionPrimitive = variant _Function _Function_primitive

functionType :: TTerm Type -> TTerm Type -> TTerm FunctionType
functionType domain codomain = Base.record _FunctionType [
  _FunctionType_domain>>: domain,
  _FunctionType_codomain>>: codomain]

functionTypeDomain :: TTerm (FunctionType -> Type)
functionTypeDomain = Base.project _FunctionType _FunctionType_domain

functionTypeCodomain :: TTerm (FunctionType -> Type)
functionTypeCodomain = Base.project _FunctionType _FunctionType_codomain

injection :: TTerm Name -> TTerm Field -> TTerm Injection
injection typeName field = Base.record _Injection [
  _Injection_typeName>>: typeName,
  _Injection_field>>: field]

injectionTypeName :: TTerm (Injection -> Name)
injectionTypeName = Base.project _Injection _Injection_typeName

injectionField :: TTerm (Injection -> Field)
injectionField = Base.project _Injection _Injection_field

integerTypeInt16 :: TTerm IntegerType
integerTypeInt16 = unitVariant _IntegerType _IntegerType_int16

integerTypeInt32 :: TTerm IntegerType
integerTypeInt32 = unitVariant _IntegerType _IntegerType_int32

integerTypeInt64 :: TTerm IntegerType
integerTypeInt64 = unitVariant _IntegerType _IntegerType_int64

integerTypeUint64 :: TTerm IntegerType
integerTypeUint64 = unitVariant _IntegerType _IntegerType_uint64

integerValueInt16 :: TTerm Int16 -> TTerm IntegerValue
integerValueInt16 = inject _IntegerValue _IntegerValue_int16

integerValueInt32 :: TTerm Int -> TTerm IntegerValue
integerValueInt32 = inject _IntegerValue _IntegerValue_int32

integerValueInt64 :: TTerm Int -> TTerm IntegerValue
integerValueInt64 = inject _IntegerValue _IntegerValue_int64

integerValueUint64 :: TTerm Integer -> TTerm IntegerValue
integerValueUint64 = inject _IntegerValue _IntegerValue_uint64

lambda :: TTerm Name -> TTerm (Maybe Type) -> TTerm Term -> TTerm Lambda
lambda parameter mdom body = Base.record _Lambda [
  _Lambda_parameter>>: parameter,
  _Lambda_domain>>: mdom,
  _Lambda_body>>: body]

lambdaParameter :: TTerm (Lambda -> Name)
lambdaParameter = Base.project _Lambda _Lambda_parameter

lambdaBody :: TTerm (Lambda -> Term)
lambdaBody = Base.project _Lambda _Lambda_body

lambdaDomain :: TTerm (Lambda -> Maybe Type)
lambdaDomain = Base.project _Lambda _Lambda_domain

lambdaType :: TTerm Name -> TTerm Type -> TTerm LambdaType
lambdaType parameter body = Base.record _LambdaType [
  _LambdaType_parameter>>: parameter,
  _LambdaType_body>>: body]

lambdaTypeParameter :: TTerm (LambdaType -> Name)
lambdaTypeParameter = Base.project _LambdaType _LambdaType_parameter

lambdaTypeBody :: TTerm (LambdaType -> Type)
lambdaTypeBody = Base.project _LambdaType _LambdaType_body

letBinding :: TTerm Name -> TTerm Term -> TTerm (Maybe TypeScheme) -> TTerm LetBinding
letBinding name term mtype = Base.record _LetBinding [
  _LetBinding_name>>: name,
  _LetBinding_term>>: term,
  _LetBinding_type>>: mtype]

letExpression :: TTerm [LetBinding] -> TTerm Term -> TTerm Let
letExpression bindings environment = Base.record _Let [
  _Let_bindings>>: bindings,
  _Let_environment>>: environment]

letBindings :: TTerm (Let -> [LetBinding])
letBindings = Base.project _Let _Let_bindings

letBindingName :: TTerm (LetBinding -> Name)
letBindingName = Base.project _LetBinding _LetBinding_name

letBindingTerm :: TTerm (LetBinding -> Term)
letBindingTerm = Base.project _LetBinding _LetBinding_term

letBindingType :: TTerm (LetBinding -> Y.Maybe TypeScheme)
letBindingType = Base.project _LetBinding _LetBinding_type

letEnvironment :: TTerm (Let -> Term)
letEnvironment = Base.project _Let _Let_environment

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

literalTypeBoolean :: TTerm LiteralType
literalTypeBoolean = unitVariant _LiteralType _LiteralType_boolean

literalTypeFloat :: TTerm FloatType -> TTerm LiteralType
literalTypeFloat = variant _LiteralType _LiteralType_float

literalTypeInteger :: TTerm IntegerType -> TTerm LiteralType
literalTypeInteger = variant _LiteralType _LiteralType_integer

literalTypeString :: TTerm LiteralType
literalTypeString = unitVariant _LiteralType _LiteralType_string

mapType :: TTerm Type -> TTerm Type -> TTerm MapType
mapType keys values = Base.record _MapType [
  _MapType_keys>>: keys,
  _MapType_values>>: values]

mapTypeKeys :: TTerm (MapType -> Type)
mapTypeKeys = Base.project _MapType _MapType_keys

mapTypeValues :: TTerm (MapType -> Type)
mapTypeValues = Base.project _MapType _MapType_values

-- TODO: this is only here for legacy reasons
name :: Name -> TTerm Name
name (Name n) = wrap _Name $ string n

optionalCases :: TTerm Term -> TTerm Term -> TTerm OptionalCases
optionalCases nothing just = Base.record _OptionalCases [
  _OptionalCases_nothing>>: nothing,
  _OptionalCases_just>>: just]

optionalCasesNothing :: TTerm (OptionalCases -> Term)
optionalCasesNothing = Base.project _OptionalCases _OptionalCases_nothing

optionalCasesJust :: TTerm (OptionalCases -> Term)
optionalCasesJust = Base.project _OptionalCases _OptionalCases_just

projection :: TTerm Name -> TTerm Name -> TTerm Projection
projection tname fname = Base.record _Projection [
  _Projection_typeName>>: tname,
  _Projection_field>>: fname]

projectionTypeName :: TTerm (Projection -> Name)
projectionTypeName = Base.project _Projection _Projection_typeName

projectionField :: TTerm (Projection -> Name)
projectionField = Base.project _Projection _Projection_field

record :: TTerm Name -> TTerm [Field] -> TTerm Record
record typeName fields = Base.record _Record [
  _Record_typeName>>: typeName,
  _Record_fields>>: fields]

recordTypeName :: TTerm (Record -> Name)
recordTypeName = Base.project _Record _Record_typeName

recordFields :: TTerm (Record -> [Field])
recordFields = Base.project _Record _Record_fields

rowType :: TTerm Name -> TTerm [FieldType] -> TTerm (RowType)
rowType typeName fields = Base.record _RowType [
  _RowType_typeName>>: typeName,
  _RowType_fields>>: fields]

rowTypeTypeName :: TTerm (RowType -> Name)
rowTypeTypeName = Base.project _RowType _RowType_typeName

rowTypeFields :: TTerm (RowType -> [FieldType])
rowTypeFields = Base.project _RowType _RowType_fields

sum :: TTerm Int -> TTerm Int -> TTerm Term -> TTerm Sum
sum index size term = Base.record _Sum [
  _Sum_index>>: index,
  _Sum_size>>: size,
  _Sum_term>>: term]

sumIndex :: TTerm (Sum -> Int)
sumIndex = Base.project _Sum _Sum_index

sumSize :: TTerm (Sum -> Int)
sumSize = Base.project _Sum _Sum_size

sumTerm :: TTerm (Sum -> Term)
sumTerm = Base.project _Sum _Sum_term

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

termUnion :: TTerm Injection -> TTerm Term
termUnion = variant _Term _Term_union

termVariable :: TTerm Name -> TTerm Term
termVariable = variant _Term _Term_variable

termWrap :: TTerm WrappedTerm -> TTerm Term
termWrap = variant _Term _Term_wrap

tupleProjectionArity :: TTerm (TupleProjection -> Int)
tupleProjectionArity = Base.project _TupleProjection _TupleProjection_arity

tupleProjectionIndex :: TTerm (TupleProjection -> Int)
tupleProjectionIndex = Base.project _TupleProjection _TupleProjection_index

typeAbstractionParameter :: TTerm (TypeAbstraction -> Name)
typeAbstractionParameter = Base.project _TypeAbstraction _TypeAbstraction_parameter

typeAbstractionBody :: TTerm (TypeAbstraction -> Type)
typeAbstractionBody = Base.project _TypeAbstraction _TypeAbstraction_body

typeApplication :: TTerm ApplicationType -> TTerm Type
typeApplication = variant _Type _Type_application

typeFunction :: TTerm FunctionType -> TTerm Type
typeFunction = variant _Type _Type_function

typeLambda :: TTerm LambdaType -> TTerm Type
typeLambda = variant _Type _Type_lambda

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
typeScheme variables body = Base.record _TypeScheme [
  _TypeScheme_variables>>: variables,
  _TypeScheme_type>>: body]

typeSchemeVariables :: TTerm (TypeScheme -> [Name])
typeSchemeVariables = Base.project _TypeScheme _TypeScheme_variables

typeSchemeType :: TTerm (TypeScheme -> Type)
typeSchemeType = Base.project _TypeScheme _TypeScheme_type

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

typedTermTerm :: TTerm (TypedTerm -> Term)
typedTermTerm = Base.project _TypedTerm _TypedTerm_term

unName :: TTerm (Name -> String)
unName = unwrap _Name

unNamespace :: TTerm (Namespace -> String)
unNamespace = unwrap _Namespace

wrappedTerm :: TTerm Name -> TTerm Term -> TTerm WrappedTerm
wrappedTerm typeName object = Base.record _WrappedTerm [
  _WrappedTerm_typeName>>: typeName,
  _WrappedTerm_object>>: object]

wrappedTermTypeName :: TTerm (WrappedTerm -> Name)
wrappedTermTypeName = Base.project _WrappedTerm _WrappedTerm_typeName

wrappedTermObject :: TTerm (WrappedTerm -> Term)
wrappedTermObject = Base.project _WrappedTerm _WrappedTerm_object

wrappedType :: TTerm Name -> TTerm Type -> TTerm WrappedType
wrappedType typeName object = Base.record _WrappedType [
  _WrappedType_typeName>>: typeName,
  _WrappedType_object>>: object]

wrappedTypeTypeName :: TTerm (WrappedType -> Name)
wrappedTypeTypeName = Base.project _WrappedType _WrappedType_typeName

wrappedTypeObject :: TTerm (WrappedType -> Type)
wrappedTypeObject = Base.project _WrappedType _WrappedType_object
