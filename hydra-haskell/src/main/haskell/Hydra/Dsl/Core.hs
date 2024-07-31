module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Base as Base

import qualified Data.Map as M
import qualified Data.Maybe as Y


annotatedTerm :: Datum Term -> Datum (M.Map String Term) -> Datum AnnotatedTerm
annotatedTerm subject annotation = Base.record _AnnotatedTerm [
    _AnnotatedTerm_subject>>: subject,
    _AnnotatedTerm_annotation>>: annotation]

annotatedTermSubject :: Datum (AnnotatedTerm -> Term)
annotatedTermSubject = project _AnnotatedTerm _AnnotatedTerm_subject

annotatedTermAnnotation :: Datum (AnnotatedTerm -> M.Map String Term)
annotatedTermAnnotation = project _AnnotatedTerm _AnnotatedTerm_annotation

annotatedType :: Datum Type -> Datum (M.Map String Term) -> Datum AnnotatedType
annotatedType subject annotation = Base.record _AnnotatedType [
    _AnnotatedType_subject>>: subject,
    _AnnotatedType_annotation>>: annotation]

annotatedTypeSubject :: Datum (AnnotatedType -> Type)
annotatedTypeSubject = project _AnnotatedType _AnnotatedType_subject

annotatedTypeAnnotation :: Datum (AnnotatedType -> M.Map String Term)
annotatedTypeAnnotation = project _AnnotatedType _AnnotatedType_annotation

application :: Datum Term -> Datum Term -> Datum (Application)
application function argument = Base.record _Application [
    _Application_function>>: function,
    _Application_argument>>: argument]

applicationFunction :: Datum (Application -> Term)
applicationFunction = project _Application _Application_function

applicationArgument :: Datum (Application -> Term)
applicationArgument = project _Application _Application_argument

applicationType :: Datum Type -> Datum Type -> Datum (ApplicationType)
applicationType function argument = Base.record _ApplicationType [
    _ApplicationType_function>>: function,
    _ApplicationType_argument>>: argument]

applicationTypeFunction :: Datum (ApplicationType -> Type)
applicationTypeFunction = project _ApplicationType _ApplicationType_function

applicationTypeArgument :: Datum (ApplicationType -> Type)
applicationTypeArgument = project _ApplicationType _ApplicationType_argument

caseStatement :: Datum Name -> Datum (Maybe Term) -> Datum [Field] -> Datum (CaseStatement)
caseStatement typeName defaultTerm cases = Base.record _CaseStatement [
    _CaseStatement_typeName>>: typeName,
    _CaseStatement_default>>: defaultTerm,
    _CaseStatement_cases>>: cases]

caseStatementTypeName :: Datum (CaseStatement -> Name)
caseStatementTypeName = project _CaseStatement _CaseStatement_typeName

caseStatementDefault :: Datum (CaseStatement -> Maybe Term)
caseStatementDefault = project _CaseStatement _CaseStatement_default

caseStatementCases :: Datum (CaseStatement -> [Field])
caseStatementCases = project _CaseStatement _CaseStatement_cases

field :: Datum Name -> Datum Term -> Datum Field
field name term = Base.record _Field [
    _Field_name>>: name,
    _Field_term>>: term]

fieldName :: Datum (Field -> Name)
fieldName = project _Field _Field_name

fieldTerm :: Datum (Field -> Term)
fieldTerm = project _Field _Field_term

fieldType :: Datum Name -> Datum Type -> Datum (FieldType)
fieldType name typ = Base.record _FieldType [
    _FieldType_name>>: name,
    _FieldType_type>>: typ]

fieldTypeName :: Datum (FieldType -> Name)
fieldTypeName = project _FieldType _FieldType_name

fieldTypeType :: Datum (FieldType -> Type)
fieldTypeType = project _FieldType _FieldType_type

functionType :: Datum Type -> Datum Type -> Datum (FunctionType)
functionType domain codomain = Base.record _FunctionType [
    _FunctionType_domain>>: domain,
    _FunctionType_codomain>>: codomain]

functionTypeDomain :: Datum (FunctionType -> Type)
functionTypeDomain = project _FunctionType _FunctionType_domain

functionTypeCodomain :: Datum (FunctionType -> Type)
functionTypeCodomain = project _FunctionType _FunctionType_codomain

injection :: Datum Name -> Datum Field -> Datum Injection
injection typeName field = Base.record _Injection [
    _Injection_typeName>>: typeName,
    _Injection_field>>: field]

injectionTypeName :: Datum (Injection -> Name)
injectionTypeName = project _Injection _Injection_typeName

injectionField :: Datum (Injection -> Field)
injectionField = project _Injection _Injection_field

lambda :: Datum Name -> Datum Term -> Datum Lambda
lambda parameter body = Base.record _Lambda [
    _Lambda_parameter>>: parameter,
    _Lambda_body>>: body]

lambdaParameter :: Datum (Lambda -> Name)
lambdaParameter = project _Lambda _Lambda_parameter

lambdaBody :: Datum (Lambda -> Term)
lambdaBody = project _Lambda _Lambda_body

lambdaType :: Datum Name -> Datum Type -> Datum LambdaType
lambdaType parameter body = Base.record _LambdaType [
    _LambdaType_parameter>>: parameter,
    _LambdaType_body>>: body]

lambdaTypeParameter :: Datum (LambdaType -> Name)
lambdaTypeParameter = project _LambdaType _LambdaType_parameter

lambdaTypeBody :: Datum (LambdaType -> Type)
lambdaTypeBody = project _LambdaType _LambdaType_body

letExpression :: Datum [LetBinding] -> Datum Term -> Datum Let
letExpression bindings environment = Base.record _Let [
    _Let_bindings>>: bindings,
    _Let_environment>>: environment]

letBindings :: Datum (Let -> [LetBinding])
letBindings = project _Let _Let_bindings

letBindingName :: Datum (LetBinding -> Name)
letBindingName = project _LetBinding _LetBinding_name

letBindingTerm :: Datum (LetBinding -> Term)
letBindingTerm = project _LetBinding _LetBinding_term

letBindingType :: Datum (LetBinding -> Y.Maybe TypeScheme)
letBindingType = project _LetBinding _LetBinding_type

letEnvironment :: Datum (Let -> Term)
letEnvironment = project _Let _Let_environment

literalBinary :: Datum String -> Datum Literal
literalBinary = variant _Literal _Literal_binary

literalBoolean :: Datum Bool -> Datum Literal
literalBoolean = variant _Literal _Literal_boolean

literalFloat :: Datum FloatValue -> Datum Literal
literalFloat = variant _Literal _Literal_float

literalInteger :: Datum IntegerValue -> Datum Literal
literalInteger = variant _Literal _Literal_integer

mapType :: Datum Type -> Datum Type -> Datum MapType
mapType keys values = Base.record _MapType [
    _MapType_keys>>: keys,
    _MapType_values>>: values]

mapTypeKeys :: Datum (MapType -> Type)
mapTypeKeys = project _MapType _MapType_keys

mapTypeValues :: Datum (MapType -> Type)
mapTypeValues = project _MapType _MapType_values

optionalCases :: Datum Term -> Datum Term -> Datum OptionalCases
optionalCases nothing just = Base.record _OptionalCases [
    _OptionalCases_nothing>>: nothing,
    _OptionalCases_just>>: just]

optionalCasesNothing :: Datum (OptionalCases -> Term)
optionalCasesNothing = project _OptionalCases _OptionalCases_nothing

optionalCasesJust :: Datum (OptionalCases -> Term)
optionalCasesJust = project _OptionalCases _OptionalCases_just

projectionTypeName :: Datum (Projection -> Name)
projectionTypeName = project _Projection _Projection_typeName

projectionField :: Datum (Projection -> Name)
projectionField = project _Projection _Projection_field

record :: Datum Name -> Datum [Field] -> Datum Record
record typeName fields = Base.record _Record [
    _Record_typeName>>: typeName,
    _Record_fields>>: fields]

recordTypeName :: Datum (Record -> Name)
recordTypeName = project _Record _Record_typeName

recordFields :: Datum (Record -> [Field])
recordFields = project _Record _Record_fields

rowType :: Datum Name -> Datum (Maybe Name) -> Datum [FieldType] -> Datum (RowType)
rowType typeName extends fields = Base.record _RowType [
    _RowType_typeName>>: typeName,
    _RowType_extends>>: extends,
    _RowType_fields>>: fields]

rowTypeTypeName :: Datum (RowType -> Name)
rowTypeTypeName = project _RowType _RowType_typeName

rowTypeExtends :: Datum (RowType -> Maybe Name)
rowTypeExtends = project _RowType _RowType_extends

rowTypeFields :: Datum (RowType -> [FieldType])
rowTypeFields = project _RowType _RowType_fields

sum :: Datum Int -> Datum Int -> Datum Term -> Datum Sum
sum index size term = Base.record _Sum [
    _Sum_index>>: index,
    _Sum_size>>: size,
    _Sum_term>>: term]

sumIndex :: Datum (Sum -> Int)
sumIndex = project _Sum _Sum_index

sumSize :: Datum (Sum -> Int)
sumSize = project _Sum _Sum_size

sumTerm :: Datum (Sum -> Term)
sumTerm = project _Sum _Sum_term

termAnnotated :: Datum AnnotatedTerm -> Datum Term
termAnnotated = variant _Term _Term_annotated

tupleProjectionArity :: Datum (TupleProjection -> Int)
tupleProjectionArity = project _TupleProjection _TupleProjection_arity

tupleProjectionIndex :: Datum (TupleProjection -> Int)
tupleProjectionIndex = project _TupleProjection _TupleProjection_index

typeSchemeVariables :: Datum (TypeScheme -> [Name])
typeSchemeVariables = project _TypeScheme _TypeScheme_variables

typeSchemeType :: Datum (TypeScheme -> Type)
typeSchemeType = project _TypeScheme _TypeScheme_type

typedTermTerm :: Datum (TypedTerm -> Term)
typedTermTerm = project _TypedTerm _TypedTerm_term

wrappedTerm :: Datum Name -> Datum Term -> Datum WrappedTerm
wrappedTerm typeName object = Base.record _WrappedTerm [
    _WrappedTerm_typeName>>: typeName,
    _WrappedTerm_object>>: object]

wrappedTermTypeName :: Datum (WrappedTerm -> Name)
wrappedTermTypeName = project _WrappedTerm _WrappedTerm_typeName

wrappedTermObject :: Datum (WrappedTerm -> Term)
wrappedTermObject = project _WrappedTerm _WrappedTerm_object

wrappedType :: Datum Name -> Datum Type -> Datum WrappedType
wrappedType typeName object = Base.record _WrappedType [
    _WrappedType_typeName>>: typeName,
    _WrappedType_object>>: object]

wrappedTypeTypeName :: Datum (WrappedType -> Name)
wrappedTypeTypeName = project _WrappedType _WrappedType_typeName

wrappedTypeObject :: Datum (WrappedType -> Type)
wrappedTypeObject = project _WrappedType _WrappedType_object
