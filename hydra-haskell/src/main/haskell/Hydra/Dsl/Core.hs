module Hydra.Dsl.Core where

import Hydra.Kernel
import Hydra.Dsl.Base as Base

import qualified Data.Map as M


annotated :: Datum x -> Datum a -> Datum (Annotated x a)
annotated subject annotation = Base.record _Annotated [
    _Annotated_subject>>: subject,
    _Annotated_annotation>>: annotation]

annotatedSubject :: Datum (Annotated x a -> x)
annotatedSubject = project _Annotated _Annotated_subject

annotatedAnnotation :: Datum (Annotated x a -> a)
annotatedAnnotation = project _Annotated _Annotated_annotation

application :: Datum (Term a) -> Datum (Term a) -> Datum (Application a)
application function argument = Base.record _Application [
    _Application_function>>: function,
    _Application_argument>>: argument]

applicationFunction :: Datum (Application a -> Term a)
applicationFunction = project _Application _Application_function

applicationArgument :: Datum (Application a -> Term a)
applicationArgument = project _Application _Application_argument

applicationType :: Datum (Type a) -> Datum (Type a) -> Datum (ApplicationType a)
applicationType function argument = Base.record _ApplicationType [
    _ApplicationType_function>>: function,
    _ApplicationType_argument>>: argument]

applicationTypeFunction :: Datum (ApplicationType a -> Type a)
applicationTypeFunction = project _ApplicationType _ApplicationType_function

applicationTypeArgument :: Datum (ApplicationType a -> Type a)
applicationTypeArgument = project _ApplicationType _ApplicationType_argument

caseStatement :: Datum Name -> Datum (Maybe (Term a)) -> Datum [Field a] -> Datum (CaseStatement a)
caseStatement typeName defaultTerm cases = Base.record _CaseStatement [
    _CaseStatement_typeName>>: typeName,
    _CaseStatement_default>>: defaultTerm,
    _CaseStatement_cases>>: cases]

caseStatementTypeName :: Datum (CaseStatement a -> Name)
caseStatementTypeName = project _CaseStatement _CaseStatement_typeName

caseStatementDefault :: Datum (CaseStatement a -> Maybe (Term a))
caseStatementDefault = project _CaseStatement _CaseStatement_default

caseStatementCases :: Datum (CaseStatement a -> [Field a])
caseStatementCases = project _CaseStatement _CaseStatement_cases

field :: Datum Name -> Datum (Term a) -> Datum (Field a)
field name term = Base.record _Field [
    _Field_name>>: name,
    _Field_term>>: term]

fieldName :: Datum (Field a -> Name)
fieldName = project _Field _Field_name

fieldTerm :: Datum (Field a -> Term a)
fieldTerm = project _Field _Field_term

fieldType :: Datum Name -> Datum (Type a) -> Datum (FieldType a)
fieldType name typ = Base.record _FieldType [
    _FieldType_name>>: name,
    _FieldType_type>>: typ]

fieldTypeName :: Datum (FieldType a -> Name)
fieldTypeName = project _FieldType _FieldType_name

fieldTypeType :: Datum (FieldType a -> Type a)
fieldTypeType = project _FieldType _FieldType_type

functionType :: Datum (Type a) -> Datum (Type a) -> Datum (FunctionType a)
functionType domain codomain = Base.record _FunctionType [
    _FunctionType_domain>>: domain,
    _FunctionType_codomain>>: codomain]

functionTypeDomain :: Datum (FunctionType a -> Type a)
functionTypeDomain = project _FunctionType _FunctionType_domain

functionTypeCodomain :: Datum (FunctionType a -> Type a)
functionTypeCodomain = project _FunctionType _FunctionType_codomain

injection :: Datum Name -> Datum (Field a) -> Datum (Injection a)
injection typeName field = Base.record _Injection [
    _Injection_typeName>>: typeName,
    _Injection_field>>: field]

injectionTypeName :: Datum (Injection a -> Name)
injectionTypeName = project _Injection _Injection_typeName

injectionField :: Datum (Injection a -> Field a)
injectionField = project _Injection _Injection_field

lambda :: Datum Name -> Datum (Term a) -> Datum (Lambda a)
lambda parameter body = Base.record _Lambda [
    _Lambda_parameter>>: parameter,
    _Lambda_body>>: body]

lambdaParameter :: Datum (Lambda a -> Name)
lambdaParameter = project _Lambda _Lambda_parameter

lambdaBody :: Datum (Lambda a -> Term a)
lambdaBody = project _Lambda _Lambda_body

lambdaType :: Datum Name -> Datum (Type a) -> Datum (LambdaType a)
lambdaType parameter body = Base.record _LambdaType [
    _LambdaType_parameter>>: parameter,
    _LambdaType_body>>: body]

lambdaTypeParameter :: Datum (LambdaType a -> Name)
lambdaTypeParameter = project _LambdaType _LambdaType_parameter

lambdaTypeBody :: Datum (LambdaType a -> Type a)
lambdaTypeBody = project _LambdaType _LambdaType_body

letExpression :: Datum (M.Map Name (Term a)) -> Datum (Term a) -> Datum (Let a)
letExpression bindings environment = Base.record _Let [
    _Let_bindings>>: bindings,
    _Let_environment>>: environment]

letBindings :: Datum (Let a -> M.Map Name (Term a))
letBindings = project _Let _Let_bindings

letEnvironment :: Datum (Let a -> Term a)
letEnvironment = project _Let _Let_environment

mapType :: Datum (Type a) -> Datum (Type a) -> Datum (MapType a)
mapType keys values = Base.record _MapType [
    _MapType_keys>>: keys,
    _MapType_values>>: values]

mapTypeKeys :: Datum (MapType a -> Type a)
mapTypeKeys = project _MapType _MapType_keys

mapTypeValues :: Datum (MapType a -> Type a)
mapTypeValues = project _MapType _MapType_values

nominal :: Datum Name -> Datum x -> Datum (Nominal x)
nominal typeName object = Base.record _Nominal [
    _Nominal_typeName>>: typeName,
    _Nominal_object>>: object]

nominalTypeName :: Datum (Nominal x -> Name)
nominalTypeName = project _Nominal _Nominal_typeName

nominalObject :: Datum (Nominal x -> x)
nominalObject = project _Nominal _Nominal_object

optionalCases :: Datum (Term a) -> Datum (Term a) -> Datum (OptionalCases a)
optionalCases nothing just = Base.record _OptionalCases [
    _OptionalCases_nothing>>: nothing,
    _OptionalCases_just>>: just]

optionalCasesNothing :: Datum (OptionalCases a -> Term a)
optionalCasesNothing = project _OptionalCases _OptionalCases_nothing

optionalCasesJust :: Datum (OptionalCases a -> Term a)
optionalCasesJust = project _OptionalCases _OptionalCases_just

record :: Datum Name -> Datum [Field a] -> Datum (Record a)
record typeName fields = Base.record _Record [
    _Record_typeName>>: typeName,
    _Record_fields>>: fields]

recordTypeName :: Datum (Record a -> Name)
recordTypeName = project _Record _Record_typeName

recordFields :: Datum (Record a -> [Field a])
recordFields = project _Record _Record_fields

rowType :: Datum Name -> Datum (Maybe Name) -> Datum [FieldType a] -> Datum (RowType a)
rowType typeName extends fields = Base.record _RowType [
    _RowType_typeName>>: typeName,
    _RowType_extends>>: extends,
    _RowType_fields>>: fields]

rowTypeTypeName :: Datum (RowType a -> Name)
rowTypeTypeName = project _RowType _RowType_typeName

rowTypeExtends :: Datum (RowType a -> Maybe Name)
rowTypeExtends = project _RowType _RowType_extends

rowTypeFields :: Datum (RowType a -> [FieldType a])
rowTypeFields = project _RowType _RowType_fields

sum :: Datum Int -> Datum Int -> Datum (Term a) -> Datum (Sum a)
sum index size term = Base.record _Sum [
    _Sum_index>>: index,
    _Sum_size>>: size,
    _Sum_term>>: term]

sumIndex :: Datum (Sum a -> Int)
sumIndex = project _Sum _Sum_index

sumSize :: Datum (Sum a -> Int)
sumSize = project _Sum _Sum_size

sumTerm :: Datum (Sum a -> Term a)
sumTerm = project _Sum _Sum_term
