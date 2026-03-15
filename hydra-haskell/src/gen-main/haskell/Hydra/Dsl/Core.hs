-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.core

module Hydra.Dsl.Core where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

annotatedTerm :: (Core.Term -> M.Map Core.Name Core.Term -> Core.AnnotatedTerm)
annotatedTerm body annotation = Core.AnnotatedTerm {
  Core.annotatedTermBody = body,
  Core.annotatedTermAnnotation = annotation}

annotatedTermBody :: (Core.AnnotatedTerm -> Core.Term)
annotatedTermBody = Core.annotatedTermBody

annotatedTermAnnotation :: (Core.AnnotatedTerm -> M.Map Core.Name Core.Term)
annotatedTermAnnotation = Core.annotatedTermAnnotation

annotatedTermWithBody :: (Core.AnnotatedTerm -> Core.Term -> Core.AnnotatedTerm)
annotatedTermWithBody original newVal = Core.AnnotatedTerm {
  Core.annotatedTermBody = newVal,
  Core.annotatedTermAnnotation = (Core.annotatedTermAnnotation original)}

annotatedTermWithAnnotation :: (Core.AnnotatedTerm -> M.Map Core.Name Core.Term -> Core.AnnotatedTerm)
annotatedTermWithAnnotation original newVal = Core.AnnotatedTerm {
  Core.annotatedTermBody = (Core.annotatedTermBody original),
  Core.annotatedTermAnnotation = newVal}

annotatedType :: (Core.Type -> M.Map Core.Name Core.Term -> Core.AnnotatedType)
annotatedType body annotation = Core.AnnotatedType {
  Core.annotatedTypeBody = body,
  Core.annotatedTypeAnnotation = annotation}

annotatedTypeBody :: (Core.AnnotatedType -> Core.Type)
annotatedTypeBody = Core.annotatedTypeBody

annotatedTypeAnnotation :: (Core.AnnotatedType -> M.Map Core.Name Core.Term)
annotatedTypeAnnotation = Core.annotatedTypeAnnotation

annotatedTypeWithBody :: (Core.AnnotatedType -> Core.Type -> Core.AnnotatedType)
annotatedTypeWithBody original newVal = Core.AnnotatedType {
  Core.annotatedTypeBody = newVal,
  Core.annotatedTypeAnnotation = (Core.annotatedTypeAnnotation original)}

annotatedTypeWithAnnotation :: (Core.AnnotatedType -> M.Map Core.Name Core.Term -> Core.AnnotatedType)
annotatedTypeWithAnnotation original newVal = Core.AnnotatedType {
  Core.annotatedTypeBody = (Core.annotatedTypeBody original),
  Core.annotatedTypeAnnotation = newVal}

application :: (Core.Term -> Core.Term -> Core.Application)
application function argument = Core.Application {
  Core.applicationFunction = function,
  Core.applicationArgument = argument}

applicationFunction :: (Core.Application -> Core.Term)
applicationFunction = Core.applicationFunction

applicationArgument :: (Core.Application -> Core.Term)
applicationArgument = Core.applicationArgument

applicationWithFunction :: (Core.Application -> Core.Term -> Core.Application)
applicationWithFunction original newVal = Core.Application {
  Core.applicationFunction = newVal,
  Core.applicationArgument = (Core.applicationArgument original)}

applicationWithArgument :: (Core.Application -> Core.Term -> Core.Application)
applicationWithArgument original newVal = Core.Application {
  Core.applicationFunction = (Core.applicationFunction original),
  Core.applicationArgument = newVal}

applicationType :: (Core.Type -> Core.Type -> Core.ApplicationType)
applicationType function argument = Core.ApplicationType {
  Core.applicationTypeFunction = function,
  Core.applicationTypeArgument = argument}

applicationTypeFunction :: (Core.ApplicationType -> Core.Type)
applicationTypeFunction = Core.applicationTypeFunction

applicationTypeArgument :: (Core.ApplicationType -> Core.Type)
applicationTypeArgument = Core.applicationTypeArgument

applicationTypeWithFunction :: (Core.ApplicationType -> Core.Type -> Core.ApplicationType)
applicationTypeWithFunction original newVal = Core.ApplicationType {
  Core.applicationTypeFunction = newVal,
  Core.applicationTypeArgument = (Core.applicationTypeArgument original)}

applicationTypeWithArgument :: (Core.ApplicationType -> Core.Type -> Core.ApplicationType)
applicationTypeWithArgument original newVal = Core.ApplicationType {
  Core.applicationTypeFunction = (Core.applicationTypeFunction original),
  Core.applicationTypeArgument = newVal}

binding :: (Core.Name -> Core.Term -> Maybe Core.TypeScheme -> Core.Binding)
binding name term type_ = Core.Binding {
  Core.bindingName = name,
  Core.bindingTerm = term,
  Core.bindingType = type_}

bindingName :: (Core.Binding -> Core.Name)
bindingName = Core.bindingName

bindingTerm :: (Core.Binding -> Core.Term)
bindingTerm = Core.bindingTerm

bindingType :: (Core.Binding -> Maybe Core.TypeScheme)
bindingType = Core.bindingType

bindingWithName :: (Core.Binding -> Core.Name -> Core.Binding)
bindingWithName original newVal = Core.Binding {
  Core.bindingName = newVal,
  Core.bindingTerm = (Core.bindingTerm original),
  Core.bindingType = (Core.bindingType original)}

bindingWithTerm :: (Core.Binding -> Core.Term -> Core.Binding)
bindingWithTerm original newVal = Core.Binding {
  Core.bindingName = (Core.bindingName original),
  Core.bindingTerm = newVal,
  Core.bindingType = (Core.bindingType original)}

bindingWithType :: (Core.Binding -> Maybe Core.TypeScheme -> Core.Binding)
bindingWithType original newVal = Core.Binding {
  Core.bindingName = (Core.bindingName original),
  Core.bindingTerm = (Core.bindingTerm original),
  Core.bindingType = newVal}

caseStatement :: (Core.Name -> Maybe Core.Term -> [Core.Field] -> Core.CaseStatement)
caseStatement typeName default_ cases = Core.CaseStatement {
  Core.caseStatementTypeName = typeName,
  Core.caseStatementDefault = default_,
  Core.caseStatementCases = cases}

caseStatementTypeName :: (Core.CaseStatement -> Core.Name)
caseStatementTypeName = Core.caseStatementTypeName

caseStatementDefault :: (Core.CaseStatement -> Maybe Core.Term)
caseStatementDefault = Core.caseStatementDefault

caseStatementCases :: (Core.CaseStatement -> [Core.Field])
caseStatementCases = Core.caseStatementCases

caseStatementWithTypeName :: (Core.CaseStatement -> Core.Name -> Core.CaseStatement)
caseStatementWithTypeName original newVal = Core.CaseStatement {
  Core.caseStatementTypeName = newVal,
  Core.caseStatementDefault = (Core.caseStatementDefault original),
  Core.caseStatementCases = (Core.caseStatementCases original)}

caseStatementWithDefault :: (Core.CaseStatement -> Maybe Core.Term -> Core.CaseStatement)
caseStatementWithDefault original newVal = Core.CaseStatement {
  Core.caseStatementTypeName = (Core.caseStatementTypeName original),
  Core.caseStatementDefault = newVal,
  Core.caseStatementCases = (Core.caseStatementCases original)}

caseStatementWithCases :: (Core.CaseStatement -> [Core.Field] -> Core.CaseStatement)
caseStatementWithCases original newVal = Core.CaseStatement {
  Core.caseStatementTypeName = (Core.caseStatementTypeName original),
  Core.caseStatementDefault = (Core.caseStatementDefault original),
  Core.caseStatementCases = newVal}

eitherType :: (Core.Type -> Core.Type -> Core.EitherType)
eitherType left right = Core.EitherType {
  Core.eitherTypeLeft = left,
  Core.eitherTypeRight = right}

eitherTypeLeft :: (Core.EitherType -> Core.Type)
eitherTypeLeft = Core.eitherTypeLeft

eitherTypeRight :: (Core.EitherType -> Core.Type)
eitherTypeRight = Core.eitherTypeRight

eitherTypeWithLeft :: (Core.EitherType -> Core.Type -> Core.EitherType)
eitherTypeWithLeft original newVal = Core.EitherType {
  Core.eitherTypeLeft = newVal,
  Core.eitherTypeRight = (Core.eitherTypeRight original)}

eitherTypeWithRight :: (Core.EitherType -> Core.Type -> Core.EitherType)
eitherTypeWithRight original newVal = Core.EitherType {
  Core.eitherTypeLeft = (Core.eitherTypeLeft original),
  Core.eitherTypeRight = newVal}

pairType :: (Core.Type -> Core.Type -> Core.PairType)
pairType first second = Core.PairType {
  Core.pairTypeFirst = first,
  Core.pairTypeSecond = second}

pairTypeFirst :: (Core.PairType -> Core.Type)
pairTypeFirst = Core.pairTypeFirst

pairTypeSecond :: (Core.PairType -> Core.Type)
pairTypeSecond = Core.pairTypeSecond

pairTypeWithFirst :: (Core.PairType -> Core.Type -> Core.PairType)
pairTypeWithFirst original newVal = Core.PairType {
  Core.pairTypeFirst = newVal,
  Core.pairTypeSecond = (Core.pairTypeSecond original)}

pairTypeWithSecond :: (Core.PairType -> Core.Type -> Core.PairType)
pairTypeWithSecond original newVal = Core.PairType {
  Core.pairTypeFirst = (Core.pairTypeFirst original),
  Core.pairTypeSecond = newVal}

eliminationRecord :: (Core.Projection -> Core.Elimination)
eliminationRecord x = (Core.EliminationRecord x)

eliminationUnion :: (Core.CaseStatement -> Core.Elimination)
eliminationUnion x = (Core.EliminationUnion x)

eliminationWrap :: (Core.Name -> Core.Elimination)
eliminationWrap x = (Core.EliminationWrap x)

field :: (Core.Name -> Core.Term -> Core.Field)
field name term = Core.Field {
  Core.fieldName = name,
  Core.fieldTerm = term}

fieldName :: (Core.Field -> Core.Name)
fieldName = Core.fieldName

fieldTerm :: (Core.Field -> Core.Term)
fieldTerm = Core.fieldTerm

fieldWithName :: (Core.Field -> Core.Name -> Core.Field)
fieldWithName original newVal = Core.Field {
  Core.fieldName = newVal,
  Core.fieldTerm = (Core.fieldTerm original)}

fieldWithTerm :: (Core.Field -> Core.Term -> Core.Field)
fieldWithTerm original newVal = Core.Field {
  Core.fieldName = (Core.fieldName original),
  Core.fieldTerm = newVal}

fieldType :: (Core.Name -> Core.Type -> Core.FieldType)
fieldType name type_ = Core.FieldType {
  Core.fieldTypeName = name,
  Core.fieldTypeType = type_}

fieldTypeName :: (Core.FieldType -> Core.Name)
fieldTypeName = Core.fieldTypeName

fieldTypeType :: (Core.FieldType -> Core.Type)
fieldTypeType = Core.fieldTypeType

fieldTypeWithName :: (Core.FieldType -> Core.Name -> Core.FieldType)
fieldTypeWithName original newVal = Core.FieldType {
  Core.fieldTypeName = newVal,
  Core.fieldTypeType = (Core.fieldTypeType original)}

fieldTypeWithType :: (Core.FieldType -> Core.Type -> Core.FieldType)
fieldTypeWithType original newVal = Core.FieldType {
  Core.fieldTypeName = (Core.fieldTypeName original),
  Core.fieldTypeType = newVal}

floatTypeBigfloat :: Core.FloatType
floatTypeBigfloat = Core.FloatTypeBigfloat

floatTypeFloat32 :: Core.FloatType
floatTypeFloat32 = Core.FloatTypeFloat32

floatTypeFloat64 :: Core.FloatType
floatTypeFloat64 = Core.FloatTypeFloat64

floatValueBigfloat :: (Double -> Core.FloatValue)
floatValueBigfloat x = (Core.FloatValueBigfloat x)

floatValueFloat32 :: (Float -> Core.FloatValue)
floatValueFloat32 x = (Core.FloatValueFloat32 x)

floatValueFloat64 :: (Double -> Core.FloatValue)
floatValueFloat64 x = (Core.FloatValueFloat64 x)

forallType :: (Core.Name -> Core.Type -> Core.ForallType)
forallType parameter body = Core.ForallType {
  Core.forallTypeParameter = parameter,
  Core.forallTypeBody = body}

forallTypeParameter :: (Core.ForallType -> Core.Name)
forallTypeParameter = Core.forallTypeParameter

forallTypeBody :: (Core.ForallType -> Core.Type)
forallTypeBody = Core.forallTypeBody

forallTypeWithParameter :: (Core.ForallType -> Core.Name -> Core.ForallType)
forallTypeWithParameter original newVal = Core.ForallType {
  Core.forallTypeParameter = newVal,
  Core.forallTypeBody = (Core.forallTypeBody original)}

forallTypeWithBody :: (Core.ForallType -> Core.Type -> Core.ForallType)
forallTypeWithBody original newVal = Core.ForallType {
  Core.forallTypeParameter = (Core.forallTypeParameter original),
  Core.forallTypeBody = newVal}

functionElimination :: (Core.Elimination -> Core.Function)
functionElimination x = (Core.FunctionElimination x)

functionLambda :: (Core.Lambda -> Core.Function)
functionLambda x = (Core.FunctionLambda x)

functionPrimitive :: (Core.Name -> Core.Function)
functionPrimitive x = (Core.FunctionPrimitive x)

functionType :: (Core.Type -> Core.Type -> Core.FunctionType)
functionType domain codomain = Core.FunctionType {
  Core.functionTypeDomain = domain,
  Core.functionTypeCodomain = codomain}

functionTypeDomain :: (Core.FunctionType -> Core.Type)
functionTypeDomain = Core.functionTypeDomain

functionTypeCodomain :: (Core.FunctionType -> Core.Type)
functionTypeCodomain = Core.functionTypeCodomain

functionTypeWithDomain :: (Core.FunctionType -> Core.Type -> Core.FunctionType)
functionTypeWithDomain original newVal = Core.FunctionType {
  Core.functionTypeDomain = newVal,
  Core.functionTypeCodomain = (Core.functionTypeCodomain original)}

functionTypeWithCodomain :: (Core.FunctionType -> Core.Type -> Core.FunctionType)
functionTypeWithCodomain original newVal = Core.FunctionType {
  Core.functionTypeDomain = (Core.functionTypeDomain original),
  Core.functionTypeCodomain = newVal}

injection :: (Core.Name -> Core.Field -> Core.Injection)
injection typeName field = Core.Injection {
  Core.injectionTypeName = typeName,
  Core.injectionField = field}

injectionTypeName :: (Core.Injection -> Core.Name)
injectionTypeName = Core.injectionTypeName

injectionField :: (Core.Injection -> Core.Field)
injectionField = Core.injectionField

injectionWithTypeName :: (Core.Injection -> Core.Name -> Core.Injection)
injectionWithTypeName original newVal = Core.Injection {
  Core.injectionTypeName = newVal,
  Core.injectionField = (Core.injectionField original)}

injectionWithField :: (Core.Injection -> Core.Field -> Core.Injection)
injectionWithField original newVal = Core.Injection {
  Core.injectionTypeName = (Core.injectionTypeName original),
  Core.injectionField = newVal}

integerTypeBigint :: Core.IntegerType
integerTypeBigint = Core.IntegerTypeBigint

integerTypeInt8 :: Core.IntegerType
integerTypeInt8 = Core.IntegerTypeInt8

integerTypeInt16 :: Core.IntegerType
integerTypeInt16 = Core.IntegerTypeInt16

integerTypeInt32 :: Core.IntegerType
integerTypeInt32 = Core.IntegerTypeInt32

integerTypeInt64 :: Core.IntegerType
integerTypeInt64 = Core.IntegerTypeInt64

integerTypeUint8 :: Core.IntegerType
integerTypeUint8 = Core.IntegerTypeUint8

integerTypeUint16 :: Core.IntegerType
integerTypeUint16 = Core.IntegerTypeUint16

integerTypeUint32 :: Core.IntegerType
integerTypeUint32 = Core.IntegerTypeUint32

integerTypeUint64 :: Core.IntegerType
integerTypeUint64 = Core.IntegerTypeUint64

integerValueBigint :: (Integer -> Core.IntegerValue)
integerValueBigint x = (Core.IntegerValueBigint x)

integerValueInt8 :: (I.Int8 -> Core.IntegerValue)
integerValueInt8 x = (Core.IntegerValueInt8 x)

integerValueInt16 :: (I.Int16 -> Core.IntegerValue)
integerValueInt16 x = (Core.IntegerValueInt16 x)

integerValueInt32 :: (Int -> Core.IntegerValue)
integerValueInt32 x = (Core.IntegerValueInt32 x)

integerValueInt64 :: (I.Int64 -> Core.IntegerValue)
integerValueInt64 x = (Core.IntegerValueInt64 x)

integerValueUint8 :: (I.Int16 -> Core.IntegerValue)
integerValueUint8 x = (Core.IntegerValueUint8 x)

integerValueUint16 :: (Int -> Core.IntegerValue)
integerValueUint16 x = (Core.IntegerValueUint16 x)

integerValueUint32 :: (I.Int64 -> Core.IntegerValue)
integerValueUint32 x = (Core.IntegerValueUint32 x)

integerValueUint64 :: (Integer -> Core.IntegerValue)
integerValueUint64 x = (Core.IntegerValueUint64 x)

lambda :: (Core.Name -> Maybe Core.Type -> Core.Term -> Core.Lambda)
lambda parameter domain body = Core.Lambda {
  Core.lambdaParameter = parameter,
  Core.lambdaDomain = domain,
  Core.lambdaBody = body}

lambdaParameter :: (Core.Lambda -> Core.Name)
lambdaParameter = Core.lambdaParameter

lambdaDomain :: (Core.Lambda -> Maybe Core.Type)
lambdaDomain = Core.lambdaDomain

lambdaBody :: (Core.Lambda -> Core.Term)
lambdaBody = Core.lambdaBody

lambdaWithParameter :: (Core.Lambda -> Core.Name -> Core.Lambda)
lambdaWithParameter original newVal = Core.Lambda {
  Core.lambdaParameter = newVal,
  Core.lambdaDomain = (Core.lambdaDomain original),
  Core.lambdaBody = (Core.lambdaBody original)}

lambdaWithDomain :: (Core.Lambda -> Maybe Core.Type -> Core.Lambda)
lambdaWithDomain original newVal = Core.Lambda {
  Core.lambdaParameter = (Core.lambdaParameter original),
  Core.lambdaDomain = newVal,
  Core.lambdaBody = (Core.lambdaBody original)}

lambdaWithBody :: (Core.Lambda -> Core.Term -> Core.Lambda)
lambdaWithBody original newVal = Core.Lambda {
  Core.lambdaParameter = (Core.lambdaParameter original),
  Core.lambdaDomain = (Core.lambdaDomain original),
  Core.lambdaBody = newVal}

let_ :: ([Core.Binding] -> Core.Term -> Core.Let)
let_ bindings body = Core.Let {
  Core.letBindings = bindings,
  Core.letBody = body}

letBindings :: (Core.Let -> [Core.Binding])
letBindings = Core.letBindings

letBody :: (Core.Let -> Core.Term)
letBody = Core.letBody

letWithBindings :: (Core.Let -> [Core.Binding] -> Core.Let)
letWithBindings original newVal = Core.Let {
  Core.letBindings = newVal,
  Core.letBody = (Core.letBody original)}

letWithBody :: (Core.Let -> Core.Term -> Core.Let)
letWithBody original newVal = Core.Let {
  Core.letBindings = (Core.letBindings original),
  Core.letBody = newVal}

literalBinary :: (B.ByteString -> Core.Literal)
literalBinary x = (Core.LiteralBinary x)

literalBoolean :: (Bool -> Core.Literal)
literalBoolean x = (Core.LiteralBoolean x)

literalFloat :: (Core.FloatValue -> Core.Literal)
literalFloat x = (Core.LiteralFloat x)

literalInteger :: (Core.IntegerValue -> Core.Literal)
literalInteger x = (Core.LiteralInteger x)

literalString :: (String -> Core.Literal)
literalString x = (Core.LiteralString x)

literalTypeBinary :: Core.LiteralType
literalTypeBinary = Core.LiteralTypeBinary

literalTypeBoolean :: Core.LiteralType
literalTypeBoolean = Core.LiteralTypeBoolean

literalTypeFloat :: (Core.FloatType -> Core.LiteralType)
literalTypeFloat x = (Core.LiteralTypeFloat x)

literalTypeInteger :: (Core.IntegerType -> Core.LiteralType)
literalTypeInteger x = (Core.LiteralTypeInteger x)

literalTypeString :: Core.LiteralType
literalTypeString = Core.LiteralTypeString

mapType :: (Core.Type -> Core.Type -> Core.MapType)
mapType keys values = Core.MapType {
  Core.mapTypeKeys = keys,
  Core.mapTypeValues = values}

mapTypeKeys :: (Core.MapType -> Core.Type)
mapTypeKeys = Core.mapTypeKeys

mapTypeValues :: (Core.MapType -> Core.Type)
mapTypeValues = Core.mapTypeValues

mapTypeWithKeys :: (Core.MapType -> Core.Type -> Core.MapType)
mapTypeWithKeys original newVal = Core.MapType {
  Core.mapTypeKeys = newVal,
  Core.mapTypeValues = (Core.mapTypeValues original)}

mapTypeWithValues :: (Core.MapType -> Core.Type -> Core.MapType)
mapTypeWithValues original newVal = Core.MapType {
  Core.mapTypeKeys = (Core.mapTypeKeys original),
  Core.mapTypeValues = newVal}

name :: (String -> Core.Name)
name x = (Core.Name x)

unName :: (Core.Name -> String)
unName = Core.unName

projection :: (Core.Name -> Core.Name -> Core.Projection)
projection typeName field = Core.Projection {
  Core.projectionTypeName = typeName,
  Core.projectionField = field}

projectionTypeName :: (Core.Projection -> Core.Name)
projectionTypeName = Core.projectionTypeName

projectionField :: (Core.Projection -> Core.Name)
projectionField = Core.projectionField

projectionWithTypeName :: (Core.Projection -> Core.Name -> Core.Projection)
projectionWithTypeName original newVal = Core.Projection {
  Core.projectionTypeName = newVal,
  Core.projectionField = (Core.projectionField original)}

projectionWithField :: (Core.Projection -> Core.Name -> Core.Projection)
projectionWithField original newVal = Core.Projection {
  Core.projectionTypeName = (Core.projectionTypeName original),
  Core.projectionField = newVal}

record :: (Core.Name -> [Core.Field] -> Core.Record)
record typeName fields = Core.Record {
  Core.recordTypeName = typeName,
  Core.recordFields = fields}

recordTypeName :: (Core.Record -> Core.Name)
recordTypeName = Core.recordTypeName

recordFields :: (Core.Record -> [Core.Field])
recordFields = Core.recordFields

recordWithTypeName :: (Core.Record -> Core.Name -> Core.Record)
recordWithTypeName original newVal = Core.Record {
  Core.recordTypeName = newVal,
  Core.recordFields = (Core.recordFields original)}

recordWithFields :: (Core.Record -> [Core.Field] -> Core.Record)
recordWithFields original newVal = Core.Record {
  Core.recordTypeName = (Core.recordTypeName original),
  Core.recordFields = newVal}

rowType :: (Core.Name -> [Core.FieldType] -> Core.RowType)
rowType typeName fields = Core.RowType {
  Core.rowTypeTypeName = typeName,
  Core.rowTypeFields = fields}

rowTypeTypeName :: (Core.RowType -> Core.Name)
rowTypeTypeName = Core.rowTypeTypeName

rowTypeFields :: (Core.RowType -> [Core.FieldType])
rowTypeFields = Core.rowTypeFields

rowTypeWithTypeName :: (Core.RowType -> Core.Name -> Core.RowType)
rowTypeWithTypeName original newVal = Core.RowType {
  Core.rowTypeTypeName = newVal,
  Core.rowTypeFields = (Core.rowTypeFields original)}

rowTypeWithFields :: (Core.RowType -> [Core.FieldType] -> Core.RowType)
rowTypeWithFields original newVal = Core.RowType {
  Core.rowTypeTypeName = (Core.rowTypeTypeName original),
  Core.rowTypeFields = newVal}

termAnnotated :: (Core.AnnotatedTerm -> Core.Term)
termAnnotated x = (Core.TermAnnotated x)

termApplication :: (Core.Application -> Core.Term)
termApplication x = (Core.TermApplication x)

termEither :: (Either Core.Term Core.Term -> Core.Term)
termEither x = (Core.TermEither x)

termFunction :: (Core.Function -> Core.Term)
termFunction x = (Core.TermFunction x)

termLet :: (Core.Let -> Core.Term)
termLet x = (Core.TermLet x)

termList :: ([Core.Term] -> Core.Term)
termList x = (Core.TermList x)

termLiteral :: (Core.Literal -> Core.Term)
termLiteral x = (Core.TermLiteral x)

termMap :: (M.Map Core.Term Core.Term -> Core.Term)
termMap x = (Core.TermMap x)

termMaybe :: (Maybe Core.Term -> Core.Term)
termMaybe x = (Core.TermMaybe x)

termPair :: ((Core.Term, Core.Term) -> Core.Term)
termPair x = (Core.TermPair x)

termRecord :: (Core.Record -> Core.Term)
termRecord x = (Core.TermRecord x)

termSet :: (S.Set Core.Term -> Core.Term)
termSet x = (Core.TermSet x)

termTypeApplication :: (Core.TypeApplicationTerm -> Core.Term)
termTypeApplication x = (Core.TermTypeApplication x)

termTypeLambda :: (Core.TypeLambda -> Core.Term)
termTypeLambda x = (Core.TermTypeLambda x)

termUnion :: (Core.Injection -> Core.Term)
termUnion x = (Core.TermUnion x)

termUnit :: Core.Term
termUnit = Core.TermUnit

termVariable :: (Core.Name -> Core.Term)
termVariable x = (Core.TermVariable x)

termWrap :: (Core.WrappedTerm -> Core.Term)
termWrap x = (Core.TermWrap x)

typeAnnotated :: (Core.AnnotatedType -> Core.Type)
typeAnnotated x = (Core.TypeAnnotated x)

typeApplication :: (Core.ApplicationType -> Core.Type)
typeApplication x = (Core.TypeApplication x)

typeEither :: (Core.EitherType -> Core.Type)
typeEither x = (Core.TypeEither x)

typeForall :: (Core.ForallType -> Core.Type)
typeForall x = (Core.TypeForall x)

typeFunction :: (Core.FunctionType -> Core.Type)
typeFunction x = (Core.TypeFunction x)

typeList :: (Core.Type -> Core.Type)
typeList x = (Core.TypeList x)

typeLiteral :: (Core.LiteralType -> Core.Type)
typeLiteral x = (Core.TypeLiteral x)

typeMap :: (Core.MapType -> Core.Type)
typeMap x = (Core.TypeMap x)

typeMaybe :: (Core.Type -> Core.Type)
typeMaybe x = (Core.TypeMaybe x)

typePair :: (Core.PairType -> Core.Type)
typePair x = (Core.TypePair x)

typeRecord :: (Core.RowType -> Core.Type)
typeRecord x = (Core.TypeRecord x)

typeSet :: (Core.Type -> Core.Type)
typeSet x = (Core.TypeSet x)

typeUnion :: (Core.RowType -> Core.Type)
typeUnion x = (Core.TypeUnion x)

typeUnit :: Core.Type
typeUnit = Core.TypeUnit

typeVariable :: (Core.Name -> Core.Type)
typeVariable x = (Core.TypeVariable x)

typeWrap :: (Core.WrappedType -> Core.Type)
typeWrap x = (Core.TypeWrap x)

typeApplicationTerm :: (Core.Term -> Core.Type -> Core.TypeApplicationTerm)
typeApplicationTerm body type_ = Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = body,
  Core.typeApplicationTermType = type_}

typeApplicationTermBody :: (Core.TypeApplicationTerm -> Core.Term)
typeApplicationTermBody = Core.typeApplicationTermBody

typeApplicationTermType :: (Core.TypeApplicationTerm -> Core.Type)
typeApplicationTermType = Core.typeApplicationTermType

typeApplicationTermWithBody :: (Core.TypeApplicationTerm -> Core.Term -> Core.TypeApplicationTerm)
typeApplicationTermWithBody original newVal = Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = newVal,
  Core.typeApplicationTermType = (Core.typeApplicationTermType original)}

typeApplicationTermWithType :: (Core.TypeApplicationTerm -> Core.Type -> Core.TypeApplicationTerm)
typeApplicationTermWithType original newVal = Core.TypeApplicationTerm {
  Core.typeApplicationTermBody = (Core.typeApplicationTermBody original),
  Core.typeApplicationTermType = newVal}

typeLambda :: (Core.Name -> Core.Term -> Core.TypeLambda)
typeLambda parameter body = Core.TypeLambda {
  Core.typeLambdaParameter = parameter,
  Core.typeLambdaBody = body}

typeLambdaParameter :: (Core.TypeLambda -> Core.Name)
typeLambdaParameter = Core.typeLambdaParameter

typeLambdaBody :: (Core.TypeLambda -> Core.Term)
typeLambdaBody = Core.typeLambdaBody

typeLambdaWithParameter :: (Core.TypeLambda -> Core.Name -> Core.TypeLambda)
typeLambdaWithParameter original newVal = Core.TypeLambda {
  Core.typeLambdaParameter = newVal,
  Core.typeLambdaBody = (Core.typeLambdaBody original)}

typeLambdaWithBody :: (Core.TypeLambda -> Core.Term -> Core.TypeLambda)
typeLambdaWithBody original newVal = Core.TypeLambda {
  Core.typeLambdaParameter = (Core.typeLambdaParameter original),
  Core.typeLambdaBody = newVal}

typeScheme :: ([Core.Name] -> Core.Type -> Maybe (M.Map Core.Name Core.TypeVariableMetadata) -> Core.TypeScheme)
typeScheme variables type_ constraints = Core.TypeScheme {
  Core.typeSchemeVariables = variables,
  Core.typeSchemeType = type_,
  Core.typeSchemeConstraints = constraints}

typeSchemeVariables :: (Core.TypeScheme -> [Core.Name])
typeSchemeVariables = Core.typeSchemeVariables

typeSchemeType :: (Core.TypeScheme -> Core.Type)
typeSchemeType = Core.typeSchemeType

typeSchemeConstraints :: (Core.TypeScheme -> Maybe (M.Map Core.Name Core.TypeVariableMetadata))
typeSchemeConstraints = Core.typeSchemeConstraints

typeSchemeWithVariables :: (Core.TypeScheme -> [Core.Name] -> Core.TypeScheme)
typeSchemeWithVariables original newVal = Core.TypeScheme {
  Core.typeSchemeVariables = newVal,
  Core.typeSchemeType = (Core.typeSchemeType original),
  Core.typeSchemeConstraints = (Core.typeSchemeConstraints original)}

typeSchemeWithType :: (Core.TypeScheme -> Core.Type -> Core.TypeScheme)
typeSchemeWithType original newVal = Core.TypeScheme {
  Core.typeSchemeVariables = (Core.typeSchemeVariables original),
  Core.typeSchemeType = newVal,
  Core.typeSchemeConstraints = (Core.typeSchemeConstraints original)}

typeSchemeWithConstraints :: (Core.TypeScheme -> Maybe (M.Map Core.Name Core.TypeVariableMetadata) -> Core.TypeScheme)
typeSchemeWithConstraints original newVal = Core.TypeScheme {
  Core.typeSchemeVariables = (Core.typeSchemeVariables original),
  Core.typeSchemeType = (Core.typeSchemeType original),
  Core.typeSchemeConstraints = newVal}

typeVariableMetadata :: (S.Set Core.Name -> Core.TypeVariableMetadata)
typeVariableMetadata classes = Core.TypeVariableMetadata {
  Core.typeVariableMetadataClasses = classes}

typeVariableMetadataClasses :: (Core.TypeVariableMetadata -> S.Set Core.Name)
typeVariableMetadataClasses = Core.typeVariableMetadataClasses

typeVariableMetadataWithClasses :: (t0 -> S.Set Core.Name -> Core.TypeVariableMetadata)
typeVariableMetadataWithClasses original newVal = Core.TypeVariableMetadata {
  Core.typeVariableMetadataClasses = newVal}

wrappedTerm :: (Core.Name -> Core.Term -> Core.WrappedTerm)
wrappedTerm typeName body = Core.WrappedTerm {
  Core.wrappedTermTypeName = typeName,
  Core.wrappedTermBody = body}

wrappedTermTypeName :: (Core.WrappedTerm -> Core.Name)
wrappedTermTypeName = Core.wrappedTermTypeName

wrappedTermBody :: (Core.WrappedTerm -> Core.Term)
wrappedTermBody = Core.wrappedTermBody

wrappedTermWithTypeName :: (Core.WrappedTerm -> Core.Name -> Core.WrappedTerm)
wrappedTermWithTypeName original newVal = Core.WrappedTerm {
  Core.wrappedTermTypeName = newVal,
  Core.wrappedTermBody = (Core.wrappedTermBody original)}

wrappedTermWithBody :: (Core.WrappedTerm -> Core.Term -> Core.WrappedTerm)
wrappedTermWithBody original newVal = Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.wrappedTermTypeName original),
  Core.wrappedTermBody = newVal}

wrappedType :: (Core.Name -> Core.Type -> Core.WrappedType)
wrappedType typeName body = Core.WrappedType {
  Core.wrappedTypeTypeName = typeName,
  Core.wrappedTypeBody = body}

wrappedTypeTypeName :: (Core.WrappedType -> Core.Name)
wrappedTypeTypeName = Core.wrappedTypeTypeName

wrappedTypeBody :: (Core.WrappedType -> Core.Type)
wrappedTypeBody = Core.wrappedTypeBody

wrappedTypeWithTypeName :: (Core.WrappedType -> Core.Name -> Core.WrappedType)
wrappedTypeWithTypeName original newVal = Core.WrappedType {
  Core.wrappedTypeTypeName = newVal,
  Core.wrappedTypeBody = (Core.wrappedTypeBody original)}

wrappedTypeWithBody :: (Core.WrappedType -> Core.Type -> Core.WrappedType)
wrappedTypeWithBody original newVal = Core.WrappedType {
  Core.wrappedTypeTypeName = (Core.wrappedTypeTypeName original),
  Core.wrappedTypeBody = newVal}
