module Hydra.Sources.Sql.Ansi where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: Namespace
ns = Namespace "hydra.sql.syntax"

define :: String -> Type -> Binding
define = defineType ns

sql :: String -> Type
sql = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [] [] $
    Just ("A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at "
      ++ "https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on "
      ++ "the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003")
  where
    definitions = [
      approximateNumericLiteral,
      binaryStringLiteral,
      characterStringLiteral,
      columnName,
      dateString,
      domainName,
      exactNumericLiteral,
      leftBracketOrTrigraph,
      rightBracketOrTrigraph,
      nationalCharacterStringLiteral,
      pathResolvedUserDefinedTypeName,
      tableName,
      timeString,
      timestampLiteral,
      unicodeCharacterStringLiteral,
      unsignedInteger,
      approximateNumericType,
      arrayElement,
      arrayElementList,
      arrayElementReference,
      arrayType,
      arrayValueConstructor,
      arrayValueConstructorByQuery,
      arrayValueConstructorByEnumeration,
      arrayValueExpression,
      asSubqueryClause,
      attributeOrMethodReference,
      binaryLargeObjectStringType,
      booleanFactor,
      booleanLiteral,
      booleanPredicand,
      booleanPrimary,
      booleanTerm,
      booleanTerm_And,
      booleanTest,
      booleanTest_Sequence_Option,
      booleanType,
      booleanValueExpression,
      booleanValueExpression_Or,
      caseExpression,
      castSpecification,
      characterSetSpecification,
      characterStringType,
      collateClause,
      collectionType,
      collectionValueConstructor,
      collectionValueExpression,
      columnConstraintDefinition,
      columnDefinition,
      columnDefinition_TypeOrDomain_Option,
      columnDefinition_DefaultOrIdentityOrGeneration_Option,
      columnNameList,
      columnOptions,
      columnReference,
      commonValueExpression,
      contextuallyTypedRowValueExpression,
      contextuallyTypedRowValueConstructor,
      contextuallyTypedRowValueExpressionList,
      contextuallyTypedTableValueConstructor,
      dataType,
      dateLiteral,
      datetimeLiteral,
      datetimeType,
      datetimeValueExpression,
      defaultClause,
      exactNumericType,
      exactNumericType_Numeric_Option,
      exactNumericType_Decimal_Option,
      exactNumericType_Dec_Option,
      fieldReference,
      fromConstructor,
      fromDefault,
      fromSubquery,
      generalLiteral,
      generalValueSpecification,
      generationClause,
      globalOrLocal,
      identityColumnSpecification,
      insertColumnList,
      insertColumnsAndSource,
      insertStatement,
      insertionTarget,
      intervalLiteral,
      intervalType,
      intervalValueExpression,
      largeObjectLength,
      length_,
      likeClause,
      methodInvocation,
      multisetElementReference,
      multisetType,
      multisetValueConstructor,
      multisetValueExpression,
      nationalCharacterStringType_,
      newSpecification,
      nextValueExpression,
      numericType,
      numericValueExpression,
      overrideClause,
      parenthesizedValueExpression,
      precision,
      predefinedType,
      predefinedType_String,
      predefinedType_NationalString,
      predicate,
      queryExpression,
      referenceScopeCheck,
      referenceType,
      rowType,
      rowValueSpecialCase,
      nonparenthesizedValueExpressionPrimary,
      referenceResolution,
      referenceValueExpression,
      rowValueExpression,
      routineInvocation,
      scalarSubquery,
      scale,
      selfReferencingColumnSpecification,
      setFunctionSpecification,
      staticMethodInvocation,
      stringValueExpression,
      subquery,
      subtableClause,
      subtypeTreatment,
      tableCommitAction,
      tableConstraintDefinition,
      tableContentsSource,
      tableContentsSource_Subtable,
      tableDefinition,
      tableElement,
      tableElementList,
      tableScope,
      timeLiteral,
      truthValue,
      unsignedLiteral,
      unsignedNumericLiteral,
      unsignedValueSpecification,
      userDefinedTypeValueExpression,
      valueExpression,
      valueExpressionPrimary,
      windowFunction]

-- Token definitions

approximateNumericLiteral :: Binding
approximateNumericLiteral = define "ApproximateNumericLiteral" $ T.wrap T.string

binaryStringLiteral :: Binding
binaryStringLiteral = define "BinaryStringLiteral" $ T.wrap T.unit

characterStringLiteral :: Binding
characterStringLiteral = define "CharacterStringLiteral" $ T.wrap T.string

columnName :: Binding
columnName = define "ColumnName" $ T.wrap T.string

dateString :: Binding
dateString = define "DateString" $ T.wrap T.unit

domainName :: Binding
domainName = define "DomainName" $ T.wrap T.string

exactNumericLiteral :: Binding
exactNumericLiteral = define "ExactNumericLiteral" $ T.wrap T.string

leftBracketOrTrigraph :: Binding
leftBracketOrTrigraph = define "LeftBracketOrTrigraph" $ T.wrap T.string

rightBracketOrTrigraph :: Binding
rightBracketOrTrigraph = define "RightBracketOrTrigraph" $ T.wrap T.string

nationalCharacterStringLiteral :: Binding
nationalCharacterStringLiteral = define "NationalCharacterStringLiteral" $ T.wrap T.unit

pathResolvedUserDefinedTypeName :: Binding
pathResolvedUserDefinedTypeName = define "PathResolvedUserDefinedTypeName" $ T.wrap T.string

tableName :: Binding
tableName = define "TableName" $ T.wrap T.string

timeString :: Binding
timeString = define "TimeString" $ T.wrap T.unit

timestampLiteral :: Binding
timestampLiteral = define "TimestampLiteral" $ T.wrap T.unit

unicodeCharacterStringLiteral :: Binding
unicodeCharacterStringLiteral = define "UnicodeCharacterStringLiteral" $ T.wrap T.unit

unsignedInteger :: Binding
unsignedInteger = define "UnsignedInteger" $ T.wrap T.string

-- Type definitions

approximateNumericType :: Binding
approximateNumericType = define "ApproximateNumericType" $
  T.union [
    "float">: T.maybe (sql "Precision"),
    "real">: T.unit,
    "double">: T.unit]

arrayElement :: Binding
arrayElement = define "ArrayElement" $ T.wrap $ sql "ValueExpression"

arrayElementList :: Binding
arrayElementList = define "ArrayElementList" $
  T.record [
    "first">: sql "ArrayElement",
    "rest">: T.list (sql "ArrayElement")]

arrayElementReference :: Binding
arrayElementReference = define "ArrayElementReference" $ T.wrap T.unit

arrayType :: Binding
arrayType = define "ArrayType" $ T.wrap T.unit

arrayValueConstructor :: Binding
arrayValueConstructor = define "ArrayValueConstructor" $
  T.union [
    "enumeration">: sql "ArrayValueConstructorByEnumeration",
    "query">: sql "ArrayValueConstructorByQuery"]

arrayValueConstructorByQuery :: Binding
arrayValueConstructorByQuery = define "ArrayValueConstructorByQuery" $ T.wrap T.unit

arrayValueConstructorByEnumeration :: Binding
arrayValueConstructorByEnumeration = define "ArrayValueConstructorByEnumeration" $
  T.record [
    "LeftBracketOrTrigraph">: sql "LeftBracketOrTrigraph",
    "ArrayElementList">: sql "ArrayElementList",
    "RightBracketOrTrigraph">: sql "RightBracketOrTrigraph"]

arrayValueExpression :: Binding
arrayValueExpression = define "ArrayValueExpression" $ T.wrap T.unit

asSubqueryClause :: Binding
asSubqueryClause = define "AsSubqueryClause" $ T.wrap T.unit

attributeOrMethodReference :: Binding
attributeOrMethodReference = define "AttributeOrMethodReference" $ T.wrap T.unit

binaryLargeObjectStringType :: Binding
binaryLargeObjectStringType = define "BinaryLargeObjectStringType" $
  T.union [
    "binary">: T.maybe (sql "LargeObjectLength"),
    "blob">: T.maybe (sql "LargeObjectLength")]

booleanFactor :: Binding
booleanFactor = define "BooleanFactor" $
  T.record [
    "NOT">: T.maybe T.unit,
    "BooleanTest">: sql "BooleanTest"]

booleanLiteral :: Binding
booleanLiteral = define "BooleanLiteral" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

booleanPredicand :: Binding
booleanPredicand = define "BooleanPredicand" $ T.wrap T.unit

booleanPrimary :: Binding
booleanPrimary = define "BooleanPrimary" $
  T.union [
    "predicate">: sql "Predicate",
    "predicand">: sql "BooleanPredicand"]

booleanTerm :: Binding
booleanTerm = define "BooleanTerm" $
  T.union [
    "factor">: sql "BooleanFactor",
    "and">: sql "BooleanTerm_And"]

booleanTerm_And :: Binding
booleanTerm_And = define "BooleanTerm_And" $
  T.record [
    "lhs">: sql "BooleanTerm",
    "rhs">: sql "BooleanFactor"]

booleanTest :: Binding
booleanTest = define "BooleanTest" $
  T.record [
    "BooleanPrimary">: sql "BooleanPrimary",
    "Sequence">: T.maybe (sql "BooleanTest_Sequence_Option")]

booleanTest_Sequence_Option :: Binding
booleanTest_Sequence_Option = define "BooleanTest_Sequence_Option" $
  T.record [
    "NOT">: T.maybe T.unit,
    "TruthValue">: sql "TruthValue"]

booleanType :: Binding
booleanType = define "BooleanType" $ T.wrap T.unit

booleanValueExpression :: Binding
booleanValueExpression = define "BooleanValueExpression" $
  T.union [
    "term">: sql "BooleanTerm",
    "or">: sql "BooleanValueExpression_Or"]

booleanValueExpression_Or :: Binding
booleanValueExpression_Or = define "BooleanValueExpression_Or" $
  T.record [
    "lhs">: sql "BooleanValueExpression",
    "rhs">: sql "BooleanTerm"]

caseExpression :: Binding
caseExpression = define "CaseExpression" $ T.wrap T.unit

castSpecification :: Binding
castSpecification = define "CastSpecification" $ T.wrap T.unit

characterSetSpecification :: Binding
characterSetSpecification = define "CharacterSetSpecification" $ T.wrap T.unit

characterStringType :: Binding
characterStringType = define "CharacterStringType" $
  T.union [
    "character">: T.maybe (sql "Length"),
    "char">: T.maybe (sql "Length"),
    "characterVarying">: sql "Length",
    "charVarying">: sql "Length",
    "varchar">: sql "Length",
    "characterLargeObject">: T.maybe (sql "LargeObjectLength"),
    "charLargeObject">: T.maybe (sql "LargeObjectLength"),
    "clob">: T.maybe (sql "LargeObjectLength")]

collateClause :: Binding
collateClause = define "CollateClause" $ T.wrap T.unit

collectionType :: Binding
collectionType = define "CollectionType" $
  T.union [
    "array">: sql "ArrayType",
    "multiset">: sql "MultisetType"]

collectionValueConstructor :: Binding
collectionValueConstructor = define "CollectionValueConstructor" $
  T.union [
    "array">: sql "ArrayValueConstructor",
    "multiset">: sql "MultisetValueConstructor"]

collectionValueExpression :: Binding
collectionValueExpression = define "CollectionValueExpression" $
  T.union [
    "array">: sql "ArrayValueExpression",
    "multiset">: sql "MultisetValueExpression"]

columnConstraintDefinition :: Binding
columnConstraintDefinition = define "ColumnConstraintDefinition" $ T.wrap T.unit

columnDefinition :: Binding
columnDefinition = define "ColumnDefinition" $
  T.record [
    "name">: sql "ColumnName",
    "typeOrDomain">: T.maybe (sql "ColumnDefinition_TypeOrDomain_Option"),
    "refScope">: T.maybe (sql "ReferenceScopeCheck"),
    "defaultOrIdentityOrGeneration">: T.maybe (sql "ColumnDefinition_DefaultOrIdentityOrGeneration_Option"),
    "constraints">: T.list (sql "ColumnConstraintDefinition"),
    "collate">: T.maybe (sql "CollateClause")]

columnDefinition_TypeOrDomain_Option :: Binding
columnDefinition_TypeOrDomain_Option = define "ColumnDefinition_TypeOrDomain_Option" $
  T.union [
    "DataType">: sql "DataType",
    "DomainName">: sql "DomainName"]

columnDefinition_DefaultOrIdentityOrGeneration_Option :: Binding
columnDefinition_DefaultOrIdentityOrGeneration_Option = define "ColumnDefinition_DefaultOrIdentityOrGeneration_Option" $
  T.union [
    "DefaultClause">: sql "DefaultClause",
    "IdentityColumnSpecification">: sql "IdentityColumnSpecification",
    "GenerationClause">: sql "GenerationClause"]

columnNameList :: Binding
columnNameList = define "ColumnNameList" $
  T.record [
    "first">: sql "ColumnName",
    "rest">: T.list (sql "ColumnName")]

columnOptions :: Binding
columnOptions = define "ColumnOptions" $ T.wrap T.unit

columnReference :: Binding
columnReference = define "ColumnReference" $ T.wrap T.unit

commonValueExpression :: Binding
commonValueExpression = define "CommonValueExpression" $
  T.union [
    "numeric">: sql "NumericValueExpression",
    "string">: sql "StringValueExpression",
    "datetime">: sql "DatetimeValueExpression",
    "interval">: sql "IntervalValueExpression",
    "userDefined">: sql "UserDefinedTypeValueExpression",
    "reference">: sql "ReferenceValueExpression",
    "collection">: sql "CollectionValueExpression"]

contextuallyTypedRowValueExpression :: Binding
contextuallyTypedRowValueExpression = define "ContextuallyTypedRowValueExpression" $
  T.union [
    "specialCase">: sql "RowValueSpecialCase",
    "constructor">: sql "ContextuallyTypedRowValueConstructor"]

contextuallyTypedRowValueConstructor :: Binding
contextuallyTypedRowValueConstructor = define "ContextuallyTypedRowValueConstructor" $ T.wrap T.unit

contextuallyTypedRowValueExpressionList :: Binding
contextuallyTypedRowValueExpressionList = define "ContextuallyTypedRowValueExpressionList" $
  T.record [
    "first">: sql "ContextuallyTypedRowValueExpression",
    "rest">: T.list (sql "ContextuallyTypedRowValueExpression")]

contextuallyTypedTableValueConstructor :: Binding
contextuallyTypedTableValueConstructor = define "ContextuallyTypedTableValueConstructor" $
  T.wrap $ sql "ContextuallyTypedRowValueExpressionList"

dataType :: Binding
dataType = define "DataType" $
  T.union [
    "predefined">: sql "PredefinedType",
    "row">: sql "RowType",
    "named">: sql "PathResolvedUserDefinedTypeName",
    "reference">: sql "ReferenceType",
    "collection">: sql "CollectionType"]

dateLiteral :: Binding
dateLiteral = define "DateLiteral" $ T.wrap $ sql "DateString"

datetimeLiteral :: Binding
datetimeLiteral = define "DatetimeLiteral" $
  T.union [
    "date">: sql "DateLiteral",
    "time">: sql "TimeLiteral",
    "timestamp">: sql "TimestampLiteral"]

datetimeType :: Binding
datetimeType = define "DatetimeType" $ T.wrap T.unit

datetimeValueExpression :: Binding
datetimeValueExpression = define "DatetimeValueExpression" $ T.wrap T.unit

defaultClause :: Binding
defaultClause = define "DefaultClause" $ T.wrap T.unit

exactNumericType :: Binding
exactNumericType = define "ExactNumericType" $
  T.union [
    "numeric">: T.maybe (sql "ExactNumericType_Numeric_Option"),
    "decimal">: T.maybe (sql "ExactNumericType_Decimal_Option"),
    "dec">: T.maybe (sql "ExactNumericType_Dec_Option"),
    "smallint">: T.unit,
    "integer">: T.unit,
    "int">: T.unit,
    "bigint">: T.unit]

exactNumericType_Numeric_Option :: Binding
exactNumericType_Numeric_Option = define "ExactNumericType_Numeric_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.maybe (sql "Scale")]

exactNumericType_Decimal_Option :: Binding
exactNumericType_Decimal_Option = define "ExactNumericType_Decimal_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.maybe (sql "Scale")]

exactNumericType_Dec_Option :: Binding
exactNumericType_Dec_Option = define "ExactNumericType_Dec_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.maybe (sql "Scale")]

fieldReference :: Binding
fieldReference = define "FieldReference" $ T.wrap T.unit

fromConstructor :: Binding
fromConstructor = define "FromConstructor" $
  T.record [
    "columns">: T.maybe (sql "InsertColumnList"),
    "override">: T.maybe (sql "OverrideClause"),
    "values">: sql "ContextuallyTypedTableValueConstructor"]

fromDefault :: Binding
fromDefault = define "FromDefault" $ T.wrap T.unit

fromSubquery :: Binding
fromSubquery = define "FromSubquery" $ T.wrap T.unit

generalLiteral :: Binding
generalLiteral = define "GeneralLiteral" $
  T.union [
    "string">: sql "CharacterStringLiteral",
    "nationalString">: sql "NationalCharacterStringLiteral",
    "unicode">: sql "UnicodeCharacterStringLiteral",
    "binary">: sql "BinaryStringLiteral",
    "dateTime">: sql "DatetimeLiteral",
    "interval">: sql "IntervalLiteral",
    "boolean">: sql "BooleanLiteral"]

generalValueSpecification :: Binding
generalValueSpecification = define "GeneralValueSpecification" $ T.wrap T.unit

generationClause :: Binding
generationClause = define "GenerationClause" $ T.wrap T.unit

globalOrLocal :: Binding
globalOrLocal = define "GlobalOrLocal" $
  T.union [
    "global">: T.unit,
    "local">: T.unit]

identityColumnSpecification :: Binding
identityColumnSpecification = define "IdentityColumnSpecification" $ T.wrap T.unit

insertColumnList :: Binding
insertColumnList = define "InsertColumnList" $ T.wrap $ sql "ColumnNameList"

insertColumnsAndSource :: Binding
insertColumnsAndSource = define "InsertColumnsAndSource" $
  T.union [
    "subquery">: sql "FromSubquery",
    "constructor">: sql "FromConstructor",
    "default">: sql "FromDefault"]

insertStatement :: Binding
insertStatement = define "InsertStatement" $
  T.record [
    "target">: sql "InsertionTarget",
    "columnsAndSource">: sql "InsertColumnsAndSource"]

insertionTarget :: Binding
insertionTarget = define "InsertionTarget" $ T.wrap $ sql "TableName"

intervalLiteral :: Binding
intervalLiteral = define "IntervalLiteral" $ T.wrap T.unit

intervalType :: Binding
intervalType = define "IntervalType" $ T.wrap T.unit

intervalValueExpression :: Binding
intervalValueExpression = define "IntervalValueExpression" $ T.wrap T.unit

largeObjectLength :: Binding
largeObjectLength = define "LargeObjectLength" $ T.wrap T.unit

length_ :: Binding
length_ = define "Length" $ T.wrap $ sql "UnsignedInteger"

likeClause :: Binding
likeClause = define "LikeClause" $ T.wrap T.unit

methodInvocation :: Binding
methodInvocation = define "MethodInvocation" $ T.wrap T.unit

multisetElementReference :: Binding
multisetElementReference = define "MultisetElementReference" $ T.wrap T.unit

multisetType :: Binding
multisetType = define "MultisetType" $ T.wrap $ sql "DataType"

multisetValueConstructor :: Binding
multisetValueConstructor = define "MultisetValueConstructor" $ T.wrap T.unit

multisetValueExpression :: Binding
multisetValueExpression = define "MultisetValueExpression" $ T.wrap T.unit

nationalCharacterStringType_ :: Binding
nationalCharacterStringType_ = define "NationalCharacterStringType" $ T.wrap T.unit

newSpecification :: Binding
newSpecification = define "NewSpecification" $ T.wrap T.unit

nextValueExpression :: Binding
nextValueExpression = define "NextValueExpression" $ T.wrap T.unit

numericType :: Binding
numericType = define "NumericType" $
  T.union [
    "exact">: sql "ExactNumericType",
    "approximate">: sql "ApproximateNumericType"]

numericValueExpression :: Binding
numericValueExpression = define "NumericValueExpression" $ T.wrap T.unit

overrideClause :: Binding
overrideClause = define "OverrideClause" $
  T.union [
    "OVERRIDINGspUSERspVALUE">: T.unit,
    "OVERRIDINGspSYSTEMspVALUE">: T.unit]

parenthesizedValueExpression :: Binding
parenthesizedValueExpression = define "ParenthesizedValueExpression" $
  T.wrap $ sql "ValueExpression"

precision :: Binding
precision = define "Precision" $ T.wrap $ sql "UnsignedInteger"

predefinedType :: Binding
predefinedType = define "PredefinedType" $
  T.union [
    "string">: sql "PredefinedType_String",
    "nationalString">: sql "PredefinedType_NationalString",
    "blob">: sql "BinaryLargeObjectStringType",
    "numeric">: sql "NumericType",
    "boolean">: sql "BooleanType",
    "datetime">: sql "DatetimeType",
    "interval">: sql "IntervalType"]

predefinedType_String :: Binding
predefinedType_String = define "PredefinedType_String" $
  T.record [
    "type">: sql "CharacterStringType",
    "characters">: T.maybe (sql "CharacterSetSpecification"),
    "collate">: T.maybe (sql "CollateClause")]

predefinedType_NationalString :: Binding
predefinedType_NationalString = define "PredefinedType_NationalString" $
  T.record [
    "type">: sql "NationalCharacterStringType",
    "collate">: T.maybe (sql "CollateClause")]

predicate :: Binding
predicate = define "Predicate" $ T.wrap T.unit

queryExpression :: Binding
queryExpression = define "QueryExpression" $ T.wrap T.unit

referenceScopeCheck :: Binding
referenceScopeCheck = define "ReferenceScopeCheck" $ T.wrap T.unit

referenceType :: Binding
referenceType = define "ReferenceType" $ T.wrap T.unit

rowType :: Binding
rowType = define "RowType" $ T.wrap T.unit

rowValueSpecialCase :: Binding
rowValueSpecialCase = define "RowValueSpecialCase" $
  T.wrap $ sql "NonparenthesizedValueExpressionPrimary"

nonparenthesizedValueExpressionPrimary :: Binding
nonparenthesizedValueExpressionPrimary = define "NonparenthesizedValueExpressionPrimary" $
  T.union [
    "unsigned">: sql "UnsignedValueSpecification",
    "column">: sql "ColumnReference",
    "setFunction">: sql "SetFunctionSpecification",
    "windowFunction">: sql "WindowFunction",
    "scalarSubquery">: sql "ScalarSubquery",
    "cases">: sql "CaseExpression",
    "cast">: sql "CastSpecification",
    "field">: sql "FieldReference",
    "subtype">: sql "SubtypeTreatment",
    "method">: sql "MethodInvocation",
    "staticMethod">: sql "StaticMethodInvocation",
    "new">: sql "NewSpecification",
    "attributeOrMethod">: sql "AttributeOrMethodReference",
    "reference">: sql "ReferenceResolution",
    "collection">: sql "CollectionValueConstructor",
    "arrayElement">: sql "ArrayElementReference",
    "multisetElement">: sql "MultisetElementReference",
    "routine">: sql "RoutineInvocation",
    "next">: sql "NextValueExpression"]

referenceResolution :: Binding
referenceResolution = define "ReferenceResolution" $ T.wrap T.unit

referenceValueExpression :: Binding
referenceValueExpression = define "ReferenceValueExpression" $
  T.wrap $ sql "ValueExpressionPrimary"

rowValueExpression :: Binding
rowValueExpression = define "RowValueExpression" $ T.wrap T.unit

routineInvocation :: Binding
routineInvocation = define "RoutineInvocation" $ T.wrap T.unit

scalarSubquery :: Binding
scalarSubquery = define "ScalarSubquery" $ T.wrap $ sql "Subquery"

scale :: Binding
scale = define "Scale" $ T.wrap $ sql "UnsignedInteger"

selfReferencingColumnSpecification :: Binding
selfReferencingColumnSpecification = define "SelfReferencingColumnSpecification" $ T.wrap T.unit

setFunctionSpecification :: Binding
setFunctionSpecification = define "SetFunctionSpecification" $ T.wrap T.unit

staticMethodInvocation :: Binding
staticMethodInvocation = define "StaticMethodInvocation" $ T.wrap T.unit

stringValueExpression :: Binding
stringValueExpression = define "StringValueExpression" $ T.wrap T.unit

subquery :: Binding
subquery = define "Subquery" $ T.wrap $ sql "QueryExpression"

subtableClause :: Binding
subtableClause = define "SubtableClause" $ T.wrap T.unit

subtypeTreatment :: Binding
subtypeTreatment = define "SubtypeTreatment" $ T.wrap T.unit

tableCommitAction :: Binding
tableCommitAction = define "TableCommitAction" $
  T.union [
    "preserve">: T.unit,
    "delete">: T.unit]

tableConstraintDefinition :: Binding
tableConstraintDefinition = define "TableConstraintDefinition" $ T.wrap T.unit

tableContentsSource :: Binding
tableContentsSource = define "TableContentsSource" $
  T.union [
    "list">: sql "TableElementList",
    "subtable">: sql "TableContentsSource_Subtable",
    "subquery">: sql "AsSubqueryClause"]

tableContentsSource_Subtable :: Binding
tableContentsSource_Subtable = define "TableContentsSource_Subtable" $
  T.record [
    "type">: sql "PathResolvedUserDefinedTypeName",
    "subtable">: T.maybe (sql "SubtableClause"),
    "elements">: T.maybe (sql "TableElementList")]

tableDefinition :: Binding
tableDefinition = define "TableDefinition" $
  T.record [
    "scope">: T.maybe (sql "TableScope"),
    "name">: sql "TableName",
    "source">: sql "TableContentsSource",
    "commitActions">: T.maybe (sql "TableCommitAction")]

tableElement :: Binding
tableElement = define "TableElement" $
  T.union [
    "column">: sql "ColumnDefinition",
    "tableConstraint">: sql "TableConstraintDefinition",
    "like">: sql "LikeClause",
    "selfReferencingColumn">: sql "SelfReferencingColumnSpecification",
    "columOptions">: sql "ColumnOptions"]

tableElementList :: Binding
tableElementList = define "TableElementList" $
  T.record [
    "first">: sql "TableElement",
    "rest">: T.list (sql "TableElement")]

tableScope :: Binding
tableScope = define "TableScope" $ T.wrap $ sql "GlobalOrLocal"

timeLiteral :: Binding
timeLiteral = define "TimeLiteral" $ T.wrap $ sql "TimeString"

truthValue :: Binding
truthValue = define "TruthValue" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

unsignedLiteral :: Binding
unsignedLiteral = define "UnsignedLiteral" $
  T.union [
    "numeric">: sql "UnsignedNumericLiteral",
    "general">: sql "GeneralLiteral"]

unsignedNumericLiteral :: Binding
unsignedNumericLiteral = define "UnsignedNumericLiteral" $
  T.union [
    "exact">: sql "ExactNumericLiteral",
    "approximate">: sql "ApproximateNumericLiteral"]

unsignedValueSpecification :: Binding
unsignedValueSpecification = define "UnsignedValueSpecification" $
  T.union [
    "literal">: sql "UnsignedLiteral",
    "general">: sql "GeneralValueSpecification"]

userDefinedTypeValueExpression :: Binding
userDefinedTypeValueExpression = define "UserDefinedTypeValueExpression" $
  T.wrap $ sql "ValueExpressionPrimary"

valueExpression :: Binding
valueExpression = define "ValueExpression" $
  T.union [
    "common">: sql "CommonValueExpression",
    "boolean">: sql "BooleanValueExpression",
    "row">: sql "RowValueExpression"]

valueExpressionPrimary :: Binding
valueExpressionPrimary = define "ValueExpressionPrimary" $
  T.union [
    "parens">: sql "ParenthesizedValueExpression",
    "noparens">: sql "NonparenthesizedValueExpressionPrimary"]

windowFunction :: Binding
windowFunction = define "WindowFunction" $ T.wrap T.unit
