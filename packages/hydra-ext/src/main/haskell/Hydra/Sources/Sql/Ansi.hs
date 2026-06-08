module Hydra.Sources.Sql.Ansi where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T


ns :: ModuleName
ns = ModuleName "hydra.sql.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [],
            moduleMetadata = descriptionMetadata (Just ("A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at "
      ++ "https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on "
      ++ "the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003"))}
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

sql :: String -> Type
sql = typeref ns

-- Token definitions

approximateNumericLiteral :: TypeDefinition
approximateNumericLiteral = define "ApproximateNumericLiteral" $ T.wrap T.string

binaryStringLiteral :: TypeDefinition
binaryStringLiteral = define "BinaryStringLiteral" $ T.wrap T.unit

characterStringLiteral :: TypeDefinition
characterStringLiteral = define "CharacterStringLiteral" $ T.wrap T.string

columnName :: TypeDefinition
columnName = define "ColumnName" $ T.wrap T.string

dateString :: TypeDefinition
dateString = define "DateString" $ T.wrap T.unit

domainName :: TypeDefinition
domainName = define "DomainName" $ T.wrap T.string

exactNumericLiteral :: TypeDefinition
exactNumericLiteral = define "ExactNumericLiteral" $ T.wrap T.string

leftBracketOrTrigraph :: TypeDefinition
leftBracketOrTrigraph = define "LeftBracketOrTrigraph" $ T.wrap T.string

nationalCharacterStringLiteral :: TypeDefinition
nationalCharacterStringLiteral = define "NationalCharacterStringLiteral" $ T.wrap T.unit

pathResolvedUserDefinedTypeName :: TypeDefinition
pathResolvedUserDefinedTypeName = define "PathResolvedUserDefinedTypeName" $ T.wrap T.string

rightBracketOrTrigraph :: TypeDefinition
rightBracketOrTrigraph = define "RightBracketOrTrigraph" $ T.wrap T.string

tableName :: TypeDefinition
tableName = define "TableName" $ T.wrap T.string

timeString :: TypeDefinition
timeString = define "TimeString" $ T.wrap T.unit

timestampLiteral :: TypeDefinition
timestampLiteral = define "TimestampLiteral" $ T.wrap T.unit

unicodeCharacterStringLiteral :: TypeDefinition
unicodeCharacterStringLiteral = define "UnicodeCharacterStringLiteral" $ T.wrap T.unit

unsignedInteger :: TypeDefinition
unsignedInteger = define "UnsignedInteger" $ T.wrap T.string

-- Type definitions

approximateNumericType :: TypeDefinition
approximateNumericType = define "ApproximateNumericType" $
  T.union [
    "float">: T.optional (sql "Precision"),
    "real">: T.unit,
    "double">: T.unit]

arrayElement :: TypeDefinition
arrayElement = define "ArrayElement" $ T.wrap $ sql "ValueExpression"

arrayElementList :: TypeDefinition
arrayElementList = define "ArrayElementList" $
  T.record [
    "first">: sql "ArrayElement",
    "rest">: T.list (sql "ArrayElement")]

arrayElementReference :: TypeDefinition
arrayElementReference = define "ArrayElementReference" $ T.wrap T.unit

arrayType :: TypeDefinition
arrayType = define "ArrayType" $ T.wrap T.unit

arrayValueConstructor :: TypeDefinition
arrayValueConstructor = define "ArrayValueConstructor" $
  T.union [
    "enumeration">: sql "ArrayValueConstructorByEnumeration",
    "query">: sql "ArrayValueConstructorByQuery"]

arrayValueConstructorByEnumeration :: TypeDefinition
arrayValueConstructorByEnumeration = define "ArrayValueConstructorByEnumeration" $
  T.record [
    "LeftBracketOrTrigraph">: sql "LeftBracketOrTrigraph",
    "ArrayElementList">: sql "ArrayElementList",
    "RightBracketOrTrigraph">: sql "RightBracketOrTrigraph"]

arrayValueConstructorByQuery :: TypeDefinition
arrayValueConstructorByQuery = define "ArrayValueConstructorByQuery" $ T.wrap T.unit

arrayValueExpression :: TypeDefinition
arrayValueExpression = define "ArrayValueExpression" $ T.wrap T.unit

asSubqueryClause :: TypeDefinition
asSubqueryClause = define "AsSubqueryClause" $ T.wrap T.unit

attributeOrMethodReference :: TypeDefinition
attributeOrMethodReference = define "AttributeOrMethodReference" $ T.wrap T.unit

binaryLargeObjectStringType :: TypeDefinition
binaryLargeObjectStringType = define "BinaryLargeObjectStringType" $
  T.union [
    "binary">: T.optional (sql "LargeObjectLength"),
    "blob">: T.optional (sql "LargeObjectLength")]

booleanFactor :: TypeDefinition
booleanFactor = define "BooleanFactor" $
  T.record [
    "NOT">: T.optional T.unit,
    "BooleanTest">: sql "BooleanTest"]

booleanLiteral :: TypeDefinition
booleanLiteral = define "BooleanLiteral" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

booleanPredicand :: TypeDefinition
booleanPredicand = define "BooleanPredicand" $ T.wrap T.unit

booleanPrimary :: TypeDefinition
booleanPrimary = define "BooleanPrimary" $
  T.union [
    "predicate">: sql "Predicate",
    "predicand">: sql "BooleanPredicand"]

booleanTerm :: TypeDefinition
booleanTerm = define "BooleanTerm" $
  T.union [
    "factor">: sql "BooleanFactor",
    "and">: sql "BooleanTerm_And"]

booleanTerm_And :: TypeDefinition
booleanTerm_And = define "BooleanTerm_And" $
  T.record [
    "lhs">: sql "BooleanTerm",
    "rhs">: sql "BooleanFactor"]

booleanTest :: TypeDefinition
booleanTest = define "BooleanTest" $
  T.record [
    "BooleanPrimary">: sql "BooleanPrimary",
    "Sequence">: T.optional (sql "BooleanTest_Sequence_Option")]

booleanTest_Sequence_Option :: TypeDefinition
booleanTest_Sequence_Option = define "BooleanTest_Sequence_Option" $
  T.record [
    "NOT">: T.optional T.unit,
    "TruthValue">: sql "TruthValue"]

booleanType :: TypeDefinition
booleanType = define "BooleanType" $ T.wrap T.unit

booleanValueExpression :: TypeDefinition
booleanValueExpression = define "BooleanValueExpression" $
  T.union [
    "term">: sql "BooleanTerm",
    "or">: sql "BooleanValueExpression_Or"]

booleanValueExpression_Or :: TypeDefinition
booleanValueExpression_Or = define "BooleanValueExpression_Or" $
  T.record [
    "lhs">: sql "BooleanValueExpression",
    "rhs">: sql "BooleanTerm"]

caseExpression :: TypeDefinition
caseExpression = define "CaseExpression" $ T.wrap T.unit

castSpecification :: TypeDefinition
castSpecification = define "CastSpecification" $ T.wrap T.unit

characterSetSpecification :: TypeDefinition
characterSetSpecification = define "CharacterSetSpecification" $ T.wrap T.unit

characterStringType :: TypeDefinition
characterStringType = define "CharacterStringType" $
  T.union [
    "character">: T.optional (sql "Length"),
    "char">: T.optional (sql "Length"),
    "characterVarying">: sql "Length",
    "charVarying">: sql "Length",
    "varchar">: sql "Length",
    "characterLargeObject">: T.optional (sql "LargeObjectLength"),
    "charLargeObject">: T.optional (sql "LargeObjectLength"),
    "clob">: T.optional (sql "LargeObjectLength")]

collateClause :: TypeDefinition
collateClause = define "CollateClause" $ T.wrap T.unit

collectionType :: TypeDefinition
collectionType = define "CollectionType" $
  T.union [
    "array">: sql "ArrayType",
    "multiset">: sql "MultisetType"]

collectionValueConstructor :: TypeDefinition
collectionValueConstructor = define "CollectionValueConstructor" $
  T.union [
    "array">: sql "ArrayValueConstructor",
    "multiset">: sql "MultisetValueConstructor"]

collectionValueExpression :: TypeDefinition
collectionValueExpression = define "CollectionValueExpression" $
  T.union [
    "array">: sql "ArrayValueExpression",
    "multiset">: sql "MultisetValueExpression"]

columnConstraintDefinition :: TypeDefinition
columnConstraintDefinition = define "ColumnConstraintDefinition" $ T.wrap T.unit

columnDefinition :: TypeDefinition
columnDefinition = define "ColumnDefinition" $
  T.record [
    "name">: sql "ColumnName",
    "typeOrDomain">: T.optional (sql "ColumnDefinition_TypeOrDomain_Option"),
    "refScope">: T.optional (sql "ReferenceScopeCheck"),
    "defaultOrIdentityOrGeneration">: T.optional (sql "ColumnDefinition_DefaultOrIdentityOrGeneration_Option"),
    "constraints">: T.list (sql "ColumnConstraintDefinition"),
    "collate">: T.optional (sql "CollateClause")]

columnDefinition_DefaultOrIdentityOrGeneration_Option :: TypeDefinition
columnDefinition_DefaultOrIdentityOrGeneration_Option = define "ColumnDefinition_DefaultOrIdentityOrGeneration_Option" $
  T.union [
    "DefaultClause">: sql "DefaultClause",
    "IdentityColumnSpecification">: sql "IdentityColumnSpecification",
    "GenerationClause">: sql "GenerationClause"]

columnDefinition_TypeOrDomain_Option :: TypeDefinition
columnDefinition_TypeOrDomain_Option = define "ColumnDefinition_TypeOrDomain_Option" $
  T.union [
    "DataType">: sql "DataType",
    "DomainName">: sql "DomainName"]

columnNameList :: TypeDefinition
columnNameList = define "ColumnNameList" $
  T.record [
    "first">: sql "ColumnName",
    "rest">: T.list (sql "ColumnName")]

columnOptions :: TypeDefinition
columnOptions = define "ColumnOptions" $ T.wrap T.unit

columnReference :: TypeDefinition
columnReference = define "ColumnReference" $ T.wrap T.unit

commonValueExpression :: TypeDefinition
commonValueExpression = define "CommonValueExpression" $
  T.union [
    "numeric">: sql "NumericValueExpression",
    "string">: sql "StringValueExpression",
    "datetime">: sql "DatetimeValueExpression",
    "interval">: sql "IntervalValueExpression",
    "userDefined">: sql "UserDefinedTypeValueExpression",
    "reference">: sql "ReferenceValueExpression",
    "collection">: sql "CollectionValueExpression"]

contextuallyTypedRowValueConstructor :: TypeDefinition
contextuallyTypedRowValueConstructor = define "ContextuallyTypedRowValueConstructor" $ T.wrap T.unit

contextuallyTypedRowValueExpression :: TypeDefinition
contextuallyTypedRowValueExpression = define "ContextuallyTypedRowValueExpression" $
  T.union [
    "specialCase">: sql "RowValueSpecialCase",
    "constructor">: sql "ContextuallyTypedRowValueConstructor"]

contextuallyTypedRowValueExpressionList :: TypeDefinition
contextuallyTypedRowValueExpressionList = define "ContextuallyTypedRowValueExpressionList" $
  T.record [
    "first">: sql "ContextuallyTypedRowValueExpression",
    "rest">: T.list (sql "ContextuallyTypedRowValueExpression")]

contextuallyTypedTableValueConstructor :: TypeDefinition
contextuallyTypedTableValueConstructor = define "ContextuallyTypedTableValueConstructor" $
  T.wrap $ sql "ContextuallyTypedRowValueExpressionList"

dataType :: TypeDefinition
dataType = define "DataType" $
  T.union [
    "predefined">: sql "PredefinedType",
    "row">: sql "RowType",
    "named">: sql "PathResolvedUserDefinedTypeName",
    "reference">: sql "ReferenceType",
    "collection">: sql "CollectionType"]

dateLiteral :: TypeDefinition
dateLiteral = define "DateLiteral" $ T.wrap $ sql "DateString"

datetimeLiteral :: TypeDefinition
datetimeLiteral = define "DatetimeLiteral" $
  T.union [
    "date">: sql "DateLiteral",
    "time">: sql "TimeLiteral",
    "timestamp">: sql "TimestampLiteral"]

datetimeType :: TypeDefinition
datetimeType = define "DatetimeType" $ T.wrap T.unit

datetimeValueExpression :: TypeDefinition
datetimeValueExpression = define "DatetimeValueExpression" $ T.wrap T.unit

defaultClause :: TypeDefinition
defaultClause = define "DefaultClause" $ T.wrap T.unit

exactNumericType :: TypeDefinition
exactNumericType = define "ExactNumericType" $
  T.union [
    "numeric">: T.optional (sql "ExactNumericType_Numeric_Option"),
    "decimal">: T.optional (sql "ExactNumericType_Decimal_Option"),
    "dec">: T.optional (sql "ExactNumericType_Dec_Option"),
    "smallint">: T.unit,
    "integer">: T.unit,
    "int">: T.unit,
    "bigint">: T.unit]

exactNumericType_Dec_Option :: TypeDefinition
exactNumericType_Dec_Option = define "ExactNumericType_Dec_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

exactNumericType_Decimal_Option :: TypeDefinition
exactNumericType_Decimal_Option = define "ExactNumericType_Decimal_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

exactNumericType_Numeric_Option :: TypeDefinition
exactNumericType_Numeric_Option = define "ExactNumericType_Numeric_Option" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

fieldReference :: TypeDefinition
fieldReference = define "FieldReference" $ T.wrap T.unit

fromConstructor :: TypeDefinition
fromConstructor = define "FromConstructor" $
  T.record [
    "columns">: T.optional (sql "InsertColumnList"),
    "override">: T.optional (sql "OverrideClause"),
    "values">: sql "ContextuallyTypedTableValueConstructor"]

fromDefault :: TypeDefinition
fromDefault = define "FromDefault" $ T.wrap T.unit

fromSubquery :: TypeDefinition
fromSubquery = define "FromSubquery" $ T.wrap T.unit

generalLiteral :: TypeDefinition
generalLiteral = define "GeneralLiteral" $
  T.union [
    "string">: sql "CharacterStringLiteral",
    "nationalString">: sql "NationalCharacterStringLiteral",
    "unicode">: sql "UnicodeCharacterStringLiteral",
    "binary">: sql "BinaryStringLiteral",
    "dateTime">: sql "DatetimeLiteral",
    "interval">: sql "IntervalLiteral",
    "boolean">: sql "BooleanLiteral"]

generalValueSpecification :: TypeDefinition
generalValueSpecification = define "GeneralValueSpecification" $ T.wrap T.unit

generationClause :: TypeDefinition
generationClause = define "GenerationClause" $ T.wrap T.unit

globalOrLocal :: TypeDefinition
globalOrLocal = define "GlobalOrLocal" $
  T.union [
    "global">: T.unit,
    "local">: T.unit]

identityColumnSpecification :: TypeDefinition
identityColumnSpecification = define "IdentityColumnSpecification" $ T.wrap T.unit

insertColumnList :: TypeDefinition
insertColumnList = define "InsertColumnList" $ T.wrap $ sql "ColumnNameList"

insertColumnsAndSource :: TypeDefinition
insertColumnsAndSource = define "InsertColumnsAndSource" $
  T.union [
    "subquery">: sql "FromSubquery",
    "constructor">: sql "FromConstructor",
    "default">: sql "FromDefault"]

insertStatement :: TypeDefinition
insertStatement = define "InsertStatement" $
  T.record [
    "target">: sql "InsertionTarget",
    "columnsAndSource">: sql "InsertColumnsAndSource"]

insertionTarget :: TypeDefinition
insertionTarget = define "InsertionTarget" $ T.wrap $ sql "TableName"

intervalLiteral :: TypeDefinition
intervalLiteral = define "IntervalLiteral" $ T.wrap T.unit

intervalType :: TypeDefinition
intervalType = define "IntervalType" $ T.wrap T.unit

intervalValueExpression :: TypeDefinition
intervalValueExpression = define "IntervalValueExpression" $ T.wrap T.unit

largeObjectLength :: TypeDefinition
largeObjectLength = define "LargeObjectLength" $ T.wrap T.unit

length_ :: TypeDefinition
length_ = define "Length" $ T.wrap $ sql "UnsignedInteger"

likeClause :: TypeDefinition
likeClause = define "LikeClause" $ T.wrap T.unit

methodInvocation :: TypeDefinition
methodInvocation = define "MethodInvocation" $ T.wrap T.unit

multisetElementReference :: TypeDefinition
multisetElementReference = define "MultisetElementReference" $ T.wrap T.unit

multisetType :: TypeDefinition
multisetType = define "MultisetType" $ T.wrap $ sql "DataType"

multisetValueConstructor :: TypeDefinition
multisetValueConstructor = define "MultisetValueConstructor" $ T.wrap T.unit

multisetValueExpression :: TypeDefinition
multisetValueExpression = define "MultisetValueExpression" $ T.wrap T.unit

nationalCharacterStringType_ :: TypeDefinition
nationalCharacterStringType_ = define "NationalCharacterStringType" $ T.wrap T.unit

newSpecification :: TypeDefinition
newSpecification = define "NewSpecification" $ T.wrap T.unit

nextValueExpression :: TypeDefinition
nextValueExpression = define "NextValueExpression" $ T.wrap T.unit

nonparenthesizedValueExpressionPrimary :: TypeDefinition
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

numericType :: TypeDefinition
numericType = define "NumericType" $
  T.union [
    "exact">: sql "ExactNumericType",
    "approximate">: sql "ApproximateNumericType"]

numericValueExpression :: TypeDefinition
numericValueExpression = define "NumericValueExpression" $ T.wrap T.unit

overrideClause :: TypeDefinition
overrideClause = define "OverrideClause" $
  T.union [
    "OVERRIDINGspUSERspVALUE">: T.unit,
    "OVERRIDINGspSYSTEMspVALUE">: T.unit]

parenthesizedValueExpression :: TypeDefinition
parenthesizedValueExpression = define "ParenthesizedValueExpression" $
  T.wrap $ sql "ValueExpression"

precision :: TypeDefinition
precision = define "Precision" $ T.wrap $ sql "UnsignedInteger"

predefinedType :: TypeDefinition
predefinedType = define "PredefinedType" $
  T.union [
    "string">: sql "PredefinedType_String",
    "nationalString">: sql "PredefinedType_NationalString",
    "blob">: sql "BinaryLargeObjectStringType",
    "numeric">: sql "NumericType",
    "boolean">: sql "BooleanType",
    "datetime">: sql "DatetimeType",
    "interval">: sql "IntervalType"]

predefinedType_NationalString :: TypeDefinition
predefinedType_NationalString = define "PredefinedType_NationalString" $
  T.record [
    "type">: sql "NationalCharacterStringType",
    "collate">: T.optional (sql "CollateClause")]

predefinedType_String :: TypeDefinition
predefinedType_String = define "PredefinedType_String" $
  T.record [
    "type">: sql "CharacterStringType",
    "characters">: T.optional (sql "CharacterSetSpecification"),
    "collate">: T.optional (sql "CollateClause")]

predicate :: TypeDefinition
predicate = define "Predicate" $ T.wrap T.unit

queryExpression :: TypeDefinition
queryExpression = define "QueryExpression" $ T.wrap T.unit

referenceResolution :: TypeDefinition
referenceResolution = define "ReferenceResolution" $ T.wrap T.unit

referenceScopeCheck :: TypeDefinition
referenceScopeCheck = define "ReferenceScopeCheck" $ T.wrap T.unit

referenceType :: TypeDefinition
referenceType = define "ReferenceType" $ T.wrap T.unit

referenceValueExpression :: TypeDefinition
referenceValueExpression = define "ReferenceValueExpression" $
  T.wrap $ sql "ValueExpressionPrimary"

routineInvocation :: TypeDefinition
routineInvocation = define "RoutineInvocation" $ T.wrap T.unit

rowType :: TypeDefinition
rowType = define "RowType" $ T.wrap T.unit

rowValueExpression :: TypeDefinition
rowValueExpression = define "RowValueExpression" $ T.wrap T.unit

rowValueSpecialCase :: TypeDefinition
rowValueSpecialCase = define "RowValueSpecialCase" $
  T.wrap $ sql "NonparenthesizedValueExpressionPrimary"

scalarSubquery :: TypeDefinition
scalarSubquery = define "ScalarSubquery" $ T.wrap $ sql "Subquery"

scale :: TypeDefinition
scale = define "Scale" $ T.wrap $ sql "UnsignedInteger"

selfReferencingColumnSpecification :: TypeDefinition
selfReferencingColumnSpecification = define "SelfReferencingColumnSpecification" $ T.wrap T.unit

setFunctionSpecification :: TypeDefinition
setFunctionSpecification = define "SetFunctionSpecification" $ T.wrap T.unit

staticMethodInvocation :: TypeDefinition
staticMethodInvocation = define "StaticMethodInvocation" $ T.wrap T.unit

stringValueExpression :: TypeDefinition
stringValueExpression = define "StringValueExpression" $ T.wrap T.unit

subquery :: TypeDefinition
subquery = define "Subquery" $ T.wrap $ sql "QueryExpression"

subtableClause :: TypeDefinition
subtableClause = define "SubtableClause" $ T.wrap T.unit

subtypeTreatment :: TypeDefinition
subtypeTreatment = define "SubtypeTreatment" $ T.wrap T.unit

tableCommitAction :: TypeDefinition
tableCommitAction = define "TableCommitAction" $
  T.union [
    "preserve">: T.unit,
    "delete">: T.unit]

tableConstraintDefinition :: TypeDefinition
tableConstraintDefinition = define "TableConstraintDefinition" $ T.wrap T.unit

tableContentsSource :: TypeDefinition
tableContentsSource = define "TableContentsSource" $
  T.union [
    "list">: sql "TableElementList",
    "subtable">: sql "TableContentsSource_Subtable",
    "subquery">: sql "AsSubqueryClause"]

tableContentsSource_Subtable :: TypeDefinition
tableContentsSource_Subtable = define "TableContentsSource_Subtable" $
  T.record [
    "type">: sql "PathResolvedUserDefinedTypeName",
    "subtable">: T.optional (sql "SubtableClause"),
    "elements">: T.optional (sql "TableElementList")]

tableDefinition :: TypeDefinition
tableDefinition = define "TableDefinition" $
  T.record [
    "scope">: T.optional (sql "TableScope"),
    "name">: sql "TableName",
    "source">: sql "TableContentsSource",
    "commitActions">: T.optional (sql "TableCommitAction")]

tableElement :: TypeDefinition
tableElement = define "TableElement" $
  T.union [
    "column">: sql "ColumnDefinition",
    "tableConstraint">: sql "TableConstraintDefinition",
    "like">: sql "LikeClause",
    "selfReferencingColumn">: sql "SelfReferencingColumnSpecification",
    "columOptions">: sql "ColumnOptions"]

tableElementList :: TypeDefinition
tableElementList = define "TableElementList" $
  T.record [
    "first">: sql "TableElement",
    "rest">: T.list (sql "TableElement")]

tableScope :: TypeDefinition
tableScope = define "TableScope" $ T.wrap $ sql "GlobalOrLocal"

timeLiteral :: TypeDefinition
timeLiteral = define "TimeLiteral" $ T.wrap $ sql "TimeString"

truthValue :: TypeDefinition
truthValue = define "TruthValue" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

unsignedLiteral :: TypeDefinition
unsignedLiteral = define "UnsignedLiteral" $
  T.union [
    "numeric">: sql "UnsignedNumericLiteral",
    "general">: sql "GeneralLiteral"]

unsignedNumericLiteral :: TypeDefinition
unsignedNumericLiteral = define "UnsignedNumericLiteral" $
  T.union [
    "exact">: sql "ExactNumericLiteral",
    "approximate">: sql "ApproximateNumericLiteral"]

unsignedValueSpecification :: TypeDefinition
unsignedValueSpecification = define "UnsignedValueSpecification" $
  T.union [
    "literal">: sql "UnsignedLiteral",
    "general">: sql "GeneralValueSpecification"]

userDefinedTypeValueExpression :: TypeDefinition
userDefinedTypeValueExpression = define "UserDefinedTypeValueExpression" $
  T.wrap $ sql "ValueExpressionPrimary"

valueExpression :: TypeDefinition
valueExpression = define "ValueExpression" $
  T.union [
    "common">: sql "CommonValueExpression",
    "boolean">: sql "BooleanValueExpression",
    "row">: sql "RowValueExpression"]

valueExpressionPrimary :: TypeDefinition
valueExpressionPrimary = define "ValueExpressionPrimary" $
  T.union [
    "parens">: sql "ParenthesizedValueExpression",
    "noparens">: sql "NonparenthesizedValueExpressionPrimary"]

windowFunction :: TypeDefinition
windowFunction = define "WindowFunction" $ T.wrap T.unit
