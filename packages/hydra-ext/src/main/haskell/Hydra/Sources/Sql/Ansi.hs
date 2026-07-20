module Hydra.Sources.Sql.Ansi where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T


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
      approximateNumericType,
      arrayElement,
      arrayElementList,
      arrayElementReference,
      arrayType,
      arrayValueConstructor,
      arrayValueConstructorByEnumeration,
      arrayValueConstructorByQuery,
      arrayValueExpression,
      asSubqueryClause,
      attributeOrMethodReference,
      binaryLargeObjectStringType,
      binaryStringLiteral,
      booleanFactor,
      booleanLiteral,
      booleanPredicand,
      booleanPrimary,
      booleanTerm,
      booleanTermAnd,
      booleanTest,
      booleanTestSequenceOption,
      booleanType,
      booleanValueExpression,
      booleanValueExpressionOr,
      caseExpression,
      castSpecification,
      characterSetSpecification,
      characterStringLiteral,
      characterStringType,
      collateClause,
      collectionType,
      collectionValueConstructor,
      collectionValueExpression,
      columnConstraintDefinition,
      columnDefinition,
      columnDefinitionDefaultOrIdentityOrGenerationOption,
      columnDefinitionTypeOrDomainOption,
      columnName,
      columnNameList,
      columnOptions,
      columnReference,
      commonValueExpression,
      contextuallyTypedRowValueConstructor,
      contextuallyTypedRowValueExpression,
      contextuallyTypedRowValueExpressionList,
      contextuallyTypedTableValueConstructor,
      dataType,
      dateLiteral,
      dateString,
      datetimeLiteral,
      datetimeType,
      datetimeValueExpression,
      defaultClause,
      domainName,
      exactNumericLiteral,
      exactNumericType,
      exactNumericTypeDecOption,
      exactNumericTypeDecimalOption,
      exactNumericTypeNumericOption,
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
      leftBracketOrTrigraph,
      length_,
      likeClause,
      methodInvocation,
      multisetElementReference,
      multisetType,
      multisetValueConstructor,
      multisetValueExpression,
      nationalCharacterStringLiteral,
      nationalCharacterStringType_,
      newSpecification,
      nextValueExpression,
      nonparenthesizedValueExpressionPrimary,
      numericType,
      numericValueExpression,
      overrideClause,
      parenthesizedValueExpression,
      pathResolvedUserDefinedTypeName,
      precision,
      predefinedType,
      predefinedTypeNationalString,
      predefinedTypeString,
      predicate,
      queryExpression,
      referenceResolution,
      referenceScopeCheck,
      referenceType,
      referenceValueExpression,
      rightBracketOrTrigraph,
      routineInvocation,
      rowType,
      rowValueExpression,
      rowValueSpecialCase,
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
      tableContentsSourceSubtable,
      tableDefinition,
      tableElement,
      tableElementList,
      tableName,
      tableScope,
      timeLiteral,
      timeString,
      timestampLiteral,
      truthValue,
      unicodeCharacterStringLiteral,
      unsignedInteger,
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
approximateNumericLiteral = define "ApproximateNumericLiteral" $
  doc "An approximate (floating-point) numeric literal, such as 1.5E10" $ T.wrap T.string

binaryStringLiteral :: TypeDefinition
binaryStringLiteral = define "BinaryStringLiteral" $
  doc "A binary string literal, e.g. X'FF00'" $ T.wrap T.unit

characterStringLiteral :: TypeDefinition
characterStringLiteral = define "CharacterStringLiteral" $
  doc "A character string literal, e.g. 'hello'" $ T.wrap T.string

columnName :: TypeDefinition
columnName = define "ColumnName" $
  doc "The name of a table column" $ T.wrap T.string

dateString :: TypeDefinition
dateString = define "DateString" $
  doc "The string representation of a date literal, e.g. '2003-01-01'" $ T.wrap T.unit

domainName :: TypeDefinition
domainName = define "DomainName" $
  doc "The name of a user-defined domain" $ T.wrap T.string

exactNumericLiteral :: TypeDefinition
exactNumericLiteral = define "ExactNumericLiteral" $
  doc "An exact numeric literal, such as an integer or fixed-point decimal" $ T.wrap T.string

leftBracketOrTrigraph :: TypeDefinition
leftBracketOrTrigraph = define "LeftBracketOrTrigraph" $
  doc "A left square bracket, or its trigraph equivalent ??(" $ T.wrap T.string

nationalCharacterStringLiteral :: TypeDefinition
nationalCharacterStringLiteral = define "NationalCharacterStringLiteral" $
  doc "A national character string literal, e.g. N'hello'" $ T.wrap T.unit

pathResolvedUserDefinedTypeName :: TypeDefinition
pathResolvedUserDefinedTypeName = define "PathResolvedUserDefinedTypeName" $
  doc "A user-defined type name, resolved against the current schema path" $ T.wrap T.string

rightBracketOrTrigraph :: TypeDefinition
rightBracketOrTrigraph = define "RightBracketOrTrigraph" $
  doc "A right square bracket, or its trigraph equivalent ??)" $ T.wrap T.string

tableName :: TypeDefinition
tableName = define "TableName" $
  doc "The name of a table" $ T.wrap T.string

timeString :: TypeDefinition
timeString = define "TimeString" $
  doc "The string representation of a time literal, e.g. '12:00:00'" $ T.wrap T.unit

timestampLiteral :: TypeDefinition
timestampLiteral = define "TimestampLiteral" $
  doc "A timestamp literal, combining a date and a time" $ T.wrap T.unit

unicodeCharacterStringLiteral :: TypeDefinition
unicodeCharacterStringLiteral = define "UnicodeCharacterStringLiteral" $
  doc "A Unicode character string literal, e.g. U&'hello'" $ T.wrap T.unit

unsignedInteger :: TypeDefinition
unsignedInteger = define "UnsignedInteger" $
  doc "An unsigned integer literal" $ T.wrap T.string

-- Type definitions

approximateNumericType :: TypeDefinition
approximateNumericType = define "ApproximateNumericType" $
  doc "An approximate numeric type: FLOAT, REAL, or DOUBLE PRECISION" $
  T.union [
    "float">: T.optional (sql "Precision"),
    "real">: T.unit,
    "double">: T.unit]

arrayElement :: TypeDefinition
arrayElement = define "ArrayElement" $
  doc "A single element of an array value" $ T.wrap $ sql "ValueExpression"

arrayElementList :: TypeDefinition
arrayElementList = define "ArrayElementList" $
  doc "A non-empty, comma-separated list of array elements" $
  T.record [
    "first">: sql "ArrayElement",
    "rest">: T.list (sql "ArrayElement")]

arrayElementReference :: TypeDefinition
arrayElementReference = define "ArrayElementReference" $
  doc "A reference to an element of an array by index" $ T.wrap T.unit

arrayType :: TypeDefinition
arrayType = define "ArrayType" $
  doc "An ARRAY collection type" $ T.wrap T.unit

arrayValueConstructor :: TypeDefinition
arrayValueConstructor = define "ArrayValueConstructor" $
  doc "A constructor for an array value, either by enumeration or by query" $
  T.union [
    "enumeration">: sql "ArrayValueConstructorByEnumeration",
    "query">: sql "ArrayValueConstructorByQuery"]

arrayValueConstructorByEnumeration :: TypeDefinition
arrayValueConstructorByEnumeration = define "ArrayValueConstructorByEnumeration" $
  doc "An array value constructed by explicitly enumerating its elements" $
  T.record [
    "LeftBracketOrTrigraph">: sql "LeftBracketOrTrigraph",
    "ArrayElementList">: sql "ArrayElementList",
    "RightBracketOrTrigraph">: sql "RightBracketOrTrigraph"]

arrayValueConstructorByQuery :: TypeDefinition
arrayValueConstructorByQuery = define "ArrayValueConstructorByQuery" $
  doc "An array value constructed from the result of a query" $ T.wrap T.unit

arrayValueExpression :: TypeDefinition
arrayValueExpression = define "ArrayValueExpression" $
  doc "An expression that evaluates to an array value" $ T.wrap T.unit

asSubqueryClause :: TypeDefinition
asSubqueryClause = define "AsSubqueryClause" $
  doc "A clause introducing a table's contents as the result of a subquery" $ T.wrap T.unit

attributeOrMethodReference :: TypeDefinition
attributeOrMethodReference = define "AttributeOrMethodReference" $
  doc "A reference to an attribute or method of a structured value" $ T.wrap T.unit

binaryLargeObjectStringType :: TypeDefinition
binaryLargeObjectStringType = define "BinaryLargeObjectStringType" $
  doc "A binary large object string type: BINARY LARGE OBJECT or BLOB" $
  T.union [
    "binary">: T.optional (sql "LargeObjectLength"),
    "blob">: T.optional (sql "LargeObjectLength")]

booleanFactor :: TypeDefinition
booleanFactor = define "BooleanFactor" $
  doc "A boolean test, optionally negated with NOT" $
  T.record [
    "NOT">: T.optional T.unit,
    "BooleanTest">: sql "BooleanTest"]

booleanLiteral :: TypeDefinition
booleanLiteral = define "BooleanLiteral" $
  doc "A boolean literal: TRUE, FALSE, or UNKNOWN" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

booleanPredicand :: TypeDefinition
booleanPredicand = define "BooleanPredicand" $
  doc "An operand of a boolean predicate" $ T.wrap T.unit

booleanPrimary :: TypeDefinition
booleanPrimary = define "BooleanPrimary" $
  doc "A boolean primary expression: either a predicate or a boolean predicand" $
  T.union [
    "predicate">: sql "Predicate",
    "predicand">: sql "BooleanPredicand"]

booleanTerm :: TypeDefinition
booleanTerm = define "BooleanTerm" $
  doc "A boolean term: a factor, or a conjunction of terms with AND" $
  T.union [
    "factor">: sql "BooleanFactor",
    "conjunction">: sql "BooleanTermAnd"]

booleanTermAnd :: TypeDefinition
booleanTermAnd = define "BooleanTermAnd" $
  doc "A conjunction of two boolean terms joined by AND" $
  T.record [
    "lhs">: sql "BooleanTerm",
    "rhs">: sql "BooleanFactor"]

booleanTest :: TypeDefinition
booleanTest = define "BooleanTest" $
  doc "A boolean primary, optionally compared against a truth value with IS" $
  T.record [
    "BooleanPrimary">: sql "BooleanPrimary",
    "Sequence">: T.optional (sql "BooleanTestSequenceOption")]

booleanTestSequenceOption :: TypeDefinition
booleanTestSequenceOption = define "BooleanTestSequenceOption" $
  doc "The IS [NOT] truth-value suffix of a boolean test" $
  T.record [
    "NOT">: T.optional T.unit,
    "TruthValue">: sql "TruthValue"]

booleanType :: TypeDefinition
booleanType = define "BooleanType" $
  doc "The BOOLEAN data type" $ T.wrap T.unit

booleanValueExpression :: TypeDefinition
booleanValueExpression = define "BooleanValueExpression" $
  doc "A boolean value expression: a term, or a disjunction of terms with OR" $
  T.union [
    "term">: sql "BooleanTerm",
    "disjunction">: sql "BooleanValueExpressionOr"]

booleanValueExpressionOr :: TypeDefinition
booleanValueExpressionOr = define "BooleanValueExpressionOr" $
  doc "A disjunction of two boolean value expressions joined by OR" $
  T.record [
    "lhs">: sql "BooleanValueExpression",
    "rhs">: sql "BooleanTerm"]

caseExpression :: TypeDefinition
caseExpression = define "CaseExpression" $
  doc "A CASE expression, selecting a result from among several alternatives" $ T.wrap T.unit

castSpecification :: TypeDefinition
castSpecification = define "CastSpecification" $
  doc "A CAST expression, converting a value to a specified data type" $ T.wrap T.unit

characterSetSpecification :: TypeDefinition
characterSetSpecification = define "CharacterSetSpecification" $
  doc "The name of a character set" $ T.wrap T.unit

characterStringType :: TypeDefinition
characterStringType = define "CharacterStringType" $
  doc "A character string type: CHARACTER, VARCHAR, or CHARACTER LARGE OBJECT" $
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
collateClause = define "CollateClause" $
  doc "A COLLATE clause specifying the collation to apply to a character value" $ T.wrap T.unit

collectionType :: TypeDefinition
collectionType = define "CollectionType" $
  doc "A collection type: ARRAY or MULTISET" $
  T.union [
    "array">: sql "ArrayType",
    "multiset">: sql "MultisetType"]

collectionValueConstructor :: TypeDefinition
collectionValueConstructor = define "CollectionValueConstructor" $
  doc "A constructor for a collection value: an array or multiset constructor" $
  T.union [
    "array">: sql "ArrayValueConstructor",
    "multiset">: sql "MultisetValueConstructor"]

collectionValueExpression :: TypeDefinition
collectionValueExpression = define "CollectionValueExpression" $
  doc "An expression that evaluates to a collection value" $
  T.union [
    "array">: sql "ArrayValueExpression",
    "multiset">: sql "MultisetValueExpression"]

columnConstraintDefinition :: TypeDefinition
columnConstraintDefinition = define "ColumnConstraintDefinition" $
  doc "A constraint attached to a single column definition" $ T.wrap T.unit

columnDefinition :: TypeDefinition
columnDefinition = define "ColumnDefinition" $
  doc "The definition of a single column within a table definition" $
  T.record [
    "name">: sql "ColumnName",
    "typeOrDomain">: T.optional (sql "ColumnDefinitionTypeOrDomainOption"),
    "refScope">: T.optional (sql "ReferenceScopeCheck"),
    "defaultOrIdentityOrGeneration">: T.optional (sql "ColumnDefinitionDefaultOrIdentityOrGenerationOption"),
    "constraints">: T.list (sql "ColumnConstraintDefinition"),
    "collate">: T.optional (sql "CollateClause")]

columnDefinitionDefaultOrIdentityOrGenerationOption :: TypeDefinition
columnDefinitionDefaultOrIdentityOrGenerationOption = define "ColumnDefinitionDefaultOrIdentityOrGenerationOption" $
  doc "A column's default value, identity, or generation specification" $
  T.union [
    "DefaultClause">: sql "DefaultClause",
    "IdentityColumnSpecification">: sql "IdentityColumnSpecification",
    "GenerationClause">: sql "GenerationClause"]

columnDefinitionTypeOrDomainOption :: TypeDefinition
columnDefinitionTypeOrDomainOption = define "ColumnDefinitionTypeOrDomainOption" $
  doc "A column's declared data type or domain" $
  T.union [
    "DataType">: sql "DataType",
    "DomainName">: sql "DomainName"]

columnNameList :: TypeDefinition
columnNameList = define "ColumnNameList" $
  doc "A non-empty, comma-separated list of column names" $
  T.record [
    "first">: sql "ColumnName",
    "rest">: T.list (sql "ColumnName")]

columnOptions :: TypeDefinition
columnOptions = define "ColumnOptions" $
  doc "The WITH OPTIONS clause of a column definition inherited via LIKE" $ T.wrap T.unit

columnReference :: TypeDefinition
columnReference = define "ColumnReference" $
  doc "A reference to a column, optionally qualified by a table or correlation name" $ T.wrap T.unit

commonValueExpression :: TypeDefinition
commonValueExpression = define "CommonValueExpression" $
  doc "A value expression common to numeric, string, datetime, interval, user-defined, reference, and collection types" $
  T.union [
    "numeric">: sql "NumericValueExpression",
    "string">: sql "StringValueExpression",
    "datetime">: sql "DatetimeValueExpression",
    "interval">: sql "IntervalValueExpression",
    "userDefined">: sql "UserDefinedTypeValueExpression",
    "reference">: sql "ReferenceValueExpression",
    "collection">: sql "CollectionValueExpression"]

contextuallyTypedRowValueConstructor :: TypeDefinition
contextuallyTypedRowValueConstructor = define "ContextuallyTypedRowValueConstructor" $
  doc "A row value constructor whose element types are inferred from context" $ T.wrap T.unit

contextuallyTypedRowValueExpression :: TypeDefinition
contextuallyTypedRowValueExpression = define "ContextuallyTypedRowValueExpression" $
  doc "A contextually typed row value expression: a special case or a constructor" $
  T.union [
    "specialCase">: sql "RowValueSpecialCase",
    "constructor">: sql "ContextuallyTypedRowValueConstructor"]

contextuallyTypedRowValueExpressionList :: TypeDefinition
contextuallyTypedRowValueExpressionList = define "ContextuallyTypedRowValueExpressionList" $
  doc "A non-empty, comma-separated list of contextually typed row value expressions" $
  T.record [
    "first">: sql "ContextuallyTypedRowValueExpression",
    "rest">: T.list (sql "ContextuallyTypedRowValueExpression")]

contextuallyTypedTableValueConstructor :: TypeDefinition
contextuallyTypedTableValueConstructor = define "ContextuallyTypedTableValueConstructor" $
  doc "A table value constructor built from a list of contextually typed row values" $
  T.wrap $ sql "ContextuallyTypedRowValueExpressionList"

dataType :: TypeDefinition
dataType = define "DataType" $
  doc "A data type: predefined, row, named, reference, or collection" $
  T.union [
    "predefined">: sql "PredefinedType",
    "row">: sql "RowType",
    "named">: sql "PathResolvedUserDefinedTypeName",
    "reference">: sql "ReferenceType",
    "collection">: sql "CollectionType"]

dateLiteral :: TypeDefinition
dateLiteral = define "DateLiteral" $
  doc "A DATE literal" $ T.wrap $ sql "DateString"

datetimeLiteral :: TypeDefinition
datetimeLiteral = define "DatetimeLiteral" $
  doc "A datetime literal: a date, time, or timestamp literal" $
  T.union [
    "date">: sql "DateLiteral",
    "time">: sql "TimeLiteral",
    "timestamp">: sql "TimestampLiteral"]

datetimeType :: TypeDefinition
datetimeType = define "DatetimeType" $
  doc "A datetime type: DATE, TIME, or TIMESTAMP" $ T.wrap T.unit

datetimeValueExpression :: TypeDefinition
datetimeValueExpression = define "DatetimeValueExpression" $
  doc "An expression that evaluates to a datetime value" $ T.wrap T.unit

defaultClause :: TypeDefinition
defaultClause = define "DefaultClause" $
  doc "A DEFAULT clause specifying a column's default value" $ T.wrap T.unit

exactNumericType :: TypeDefinition
exactNumericType = define "ExactNumericType" $
  doc "An exact numeric type: NUMERIC, DECIMAL, DEC, SMALLINT, INTEGER, INT, or BIGINT" $
  T.union [
    "numeric">: T.optional (sql "ExactNumericTypeNumericOption"),
    "decimal">: T.optional (sql "ExactNumericTypeDecimalOption"),
    "dec">: T.optional (sql "ExactNumericTypeDecOption"),
    "smallint">: T.unit,
    "integer">: T.unit,
    "int">: T.unit,
    "bigint">: T.unit]

exactNumericTypeDecOption :: TypeDefinition
exactNumericTypeDecOption = define "ExactNumericTypeDecOption" $
  doc "The precision and optional scale of a DEC type" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

exactNumericTypeDecimalOption :: TypeDefinition
exactNumericTypeDecimalOption = define "ExactNumericTypeDecimalOption" $
  doc "The precision and optional scale of a DECIMAL type" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

exactNumericTypeNumericOption :: TypeDefinition
exactNumericTypeNumericOption = define "ExactNumericTypeNumericOption" $
  doc "The precision and optional scale of a NUMERIC type" $
  T.record [
    "Precision">: sql "Precision",
    "Sequence">: T.optional (sql "Scale")]

fieldReference :: TypeDefinition
fieldReference = define "FieldReference" $
  doc "A reference to a field of a row or structured value" $ T.wrap T.unit

fromConstructor :: TypeDefinition
fromConstructor = define "FromConstructor" $
  doc "An INSERT source specified by an explicit value constructor, with optional column list and override clause" $
  T.record [
    "columns">: T.optional (sql "InsertColumnList"),
    "override">: T.optional (sql "OverrideClause"),
    "values">: sql "ContextuallyTypedTableValueConstructor"]

fromDefault :: TypeDefinition
fromDefault = define "FromDefault" $
  doc "An INSERT source specified as DEFAULT VALUES" $ T.wrap T.unit

fromSubquery :: TypeDefinition
fromSubquery = define "FromSubquery" $
  doc "An INSERT source specified as the result of a subquery" $ T.wrap T.unit

generalLiteral :: TypeDefinition
generalLiteral = define "GeneralLiteral" $
  doc "A general literal: a string, datetime, interval, or boolean literal" $
  T.union [
    "string">: sql "CharacterStringLiteral",
    "nationalString">: sql "NationalCharacterStringLiteral",
    "unicode">: sql "UnicodeCharacterStringLiteral",
    "binary">: sql "BinaryStringLiteral",
    "dateTime">: sql "DatetimeLiteral",
    "interval">: sql "IntervalLiteral",
    "boolean">: sql "BooleanLiteral"]

generalValueSpecification :: TypeDefinition
generalValueSpecification = define "GeneralValueSpecification" $
  doc "A general value specification, such as a parameter or dynamic value" $ T.wrap T.unit

generationClause :: TypeDefinition
generationClause = define "GenerationClause" $
  doc "A GENERATED ALWAYS AS clause for a computed column" $ T.wrap T.unit

globalOrLocal :: TypeDefinition
globalOrLocal = define "GlobalOrLocal" $
  doc "The GLOBAL or LOCAL qualifier of a temporary table scope" $
  T.union [
    "global">: T.unit,
    "local">: T.unit]

identityColumnSpecification :: TypeDefinition
identityColumnSpecification = define "IdentityColumnSpecification" $
  doc "A GENERATED ... AS IDENTITY column specification" $ T.wrap T.unit

insertColumnList :: TypeDefinition
insertColumnList = define "InsertColumnList" $
  doc "The parenthesized list of target column names in an INSERT statement" $ T.wrap $ sql "ColumnNameList"

insertColumnsAndSource :: TypeDefinition
insertColumnsAndSource = define "InsertColumnsAndSource" $
  doc "The column list and value source of an INSERT statement: a subquery, constructor, or default" $
  T.union [
    "subquery">: sql "FromSubquery",
    "constructor">: sql "FromConstructor",
    "default">: sql "FromDefault"]

insertStatement :: TypeDefinition
insertStatement = define "InsertStatement" $
  doc "An ANSI SQL INSERT statement" $
  T.record [
    "target">: sql "InsertionTarget",
    "columnsAndSource">: sql "InsertColumnsAndSource"]

insertionTarget :: TypeDefinition
insertionTarget = define "InsertionTarget" $
  doc "The table into which an INSERT statement inserts rows" $ T.wrap $ sql "TableName"

intervalLiteral :: TypeDefinition
intervalLiteral = define "IntervalLiteral" $
  doc "An INTERVAL literal" $ T.wrap T.unit

intervalType :: TypeDefinition
intervalType = define "IntervalType" $
  doc "The INTERVAL data type" $ T.wrap T.unit

intervalValueExpression :: TypeDefinition
intervalValueExpression = define "IntervalValueExpression" $
  doc "An expression that evaluates to an interval value" $ T.wrap T.unit

largeObjectLength :: TypeDefinition
largeObjectLength = define "LargeObjectLength" $
  doc "The length specification of a large object type, e.g. 1K, 1M, or 1G" $ T.wrap T.unit

length_ :: TypeDefinition
length_ = define "Length" $
  doc "The length specification of a character or binary string type" $ T.wrap $ sql "UnsignedInteger"

likeClause :: TypeDefinition
likeClause = define "LikeClause" $
  doc "A LIKE clause, copying column definitions from another table" $ T.wrap T.unit

methodInvocation :: TypeDefinition
methodInvocation = define "MethodInvocation" $
  doc "An invocation of a method on a structured value" $ T.wrap T.unit

multisetElementReference :: TypeDefinition
multisetElementReference = define "MultisetElementReference" $
  doc "A reference to an element of a multiset by way of the ELEMENT function" $ T.wrap T.unit

multisetType :: TypeDefinition
multisetType = define "MultisetType" $
  doc "A MULTISET collection type over a given element data type" $ T.wrap $ sql "DataType"

multisetValueConstructor :: TypeDefinition
multisetValueConstructor = define "MultisetValueConstructor" $
  doc "A constructor for a multiset value" $ T.wrap T.unit

multisetValueExpression :: TypeDefinition
multisetValueExpression = define "MultisetValueExpression" $
  doc "An expression that evaluates to a multiset value" $ T.wrap T.unit

nationalCharacterStringType_ :: TypeDefinition
nationalCharacterStringType_ = define "NationalCharacterStringType" $
  doc "A national character string type: NATIONAL CHARACTER or NATIONAL CHARACTER VARYING" $ T.wrap T.unit

newSpecification :: TypeDefinition
newSpecification = define "NewSpecification" $
  doc "A NEW invocation, constructing an instance of a structured type" $ T.wrap T.unit

nextValueExpression :: TypeDefinition
nextValueExpression = define "NextValueExpression" $
  doc "A NEXT VALUE FOR expression, retrieving the next value of a sequence generator" $ T.wrap T.unit

nonparenthesizedValueExpressionPrimary :: TypeDefinition
nonparenthesizedValueExpressionPrimary = define "NonparenthesizedValueExpressionPrimary" $
  doc "A value expression primary that does not begin with a parenthesis" $
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
  doc "A numeric type: exact or approximate" $
  T.union [
    "exact">: sql "ExactNumericType",
    "approximate">: sql "ApproximateNumericType"]

numericValueExpression :: TypeDefinition
numericValueExpression = define "NumericValueExpression" $
  doc "An expression that evaluates to a numeric value" $ T.wrap T.unit

overrideClause :: TypeDefinition
overrideClause = define "OverrideClause" $
  doc "An OVERRIDING USER VALUE or OVERRIDING SYSTEM VALUE clause of an INSERT statement" $
  T.union [
    "OVERRIDINGspUSERspVALUE">: T.unit,
    "OVERRIDINGspSYSTEMspVALUE">: T.unit]

parenthesizedValueExpression :: TypeDefinition
parenthesizedValueExpression = define "ParenthesizedValueExpression" $
  doc "A value expression enclosed in parentheses" $
  T.wrap $ sql "ValueExpression"

precision :: TypeDefinition
precision = define "Precision" $
  doc "The precision of a numeric type, as an unsigned integer" $ T.wrap $ sql "UnsignedInteger"

predefinedType :: TypeDefinition
predefinedType = define "PredefinedType" $
  doc "A predefined data type: string, numeric, boolean, datetime, or interval" $
  T.union [
    "stringType">: sql "PredefinedTypeString",
    "nationalStringType">: sql "PredefinedTypeNationalString",
    "blob">: sql "BinaryLargeObjectStringType",
    "numeric">: sql "NumericType",
    "boolean">: sql "BooleanType",
    "datetime">: sql "DatetimeType",
    "interval">: sql "IntervalType"]

predefinedTypeNationalString :: TypeDefinition
predefinedTypeNationalString = define "PredefinedTypeNationalString" $
  doc "A national character string type together with an optional COLLATE clause" $
  T.record [
    "type">: sql "NationalCharacterStringType",
    "collate">: T.optional (sql "CollateClause")]

predefinedTypeString :: TypeDefinition
predefinedTypeString = define "PredefinedTypeString" $
  doc "A character string type together with an optional character set and COLLATE clause" $
  T.record [
    "type">: sql "CharacterStringType",
    "characters">: T.optional (sql "CharacterSetSpecification"),
    "collate">: T.optional (sql "CollateClause")]

predicate :: TypeDefinition
predicate = define "Predicate" $
  doc "A predicate, evaluating to a boolean truth value" $ T.wrap T.unit

queryExpression :: TypeDefinition
queryExpression = define "QueryExpression" $
  doc "A query expression, such as a SELECT statement possibly combined with set operators" $ T.wrap T.unit

referenceResolution :: TypeDefinition
referenceResolution = define "ReferenceResolution" $
  doc "The DEREF operator, resolving a reference to the value it targets" $ T.wrap T.unit

referenceScopeCheck :: TypeDefinition
referenceScopeCheck = define "ReferenceScopeCheck" $
  doc "A REFERENCES ARE [NOT] CHECKED clause on a reference-typed column" $ T.wrap T.unit

referenceType :: TypeDefinition
referenceType = define "ReferenceType" $
  doc "A REF reference type over a given referenced type" $ T.wrap T.unit

referenceValueExpression :: TypeDefinition
referenceValueExpression = define "ReferenceValueExpression" $
  doc "An expression that evaluates to a reference value" $
  T.wrap $ sql "ValueExpressionPrimary"

routineInvocation :: TypeDefinition
routineInvocation = define "RoutineInvocation" $
  doc "An invocation of a user-defined SQL routine" $ T.wrap T.unit

rowType :: TypeDefinition
rowType = define "RowType" $
  doc "A ROW type, defined by a list of field name and data type pairs" $ T.wrap T.unit

rowValueExpression :: TypeDefinition
rowValueExpression = define "RowValueExpression" $
  doc "An expression that evaluates to a row value" $ T.wrap T.unit

rowValueSpecialCase :: TypeDefinition
rowValueSpecialCase = define "RowValueSpecialCase" $
  doc "A row value expressed as a single nonparenthesized value expression" $
  T.wrap $ sql "NonparenthesizedValueExpressionPrimary"

scalarSubquery :: TypeDefinition
scalarSubquery = define "ScalarSubquery" $
  doc "A subquery that returns exactly one row and one column" $ T.wrap $ sql "Subquery"

scale :: TypeDefinition
scale = define "Scale" $
  doc "The scale of a numeric type, as an unsigned integer" $ T.wrap $ sql "UnsignedInteger"

selfReferencingColumnSpecification :: TypeDefinition
selfReferencingColumnSpecification = define "SelfReferencingColumnSpecification" $
  doc "A REF IS clause identifying a table's self-referencing column" $ T.wrap T.unit

setFunctionSpecification :: TypeDefinition
setFunctionSpecification = define "SetFunctionSpecification" $
  doc "A set (aggregate) function invocation, such as COUNT or SUM" $ T.wrap T.unit

staticMethodInvocation :: TypeDefinition
staticMethodInvocation = define "StaticMethodInvocation" $
  doc "An invocation of a static method on a user-defined type" $ T.wrap T.unit

stringValueExpression :: TypeDefinition
stringValueExpression = define "StringValueExpression" $
  doc "An expression that evaluates to a character, binary, or Unicode string value" $ T.wrap T.unit

subquery :: TypeDefinition
subquery = define "Subquery" $
  doc "A parenthesized query expression used as an operand" $ T.wrap $ sql "QueryExpression"

subtableClause :: TypeDefinition
subtableClause = define "SubtableClause" $
  doc "An UNDER clause declaring a table as a subtable of a supertable" $ T.wrap T.unit

subtypeTreatment :: TypeDefinition
subtypeTreatment = define "SubtypeTreatment" $
  doc "A TREAT AS expression, treating a value as an instance of a subtype" $ T.wrap T.unit

tableCommitAction :: TypeDefinition
tableCommitAction = define "TableCommitAction" $
  doc "The ON COMMIT action of a temporary table: PRESERVE ROWS or DELETE ROWS" $
  T.union [
    "preserve">: T.unit,
    "delete">: T.unit]

tableConstraintDefinition :: TypeDefinition
tableConstraintDefinition = define "TableConstraintDefinition" $
  doc "A table-level constraint definition" $ T.wrap T.unit

tableContentsSource :: TypeDefinition
tableContentsSource = define "TableContentsSource" $
  doc "The source of a table's contents: an element list, a supertable, or a query" $
  T.union [
    "list">: sql "TableElementList",
    "subtableOf">: sql "TableContentsSourceSubtable",
    "subquery">: sql "AsSubqueryClause"]

tableContentsSourceSubtable :: TypeDefinition
tableContentsSourceSubtable = define "TableContentsSourceSubtable" $
  doc "A table's contents defined as a subtable of a named user-defined type" $
  T.record [
    "type">: sql "PathResolvedUserDefinedTypeName",
    "subtable">: T.optional (sql "SubtableClause"),
    "elements">: T.optional (sql "TableElementList")]

tableDefinition :: TypeDefinition
tableDefinition = define "TableDefinition" $
  doc "An ANSI SQL CREATE TABLE statement" $
  T.record [
    "scope">: T.optional (sql "TableScope"),
    "name">: sql "TableName",
    "source">: sql "TableContentsSource",
    "commitActions">: T.optional (sql "TableCommitAction")]

tableElement :: TypeDefinition
tableElement = define "TableElement" $
  doc "An element of a table definition: a column, constraint, LIKE clause, or related option" $
  T.union [
    "column">: sql "ColumnDefinition",
    "tableConstraint">: sql "TableConstraintDefinition",
    "like">: sql "LikeClause",
    "selfReferencingColumn">: sql "SelfReferencingColumnSpecification",
    "columOptions">: sql "ColumnOptions"]

tableElementList :: TypeDefinition
tableElementList = define "TableElementList" $
  doc "A non-empty, comma-separated list of table elements" $
  T.record [
    "first">: sql "TableElement",
    "rest">: T.list (sql "TableElement")]

tableScope :: TypeDefinition
tableScope = define "TableScope" $
  doc "The GLOBAL or LOCAL TEMPORARY scope of a table definition" $ T.wrap $ sql "GlobalOrLocal"

timeLiteral :: TypeDefinition
timeLiteral = define "TimeLiteral" $
  doc "A TIME literal" $ T.wrap $ sql "TimeString"

truthValue :: TypeDefinition
truthValue = define "TruthValue" $
  doc "A three-valued logic truth value: TRUE, FALSE, or UNKNOWN" $
  T.union [
    "TRUE">: T.unit,
    "FALSE">: T.unit,
    "UNKNOWN">: T.unit]

unsignedLiteral :: TypeDefinition
unsignedLiteral = define "UnsignedLiteral" $
  doc "An unsigned literal: a numeric or general literal" $
  T.union [
    "numeric">: sql "UnsignedNumericLiteral",
    "general">: sql "GeneralLiteral"]

unsignedNumericLiteral :: TypeDefinition
unsignedNumericLiteral = define "UnsignedNumericLiteral" $
  doc "An unsigned numeric literal: exact or approximate" $
  T.union [
    "exact">: sql "ExactNumericLiteral",
    "approximate">: sql "ApproximateNumericLiteral"]

unsignedValueSpecification :: TypeDefinition
unsignedValueSpecification = define "UnsignedValueSpecification" $
  doc "An unsigned value specification: a literal or a general value specification" $
  T.union [
    "literal">: sql "UnsignedLiteral",
    "general">: sql "GeneralValueSpecification"]

userDefinedTypeValueExpression :: TypeDefinition
userDefinedTypeValueExpression = define "UserDefinedTypeValueExpression" $
  doc "An expression that evaluates to a value of a user-defined type" $
  T.wrap $ sql "ValueExpressionPrimary"

valueExpression :: TypeDefinition
valueExpression = define "ValueExpression" $
  doc "A value expression: common, boolean, or row-valued" $
  T.union [
    "common">: sql "CommonValueExpression",
    "boolean">: sql "BooleanValueExpression",
    "row">: sql "RowValueExpression"]

valueExpressionPrimary :: TypeDefinition
valueExpressionPrimary = define "ValueExpressionPrimary" $
  doc "A value expression primary, optionally parenthesized" $
  T.union [
    "parens">: sql "ParenthesizedValueExpression",
    "noparens">: sql "NonparenthesizedValueExpressionPrimary"]

windowFunction :: TypeDefinition
windowFunction = define "WindowFunction" $
  doc "A window function invocation, such as ROW_NUMBER() OVER (...)" $ T.wrap T.unit
