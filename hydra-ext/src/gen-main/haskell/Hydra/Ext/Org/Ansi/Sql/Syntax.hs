-- | A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003

module Hydra.Ext.Org.Ansi.Sql.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

newtype ApproximateNumericLiteral = 
  ApproximateNumericLiteral {
    unApproximateNumericLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericLiteral")

newtype BinaryStringLiteral = 
  BinaryStringLiteral {
    unBinaryStringLiteral :: ()}
  deriving (Eq, Ord, Read, Show)

_BinaryStringLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryStringLiteral")

newtype CharacterStringLiteral = 
  CharacterStringLiteral {
    unCharacterStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_CharacterStringLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringLiteral")

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnName")

newtype DateString = 
  DateString {
    unDateString :: ()}
  deriving (Eq, Ord, Read, Show)

_DateString = (Core.Name "hydra.ext.org.ansi.sql.syntax.DateString")

newtype DomainName = 
  DomainName {
    unDomainName :: String}
  deriving (Eq, Ord, Read, Show)

_DomainName = (Core.Name "hydra.ext.org.ansi.sql.syntax.DomainName")

newtype ExactNumericLiteral = 
  ExactNumericLiteral {
    unExactNumericLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ExactNumericLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericLiteral")

newtype LeftBracketOrTrigraph = 
  LeftBracketOrTrigraph {
    unLeftBracketOrTrigraph :: String}
  deriving (Eq, Ord, Read, Show)

_LeftBracketOrTrigraph = (Core.Name "hydra.ext.org.ansi.sql.syntax.LeftBracketOrTrigraph")

newtype RightBracketOrTrigraph = 
  RightBracketOrTrigraph {
    unRightBracketOrTrigraph :: String}
  deriving (Eq, Ord, Read, Show)

_RightBracketOrTrigraph = (Core.Name "hydra.ext.org.ansi.sql.syntax.RightBracketOrTrigraph")

newtype NationalCharacterStringLiteral = 
  NationalCharacterStringLiteral {
    unNationalCharacterStringLiteral :: ()}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringLiteral")

newtype PathResolvedUserDefinedTypeName = 
  PathResolvedUserDefinedTypeName {
    unPathResolvedUserDefinedTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_PathResolvedUserDefinedTypeName = (Core.Name "hydra.ext.org.ansi.sql.syntax.PathResolvedUserDefinedTypeName")

newtype TableName = 
  TableName {
    unTableName :: String}
  deriving (Eq, Ord, Read, Show)

_TableName = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableName")

newtype TimeString = 
  TimeString {
    unTimeString :: ()}
  deriving (Eq, Ord, Read, Show)

_TimeString = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeString")

newtype TimestampLiteral = 
  TimestampLiteral {
    unTimestampLiteral :: ()}
  deriving (Eq, Ord, Read, Show)

_TimestampLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimestampLiteral")

newtype UnicodeCharacterStringLiteral = 
  UnicodeCharacterStringLiteral {
    unUnicodeCharacterStringLiteral :: ()}
  deriving (Eq, Ord, Read, Show)

_UnicodeCharacterStringLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnicodeCharacterStringLiteral")

newtype UnsignedInteger = 
  UnsignedInteger {
    unUnsignedInteger :: String}
  deriving (Eq, Ord, Read, Show)

_UnsignedInteger = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedInteger")

data ApproximateNumericType = 
  ApproximateNumericTypeFloat (Maybe Precision) |
  ApproximateNumericTypeReal  |
  ApproximateNumericTypeDouble 
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericType")

_ApproximateNumericType_float = (Core.Name "float")

_ApproximateNumericType_real = (Core.Name "real")

_ApproximateNumericType_double = (Core.Name "double")

newtype ArrayElement = 
  ArrayElement {
    unArrayElement :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ArrayElement = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElement")

data ArrayElementList = 
  ArrayElementList {
    arrayElementListFirst :: ArrayElement,
    arrayElementListRest :: [ArrayElement]}
  deriving (Eq, Ord, Read, Show)

_ArrayElementList = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementList")

_ArrayElementList_first = (Core.Name "first")

_ArrayElementList_rest = (Core.Name "rest")

newtype ArrayElementReference = 
  ArrayElementReference {
    unArrayElementReference :: ()}
  deriving (Eq, Ord, Read, Show)

_ArrayElementReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementReference")

newtype ArrayType = 
  ArrayType {
    unArrayType :: ()}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayType")

data ArrayValueConstructor = 
  ArrayValueConstructorEnumeration ArrayValueConstructorByEnumeration |
  ArrayValueConstructorQuery ArrayValueConstructorByQuery
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor")

_ArrayValueConstructor_enumeration = (Core.Name "enumeration")

_ArrayValueConstructor_query = (Core.Name "query")

newtype ArrayValueConstructorByQuery = 
  ArrayValueConstructorByQuery {
    unArrayValueConstructorByQuery :: ()}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByQuery = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery")

data ArrayValueConstructorByEnumeration = 
  ArrayValueConstructorByEnumeration {
    arrayValueConstructorByEnumerationLeftBracketOrTrigraph :: LeftBracketOrTrigraph,
    arrayValueConstructorByEnumerationArrayElementList :: ArrayElementList,
    arrayValueConstructorByEnumerationRightBracketOrTrigraph :: RightBracketOrTrigraph}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByEnumeration = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration")

_ArrayValueConstructorByEnumeration_LeftBracketOrTrigraph = (Core.Name "LeftBracketOrTrigraph")

_ArrayValueConstructorByEnumeration_ArrayElementList = (Core.Name "ArrayElementList")

_ArrayValueConstructorByEnumeration_RightBracketOrTrigraph = (Core.Name "RightBracketOrTrigraph")

newtype ArrayValueExpression = 
  ArrayValueExpression {
    unArrayValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_ArrayValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueExpression")

newtype AsSubqueryClause = 
  AsSubqueryClause {
    unAsSubqueryClause :: ()}
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.AsSubqueryClause")

newtype AttributeOrMethodReference = 
  AttributeOrMethodReference {
    unAttributeOrMethodReference :: ()}
  deriving (Eq, Ord, Read, Show)

_AttributeOrMethodReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.AttributeOrMethodReference")

data BinaryLargeObjectStringType = 
  BinaryLargeObjectStringTypeBinary (Maybe LargeObjectLength) |
  BinaryLargeObjectStringTypeBlob (Maybe LargeObjectLength)
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType = (Core.Name "hydra.ext.org.ansi.sql.syntax.BinaryLargeObjectStringType")

_BinaryLargeObjectStringType_binary = (Core.Name "binary")

_BinaryLargeObjectStringType_blob = (Core.Name "blob")

data BooleanFactor = 
  BooleanFactor {
    booleanFactorNOT :: (Maybe ()),
    booleanFactorBooleanTest :: BooleanTest}
  deriving (Eq, Ord, Read, Show)

_BooleanFactor = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanFactor")

_BooleanFactor_NOT = (Core.Name "NOT")

_BooleanFactor_BooleanTest = (Core.Name "BooleanTest")

data BooleanLiteral = 
  BooleanLiteralTRUE  |
  BooleanLiteralFALSE  |
  BooleanLiteralUNKNOWN 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanLiteral")

_BooleanLiteral_TRUE = (Core.Name "TRUE")

_BooleanLiteral_FALSE = (Core.Name "FALSE")

_BooleanLiteral_UNKNOWN = (Core.Name "UNKNOWN")

newtype BooleanPredicand = 
  BooleanPredicand {
    unBooleanPredicand :: ()}
  deriving (Eq, Ord, Read, Show)

_BooleanPredicand = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPredicand")

data BooleanPrimary = 
  BooleanPrimaryPredicate Predicate |
  BooleanPrimaryPredicand BooleanPredicand
  deriving (Eq, Ord, Read, Show)

_BooleanPrimary = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanPrimary")

_BooleanPrimary_predicate = (Core.Name "predicate")

_BooleanPrimary_predicand = (Core.Name "predicand")

data BooleanTerm = 
  BooleanTermFactor BooleanFactor |
  BooleanTermAnd BooleanTerm_And
  deriving (Eq, Ord, Read, Show)

_BooleanTerm = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm")

_BooleanTerm_factor = (Core.Name "factor")

_BooleanTerm_and = (Core.Name "and")

data BooleanTerm_And = 
  BooleanTerm_And {
    booleanTerm_AndLhs :: BooleanTerm,
    booleanTerm_AndRhs :: BooleanFactor}
  deriving (Eq, Ord, Read, Show)

_BooleanTerm_And = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTerm_And")

_BooleanTerm_And_lhs = (Core.Name "lhs")

_BooleanTerm_And_rhs = (Core.Name "rhs")

data BooleanTest = 
  BooleanTest {
    booleanTestBooleanPrimary :: BooleanPrimary,
    booleanTestSequence :: (Maybe BooleanTest_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BooleanTest = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest")

_BooleanTest_BooleanPrimary = (Core.Name "BooleanPrimary")

_BooleanTest_Sequence = (Core.Name "Sequence")

data BooleanTest_Sequence_Option = 
  BooleanTest_Sequence_Option {
    booleanTest_Sequence_OptionNOT :: (Maybe ()),
    booleanTest_Sequence_OptionTruthValue :: TruthValue}
  deriving (Eq, Ord, Read, Show)

_BooleanTest_Sequence_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option")

_BooleanTest_Sequence_Option_NOT = (Core.Name "NOT")

_BooleanTest_Sequence_Option_TruthValue = (Core.Name "TruthValue")

newtype BooleanType = 
  BooleanType {
    unBooleanType :: ()}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanType")

data BooleanValueExpression = 
  BooleanValueExpressionTerm BooleanTerm |
  BooleanValueExpressionOr BooleanValueExpression_Or
  deriving (Eq, Ord, Read, Show)

_BooleanValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression")

_BooleanValueExpression_term = (Core.Name "term")

_BooleanValueExpression_or = (Core.Name "or")

data BooleanValueExpression_Or = 
  BooleanValueExpression_Or {
    booleanValueExpression_OrLhs :: BooleanValueExpression,
    booleanValueExpression_OrRhs :: BooleanTerm}
  deriving (Eq, Ord, Read, Show)

_BooleanValueExpression_Or = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanValueExpression_Or")

_BooleanValueExpression_Or_lhs = (Core.Name "lhs")

_BooleanValueExpression_Or_rhs = (Core.Name "rhs")

newtype CaseExpression = 
  CaseExpression {
    unCaseExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.CaseExpression")

newtype CastSpecification = 
  CastSpecification {
    unCastSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_CastSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.CastSpecification")

newtype CharacterSetSpecification = 
  CharacterSetSpecification {
    unCharacterSetSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_CharacterSetSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterSetSpecification")

data CharacterStringType = 
  CharacterStringTypeCharacter (Maybe Length) |
  CharacterStringTypeChar (Maybe Length) |
  CharacterStringTypeCharacterVarying Length |
  CharacterStringTypeCharVarying Length |
  CharacterStringTypeVarchar Length |
  CharacterStringTypeCharacterLargeObject (Maybe LargeObjectLength) |
  CharacterStringTypeCharLargeObject (Maybe LargeObjectLength) |
  CharacterStringTypeClob (Maybe LargeObjectLength)
  deriving (Eq, Ord, Read, Show)

_CharacterStringType = (Core.Name "hydra.ext.org.ansi.sql.syntax.CharacterStringType")

_CharacterStringType_character = (Core.Name "character")

_CharacterStringType_char = (Core.Name "char")

_CharacterStringType_characterVarying = (Core.Name "characterVarying")

_CharacterStringType_charVarying = (Core.Name "charVarying")

_CharacterStringType_varchar = (Core.Name "varchar")

_CharacterStringType_characterLargeObject = (Core.Name "characterLargeObject")

_CharacterStringType_charLargeObject = (Core.Name "charLargeObject")

_CharacterStringType_clob = (Core.Name "clob")

newtype CollateClause = 
  CollateClause {
    unCollateClause :: ()}
  deriving (Eq, Ord, Read, Show)

_CollateClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollateClause")

data CollectionType = 
  CollectionTypeArray ArrayType |
  CollectionTypeMultiset MultisetType
  deriving (Eq, Ord, Read, Show)

_CollectionType = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionType")

_CollectionType_array = (Core.Name "array")

_CollectionType_multiset = (Core.Name "multiset")

data CollectionValueConstructor = 
  CollectionValueConstructorArray ArrayValueConstructor |
  CollectionValueConstructorMultiset MultisetValueConstructor
  deriving (Eq, Ord, Read, Show)

_CollectionValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueConstructor")

_CollectionValueConstructor_array = (Core.Name "array")

_CollectionValueConstructor_multiset = (Core.Name "multiset")

data CollectionValueExpression = 
  CollectionValueExpressionArray ArrayValueExpression |
  CollectionValueExpressionMultiset MultisetValueExpression
  deriving (Eq, Ord, Read, Show)

_CollectionValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.CollectionValueExpression")

_CollectionValueExpression_array = (Core.Name "array")

_CollectionValueExpression_multiset = (Core.Name "multiset")

newtype ColumnConstraintDefinition = 
  ColumnConstraintDefinition {
    unColumnConstraintDefinition :: ()}
  deriving (Eq, Ord, Read, Show)

_ColumnConstraintDefinition = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnConstraintDefinition")

data ColumnDefinition = 
  ColumnDefinition {
    columnDefinitionName :: ColumnName,
    columnDefinitionTypeOrDomain :: (Maybe ColumnDefinition_TypeOrDomain_Option),
    columnDefinitionRefScope :: (Maybe ReferenceScopeCheck),
    columnDefinitionDefaultOrIdentityOrGeneration :: (Maybe ColumnDefinition_DefaultOrIdentityOrGeneration_Option),
    columnDefinitionConstraints :: [ColumnConstraintDefinition],
    columnDefinitionCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition")

_ColumnDefinition_name = (Core.Name "name")

_ColumnDefinition_typeOrDomain = (Core.Name "typeOrDomain")

_ColumnDefinition_refScope = (Core.Name "refScope")

_ColumnDefinition_defaultOrIdentityOrGeneration = (Core.Name "defaultOrIdentityOrGeneration")

_ColumnDefinition_constraints = (Core.Name "constraints")

_ColumnDefinition_collate = (Core.Name "collate")

data ColumnDefinition_TypeOrDomain_Option = 
  ColumnDefinition_TypeOrDomain_OptionDataType DataType |
  ColumnDefinition_TypeOrDomain_OptionDomainName DomainName
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_TypeOrDomain_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option")

_ColumnDefinition_TypeOrDomain_Option_DataType = (Core.Name "DataType")

_ColumnDefinition_TypeOrDomain_Option_DomainName = (Core.Name "DomainName")

data ColumnDefinition_DefaultOrIdentityOrGeneration_Option = 
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause DefaultClause |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification IdentityColumnSpecification |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause GenerationClause
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_DefaultClause = (Core.Name "DefaultClause")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_IdentityColumnSpecification = (Core.Name "IdentityColumnSpecification")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_GenerationClause = (Core.Name "GenerationClause")

data ColumnNameList = 
  ColumnNameList {
    columnNameListFirst :: ColumnName,
    columnNameListRest :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_ColumnNameList = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList")

_ColumnNameList_first = (Core.Name "first")

_ColumnNameList_rest = (Core.Name "rest")

newtype ColumnOptions = 
  ColumnOptions {
    unColumnOptions :: ()}
  deriving (Eq, Ord, Read, Show)

_ColumnOptions = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnOptions")

newtype ColumnReference = 
  ColumnReference {
    unColumnReference :: ()}
  deriving (Eq, Ord, Read, Show)

_ColumnReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnReference")

data CommonValueExpression = 
  CommonValueExpressionNumeric NumericValueExpression |
  CommonValueExpressionString StringValueExpression |
  CommonValueExpressionDatetime DatetimeValueExpression |
  CommonValueExpressionInterval IntervalValueExpression |
  CommonValueExpressionUserDefined UserDefinedTypeValueExpression |
  CommonValueExpressionReference ReferenceValueExpression |
  CommonValueExpressionCollection CollectionValueExpression
  deriving (Eq, Ord, Read, Show)

_CommonValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.CommonValueExpression")

_CommonValueExpression_numeric = (Core.Name "numeric")

_CommonValueExpression_string = (Core.Name "string")

_CommonValueExpression_datetime = (Core.Name "datetime")

_CommonValueExpression_interval = (Core.Name "interval")

_CommonValueExpression_userDefined = (Core.Name "userDefined")

_CommonValueExpression_reference = (Core.Name "reference")

_CommonValueExpression_collection = (Core.Name "collection")

data ContextuallyTypedRowValueExpression = 
  ContextuallyTypedRowValueExpressionSpecialCase RowValueSpecialCase |
  ContextuallyTypedRowValueExpressionConstructor ContextuallyTypedRowValueConstructor
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpression")

_ContextuallyTypedRowValueExpression_specialCase = (Core.Name "specialCase")

_ContextuallyTypedRowValueExpression_constructor = (Core.Name "constructor")

newtype ContextuallyTypedRowValueConstructor = 
  ContextuallyTypedRowValueConstructor {
    unContextuallyTypedRowValueConstructor :: ()}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueConstructor")

data ContextuallyTypedRowValueExpressionList = 
  ContextuallyTypedRowValueExpressionList {
    contextuallyTypedRowValueExpressionListFirst :: ContextuallyTypedRowValueExpression,
    contextuallyTypedRowValueExpressionListRest :: [ContextuallyTypedRowValueExpression]}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueExpressionList = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedRowValueExpressionList")

_ContextuallyTypedRowValueExpressionList_first = (Core.Name "first")

_ContextuallyTypedRowValueExpressionList_rest = (Core.Name "rest")

newtype ContextuallyTypedTableValueConstructor = 
  ContextuallyTypedTableValueConstructor {
    unContextuallyTypedTableValueConstructor :: ContextuallyTypedRowValueExpressionList}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedTableValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.ContextuallyTypedTableValueConstructor")

data DataType = 
  DataTypePredefined PredefinedType |
  DataTypeRow RowType |
  DataTypeNamed PathResolvedUserDefinedTypeName |
  DataTypeReference ReferenceType |
  DataTypeCollection CollectionType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra.ext.org.ansi.sql.syntax.DataType")

_DataType_predefined = (Core.Name "predefined")

_DataType_row = (Core.Name "row")

_DataType_named = (Core.Name "named")

_DataType_reference = (Core.Name "reference")

_DataType_collection = (Core.Name "collection")

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: DateString}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.DateLiteral")

data DatetimeLiteral = 
  DatetimeLiteralDate DateLiteral |
  DatetimeLiteralTime TimeLiteral |
  DatetimeLiteralTimestamp TimestampLiteral
  deriving (Eq, Ord, Read, Show)

_DatetimeLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeLiteral")

_DatetimeLiteral_date = (Core.Name "date")

_DatetimeLiteral_time = (Core.Name "time")

_DatetimeLiteral_timestamp = (Core.Name "timestamp")

newtype DatetimeType = 
  DatetimeType {
    unDatetimeType :: ()}
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeType")

newtype DatetimeValueExpression = 
  DatetimeValueExpression {
    unDatetimeValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression")

newtype DefaultClause = 
  DefaultClause {
    unDefaultClause :: ()}
  deriving (Eq, Ord, Read, Show)

_DefaultClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.DefaultClause")

data ExactNumericType = 
  ExactNumericTypeNumeric (Maybe ExactNumericType_Numeric_Option) |
  ExactNumericTypeDecimal (Maybe ExactNumericType_Decimal_Option) |
  ExactNumericTypeDec (Maybe ExactNumericType_Dec_Option) |
  ExactNumericTypeSmallint  |
  ExactNumericTypeInteger  |
  ExactNumericTypeInt  |
  ExactNumericTypeBigint 
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType")

_ExactNumericType_numeric = (Core.Name "numeric")

_ExactNumericType_decimal = (Core.Name "decimal")

_ExactNumericType_dec = (Core.Name "dec")

_ExactNumericType_smallint = (Core.Name "smallint")

_ExactNumericType_integer = (Core.Name "integer")

_ExactNumericType_int = (Core.Name "int")

_ExactNumericType_bigint = (Core.Name "bigint")

data ExactNumericType_Numeric_Option = 
  ExactNumericType_Numeric_Option {
    exactNumericType_Numeric_OptionPrecision :: Precision,
    exactNumericType_Numeric_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Numeric_Option")

_ExactNumericType_Numeric_Option_Precision = (Core.Name "Precision")

_ExactNumericType_Numeric_Option_Sequence = (Core.Name "Sequence")

data ExactNumericType_Decimal_Option = 
  ExactNumericType_Decimal_Option {
    exactNumericType_Decimal_OptionPrecision :: Precision,
    exactNumericType_Decimal_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option")

_ExactNumericType_Decimal_Option_Precision = (Core.Name "Precision")

_ExactNumericType_Decimal_Option_Sequence = (Core.Name "Sequence")

data ExactNumericType_Dec_Option = 
  ExactNumericType_Dec_Option {
    exactNumericType_Dec_OptionPrecision :: Precision,
    exactNumericType_Dec_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option")

_ExactNumericType_Dec_Option_Precision = (Core.Name "Precision")

_ExactNumericType_Dec_Option_Sequence = (Core.Name "Sequence")

newtype FieldReference = 
  FieldReference {
    unFieldReference :: ()}
  deriving (Eq, Ord, Read, Show)

_FieldReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.FieldReference")

data FromConstructor = 
  FromConstructor {
    fromConstructorColumns :: (Maybe InsertColumnList),
    fromConstructorOverride :: (Maybe OverrideClause),
    fromConstructorValues :: ContextuallyTypedTableValueConstructor}
  deriving (Eq, Ord, Read, Show)

_FromConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromConstructor")

_FromConstructor_columns = (Core.Name "columns")

_FromConstructor_override = (Core.Name "override")

_FromConstructor_values = (Core.Name "values")

newtype FromDefault = 
  FromDefault {
    unFromDefault :: ()}
  deriving (Eq, Ord, Read, Show)

_FromDefault = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromDefault")

newtype FromSubquery = 
  FromSubquery {
    unFromSubquery :: ()}
  deriving (Eq, Ord, Read, Show)

_FromSubquery = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromSubquery")

data GeneralLiteral = 
  GeneralLiteralString CharacterStringLiteral |
  GeneralLiteralNationalString NationalCharacterStringLiteral |
  GeneralLiteralUnicode UnicodeCharacterStringLiteral |
  GeneralLiteralBinary BinaryStringLiteral |
  GeneralLiteralDateTime DatetimeLiteral |
  GeneralLiteralInterval IntervalLiteral |
  GeneralLiteralBoolean BooleanLiteral
  deriving (Eq, Ord, Read, Show)

_GeneralLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralLiteral")

_GeneralLiteral_string = (Core.Name "string")

_GeneralLiteral_nationalString = (Core.Name "nationalString")

_GeneralLiteral_unicode = (Core.Name "unicode")

_GeneralLiteral_binary = (Core.Name "binary")

_GeneralLiteral_dateTime = (Core.Name "dateTime")

_GeneralLiteral_interval = (Core.Name "interval")

_GeneralLiteral_boolean = (Core.Name "boolean")

newtype GeneralValueSpecification = 
  GeneralValueSpecification {
    unGeneralValueSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_GeneralValueSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralValueSpecification")

newtype GenerationClause = 
  GenerationClause {
    unGenerationClause :: ()}
  deriving (Eq, Ord, Read, Show)

_GenerationClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.GenerationClause")

data GlobalOrLocal = 
  GlobalOrLocalGlobal  |
  GlobalOrLocalLocal 
  deriving (Eq, Ord, Read, Show)

_GlobalOrLocal = (Core.Name "hydra.ext.org.ansi.sql.syntax.GlobalOrLocal")

_GlobalOrLocal_global = (Core.Name "global")

_GlobalOrLocal_local = (Core.Name "local")

newtype IdentityColumnSpecification = 
  IdentityColumnSpecification {
    unIdentityColumnSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_IdentityColumnSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.IdentityColumnSpecification")

newtype InsertColumnList = 
  InsertColumnList {
    unInsertColumnList :: ColumnNameList}
  deriving (Eq, Ord, Read, Show)

_InsertColumnList = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnList")

data InsertColumnsAndSource = 
  InsertColumnsAndSourceSubquery FromSubquery |
  InsertColumnsAndSourceConstructor FromConstructor |
  InsertColumnsAndSourceDefault FromDefault
  deriving (Eq, Ord, Read, Show)

_InsertColumnsAndSource = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertColumnsAndSource")

_InsertColumnsAndSource_subquery = (Core.Name "subquery")

_InsertColumnsAndSource_constructor = (Core.Name "constructor")

_InsertColumnsAndSource_default = (Core.Name "default")

data InsertStatement = 
  InsertStatement {
    insertStatementTarget :: InsertionTarget,
    insertStatementColumnsAndSource :: InsertColumnsAndSource}
  deriving (Eq, Ord, Read, Show)

_InsertStatement = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertStatement")

_InsertStatement_target = (Core.Name "target")

_InsertStatement_columnsAndSource = (Core.Name "columnsAndSource")

newtype InsertionTarget = 
  InsertionTarget {
    unInsertionTarget :: TableName}
  deriving (Eq, Ord, Read, Show)

_InsertionTarget = (Core.Name "hydra.ext.org.ansi.sql.syntax.InsertionTarget")

newtype IntervalLiteral = 
  IntervalLiteral {
    unIntervalLiteral :: ()}
  deriving (Eq, Ord, Read, Show)

_IntervalLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalLiteral")

newtype IntervalType = 
  IntervalType {
    unIntervalType :: ()}
  deriving (Eq, Ord, Read, Show)

_IntervalType = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalType")

newtype IntervalValueExpression = 
  IntervalValueExpression {
    unIntervalValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_IntervalValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalValueExpression")

newtype LargeObjectLength = 
  LargeObjectLength {
    unLargeObjectLength :: ()}
  deriving (Eq, Ord, Read, Show)

_LargeObjectLength = (Core.Name "hydra.ext.org.ansi.sql.syntax.LargeObjectLength")

newtype Length = 
  Length {
    unLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Length = (Core.Name "hydra.ext.org.ansi.sql.syntax.Length")

newtype LikeClause = 
  LikeClause {
    unLikeClause :: ()}
  deriving (Eq, Ord, Read, Show)

_LikeClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.LikeClause")

newtype MethodInvocation = 
  MethodInvocation {
    unMethodInvocation :: ()}
  deriving (Eq, Ord, Read, Show)

_MethodInvocation = (Core.Name "hydra.ext.org.ansi.sql.syntax.MethodInvocation")

newtype MultisetElementReference = 
  MultisetElementReference {
    unMultisetElementReference :: ()}
  deriving (Eq, Ord, Read, Show)

_MultisetElementReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetElementReference")

newtype MultisetType = 
  MultisetType {
    unMultisetType :: DataType}
  deriving (Eq, Ord, Read, Show)

_MultisetType = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetType")

newtype MultisetValueConstructor = 
  MultisetValueConstructor {
    unMultisetValueConstructor :: ()}
  deriving (Eq, Ord, Read, Show)

_MultisetValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueConstructor")

newtype MultisetValueExpression = 
  MultisetValueExpression {
    unMultisetValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_MultisetValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueExpression")

newtype NationalCharacterStringType = 
  NationalCharacterStringType {
    unNationalCharacterStringType :: ()}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringType = (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringType")

newtype NewSpecification = 
  NewSpecification {
    unNewSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_NewSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.NewSpecification")

newtype NextValueExpression = 
  NextValueExpression {
    unNextValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_NextValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.NextValueExpression")

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericType")

_NumericType_exact = (Core.Name "exact")

_NumericType_approximate = (Core.Name "approximate")

newtype NumericValueExpression = 
  NumericValueExpression {
    unNumericValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericValueExpression")

data OverrideClause = 
  OverrideClauseOVERRIDINGspUSERspVALUE  |
  OverrideClauseOVERRIDINGspSYSTEMspVALUE 
  deriving (Eq, Ord, Read, Show)

_OverrideClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.OverrideClause")

_OverrideClause_OVERRIDINGspUSERspVALUE = (Core.Name "OVERRIDINGspUSERspVALUE")

_OverrideClause_OVERRIDINGspSYSTEMspVALUE = (Core.Name "OVERRIDINGspSYSTEMspVALUE")

newtype ParenthesizedValueExpression = 
  ParenthesizedValueExpression {
    unParenthesizedValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ParenthesizedValueExpression")

newtype Precision = 
  Precision {
    unPrecision :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra.ext.org.ansi.sql.syntax.Precision")

data PredefinedType = 
  PredefinedTypeString PredefinedType_String |
  PredefinedTypeNationalString PredefinedType_NationalString |
  PredefinedTypeBlob BinaryLargeObjectStringType |
  PredefinedTypeNumeric NumericType |
  PredefinedTypeBoolean BooleanType |
  PredefinedTypeDatetime DatetimeType |
  PredefinedTypeInterval IntervalType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType")

_PredefinedType_string = (Core.Name "string")

_PredefinedType_nationalString = (Core.Name "nationalString")

_PredefinedType_blob = (Core.Name "blob")

_PredefinedType_numeric = (Core.Name "numeric")

_PredefinedType_boolean = (Core.Name "boolean")

_PredefinedType_datetime = (Core.Name "datetime")

_PredefinedType_interval = (Core.Name "interval")

data PredefinedType_String = 
  PredefinedType_String {
    predefinedType_StringType :: CharacterStringType,
    predefinedType_StringCharacters :: (Maybe CharacterSetSpecification),
    predefinedType_StringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_String = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_String")

_PredefinedType_String_type = (Core.Name "type")

_PredefinedType_String_characters = (Core.Name "characters")

_PredefinedType_String_collate = (Core.Name "collate")

data PredefinedType_NationalString = 
  PredefinedType_NationalString {
    predefinedType_NationalStringType :: NationalCharacterStringType,
    predefinedType_NationalStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_NationalString = (Core.Name "hydra.ext.org.ansi.sql.syntax.PredefinedType_NationalString")

_PredefinedType_NationalString_type = (Core.Name "type")

_PredefinedType_NationalString_collate = (Core.Name "collate")

newtype Predicate = 
  Predicate {
    unPredicate :: ()}
  deriving (Eq, Ord, Read, Show)

_Predicate = (Core.Name "hydra.ext.org.ansi.sql.syntax.Predicate")

newtype QueryExpression = 
  QueryExpression {
    unQueryExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_QueryExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.QueryExpression")

newtype ReferenceScopeCheck = 
  ReferenceScopeCheck {
    unReferenceScopeCheck :: ()}
  deriving (Eq, Ord, Read, Show)

_ReferenceScopeCheck = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceScopeCheck")

newtype ReferenceType = 
  ReferenceType {
    unReferenceType :: ()}
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceType")

newtype RowType = 
  RowType {
    unRowType :: ()}
  deriving (Eq, Ord, Read, Show)

_RowType = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowType")

newtype RowValueSpecialCase = 
  RowValueSpecialCase {
    unRowValueSpecialCase :: NonparenthesizedValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_RowValueSpecialCase = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueSpecialCase")

data NonparenthesizedValueExpressionPrimary = 
  NonparenthesizedValueExpressionPrimaryUnsigned UnsignedValueSpecification |
  NonparenthesizedValueExpressionPrimaryColumn ColumnReference |
  NonparenthesizedValueExpressionPrimarySetFunction SetFunctionSpecification |
  NonparenthesizedValueExpressionPrimaryWindowFunction WindowFunction |
  NonparenthesizedValueExpressionPrimaryScalarSubquery ScalarSubquery |
  NonparenthesizedValueExpressionPrimaryCases CaseExpression |
  NonparenthesizedValueExpressionPrimaryCast CastSpecification |
  NonparenthesizedValueExpressionPrimaryField FieldReference |
  NonparenthesizedValueExpressionPrimarySubtype SubtypeTreatment |
  NonparenthesizedValueExpressionPrimaryMethod MethodInvocation |
  NonparenthesizedValueExpressionPrimaryStaticMethod StaticMethodInvocation |
  NonparenthesizedValueExpressionPrimaryNew NewSpecification |
  NonparenthesizedValueExpressionPrimaryAttributeOrMethod AttributeOrMethodReference |
  NonparenthesizedValueExpressionPrimaryReference ReferenceResolution |
  NonparenthesizedValueExpressionPrimaryCollection CollectionValueConstructor |
  NonparenthesizedValueExpressionPrimaryArrayElement ArrayElementReference |
  NonparenthesizedValueExpressionPrimaryMultisetElement MultisetElementReference |
  NonparenthesizedValueExpressionPrimaryRoutine RoutineInvocation |
  NonparenthesizedValueExpressionPrimaryNext NextValueExpression
  deriving (Eq, Ord, Read, Show)

_NonparenthesizedValueExpressionPrimary = (Core.Name "hydra.ext.org.ansi.sql.syntax.NonparenthesizedValueExpressionPrimary")

_NonparenthesizedValueExpressionPrimary_unsigned = (Core.Name "unsigned")

_NonparenthesizedValueExpressionPrimary_column = (Core.Name "column")

_NonparenthesizedValueExpressionPrimary_setFunction = (Core.Name "setFunction")

_NonparenthesizedValueExpressionPrimary_windowFunction = (Core.Name "windowFunction")

_NonparenthesizedValueExpressionPrimary_scalarSubquery = (Core.Name "scalarSubquery")

_NonparenthesizedValueExpressionPrimary_cases = (Core.Name "cases")

_NonparenthesizedValueExpressionPrimary_cast = (Core.Name "cast")

_NonparenthesizedValueExpressionPrimary_field = (Core.Name "field")

_NonparenthesizedValueExpressionPrimary_subtype = (Core.Name "subtype")

_NonparenthesizedValueExpressionPrimary_method = (Core.Name "method")

_NonparenthesizedValueExpressionPrimary_staticMethod = (Core.Name "staticMethod")

_NonparenthesizedValueExpressionPrimary_new = (Core.Name "new")

_NonparenthesizedValueExpressionPrimary_attributeOrMethod = (Core.Name "attributeOrMethod")

_NonparenthesizedValueExpressionPrimary_reference = (Core.Name "reference")

_NonparenthesizedValueExpressionPrimary_collection = (Core.Name "collection")

_NonparenthesizedValueExpressionPrimary_arrayElement = (Core.Name "arrayElement")

_NonparenthesizedValueExpressionPrimary_multisetElement = (Core.Name "multisetElement")

_NonparenthesizedValueExpressionPrimary_routine = (Core.Name "routine")

_NonparenthesizedValueExpressionPrimary_next = (Core.Name "next")

newtype ReferenceResolution = 
  ReferenceResolution {
    unReferenceResolution :: ()}
  deriving (Eq, Ord, Read, Show)

_ReferenceResolution = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceResolution")

newtype ReferenceValueExpression = 
  ReferenceValueExpression {
    unReferenceValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_ReferenceValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression")

newtype RowValueExpression = 
  RowValueExpression {
    unRowValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_RowValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueExpression")

newtype RoutineInvocation = 
  RoutineInvocation {
    unRoutineInvocation :: ()}
  deriving (Eq, Ord, Read, Show)

_RoutineInvocation = (Core.Name "hydra.ext.org.ansi.sql.syntax.RoutineInvocation")

newtype ScalarSubquery = 
  ScalarSubquery {
    unScalarSubquery :: Subquery}
  deriving (Eq, Ord, Read, Show)

_ScalarSubquery = (Core.Name "hydra.ext.org.ansi.sql.syntax.ScalarSubquery")

newtype Scale = 
  Scale {
    unScale :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Scale = (Core.Name "hydra.ext.org.ansi.sql.syntax.Scale")

newtype SelfReferencingColumnSpecification = 
  SelfReferencingColumnSpecification {
    unSelfReferencingColumnSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_SelfReferencingColumnSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification")

newtype SetFunctionSpecification = 
  SetFunctionSpecification {
    unSetFunctionSpecification :: ()}
  deriving (Eq, Ord, Read, Show)

_SetFunctionSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.SetFunctionSpecification")

newtype StaticMethodInvocation = 
  StaticMethodInvocation {
    unStaticMethodInvocation :: ()}
  deriving (Eq, Ord, Read, Show)

_StaticMethodInvocation = (Core.Name "hydra.ext.org.ansi.sql.syntax.StaticMethodInvocation")

newtype StringValueExpression = 
  StringValueExpression {
    unStringValueExpression :: ()}
  deriving (Eq, Ord, Read, Show)

_StringValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.StringValueExpression")

newtype Subquery = 
  Subquery {
    unSubquery :: QueryExpression}
  deriving (Eq, Ord, Read, Show)

_Subquery = (Core.Name "hydra.ext.org.ansi.sql.syntax.Subquery")

newtype SubtableClause = 
  SubtableClause {
    unSubtableClause :: ()}
  deriving (Eq, Ord, Read, Show)

_SubtableClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtableClause")

newtype SubtypeTreatment = 
  SubtypeTreatment {
    unSubtypeTreatment :: ()}
  deriving (Eq, Ord, Read, Show)

_SubtypeTreatment = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtypeTreatment")

data TableCommitAction = 
  TableCommitActionPreserve  |
  TableCommitActionDelete 
  deriving (Eq, Ord, Read, Show)

_TableCommitAction = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableCommitAction")

_TableCommitAction_preserve = (Core.Name "preserve")

_TableCommitAction_delete = (Core.Name "delete")

newtype TableConstraintDefinition = 
  TableConstraintDefinition {
    unTableConstraintDefinition :: ()}
  deriving (Eq, Ord, Read, Show)

_TableConstraintDefinition = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableConstraintDefinition")

data TableContentsSource = 
  TableContentsSourceList TableElementList |
  TableContentsSourceSubtable TableContentsSource_Subtable |
  TableContentsSourceSubquery AsSubqueryClause
  deriving (Eq, Ord, Read, Show)

_TableContentsSource = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource")

_TableContentsSource_list = (Core.Name "list")

_TableContentsSource_subtable = (Core.Name "subtable")

_TableContentsSource_subquery = (Core.Name "subquery")

data TableContentsSource_Subtable = 
  TableContentsSource_Subtable {
    tableContentsSource_SubtableType :: PathResolvedUserDefinedTypeName,
    tableContentsSource_SubtableSubtable :: (Maybe SubtableClause),
    tableContentsSource_SubtableElements :: (Maybe TableElementList)}
  deriving (Eq, Ord, Read, Show)

_TableContentsSource_Subtable = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableContentsSource_Subtable")

_TableContentsSource_Subtable_type = (Core.Name "type")

_TableContentsSource_Subtable_subtable = (Core.Name "subtable")

_TableContentsSource_Subtable_elements = (Core.Name "elements")

data TableDefinition = 
  TableDefinition {
    tableDefinitionScope :: (Maybe TableScope),
    tableDefinitionName :: TableName,
    tableDefinitionSource :: TableContentsSource,
    tableDefinitionCommitActions :: (Maybe TableCommitAction)}
  deriving (Eq, Ord, Read, Show)

_TableDefinition = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableDefinition")

_TableDefinition_scope = (Core.Name "scope")

_TableDefinition_name = (Core.Name "name")

_TableDefinition_source = (Core.Name "source")

_TableDefinition_commitActions = (Core.Name "commitActions")

data TableElement = 
  TableElementColumn ColumnDefinition |
  TableElementTableConstraint TableConstraintDefinition |
  TableElementLike LikeClause |
  TableElementSelfReferencingColumn SelfReferencingColumnSpecification |
  TableElementColumOptions ColumnOptions
  deriving (Eq, Ord, Read, Show)

_TableElement = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElement")

_TableElement_column = (Core.Name "column")

_TableElement_tableConstraint = (Core.Name "tableConstraint")

_TableElement_like = (Core.Name "like")

_TableElement_selfReferencingColumn = (Core.Name "selfReferencingColumn")

_TableElement_columOptions = (Core.Name "columOptions")

data TableElementList = 
  TableElementList {
    tableElementListFirst :: TableElement,
    tableElementListRest :: [TableElement]}
  deriving (Eq, Ord, Read, Show)

_TableElementList = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableElementList")

_TableElementList_first = (Core.Name "first")

_TableElementList_rest = (Core.Name "rest")

newtype TableScope = 
  TableScope {
    unTableScope :: GlobalOrLocal}
  deriving (Eq, Ord, Read, Show)

_TableScope = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableScope")

newtype TimeLiteral = 
  TimeLiteral {
    unTimeLiteral :: TimeString}
  deriving (Eq, Ord, Read, Show)

_TimeLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeLiteral")

data TruthValue = 
  TruthValueTRUE  |
  TruthValueFALSE  |
  TruthValueUNKNOWN 
  deriving (Eq, Ord, Read, Show)

_TruthValue = (Core.Name "hydra.ext.org.ansi.sql.syntax.TruthValue")

_TruthValue_TRUE = (Core.Name "TRUE")

_TruthValue_FALSE = (Core.Name "FALSE")

_TruthValue_UNKNOWN = (Core.Name "UNKNOWN")

data UnsignedLiteral = 
  UnsignedLiteralNumeric UnsignedNumericLiteral |
  UnsignedLiteralGeneral GeneralLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedLiteral")

_UnsignedLiteral_numeric = (Core.Name "numeric")

_UnsignedLiteral_general = (Core.Name "general")

data UnsignedNumericLiteral = 
  UnsignedNumericLiteralExact ExactNumericLiteral |
  UnsignedNumericLiteralApproximate ApproximateNumericLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedNumericLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedNumericLiteral")

_UnsignedNumericLiteral_exact = (Core.Name "exact")

_UnsignedNumericLiteral_approximate = (Core.Name "approximate")

data UnsignedValueSpecification = 
  UnsignedValueSpecificationLiteral UnsignedLiteral |
  UnsignedValueSpecificationGeneral GeneralValueSpecification
  deriving (Eq, Ord, Read, Show)

_UnsignedValueSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.UnsignedValueSpecification")

_UnsignedValueSpecification_literal = (Core.Name "literal")

_UnsignedValueSpecification_general = (Core.Name "general")

newtype UserDefinedTypeValueExpression = 
  UserDefinedTypeValueExpression {
    unUserDefinedTypeValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_UserDefinedTypeValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.UserDefinedTypeValueExpression")

data ValueExpression = 
  ValueExpressionCommon CommonValueExpression |
  ValueExpressionBoolean BooleanValueExpression |
  ValueExpressionRow RowValueExpression
  deriving (Eq, Ord, Read, Show)

_ValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpression")

_ValueExpression_common = (Core.Name "common")

_ValueExpression_boolean = (Core.Name "boolean")

_ValueExpression_row = (Core.Name "row")

data ValueExpressionPrimary = 
  ValueExpressionPrimaryParens ParenthesizedValueExpression |
  ValueExpressionPrimaryNoparens NonparenthesizedValueExpressionPrimary
  deriving (Eq, Ord, Read, Show)

_ValueExpressionPrimary = (Core.Name "hydra.ext.org.ansi.sql.syntax.ValueExpressionPrimary")

_ValueExpressionPrimary_parens = (Core.Name "parens")

_ValueExpressionPrimary_noparens = (Core.Name "noparens")

newtype WindowFunction = 
  WindowFunction {
    unWindowFunction :: ()}
  deriving (Eq, Ord, Read, Show)

_WindowFunction = (Core.Name "hydra.ext.org.ansi.sql.syntax.WindowFunction")
