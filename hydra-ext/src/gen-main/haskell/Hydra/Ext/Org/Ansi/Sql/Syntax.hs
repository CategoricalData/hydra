-- | A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003

module Hydra.Ext.Org.Ansi.Sql.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype ApproximateNumericLiteral = 
  ApproximateNumericLiteral {
    unApproximateNumericLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.ApproximateNumericLiteral")

data BinaryStringLiteral = 
  BinaryStringLiteral {}
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

data DateString = 
  DateString {}
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

data NationalCharacterStringLiteral = 
  NationalCharacterStringLiteral {}
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

data TimeString = 
  TimeString {}
  deriving (Eq, Ord, Read, Show)

_TimeString = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimeString")

data TimestampLiteral = 
  TimestampLiteral {}
  deriving (Eq, Ord, Read, Show)

_TimestampLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.TimestampLiteral")

data UnicodeCharacterStringLiteral = 
  UnicodeCharacterStringLiteral {}
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

data ArrayElementReference = 
  ArrayElementReference {}
  deriving (Eq, Ord, Read, Show)

_ArrayElementReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayElementReference")

data ArrayType = 
  ArrayType {}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayType")

data ArrayValueConstructor = 
  ArrayValueConstructorEnumeration ArrayValueConstructorByEnumeration |
  ArrayValueConstructorQuery ArrayValueConstructorByQuery
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructor")

_ArrayValueConstructor_enumeration = (Core.Name "enumeration")

_ArrayValueConstructor_query = (Core.Name "query")

data ArrayValueConstructorByQuery = 
  ArrayValueConstructorByQuery {}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByQuery = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByQuery")

data ArrayValueConstructorByEnumeration = 
  ArrayValueConstructorByEnumeration {
    arrayValueConstructorByEnumerationLeftBracketOrTrigraph :: LeftBracketOrTrigraph,
    arrayValueConstructorByEnumerationArrayElementList :: ArrayElementList,
    arrayValueConstructorByEnumerationRightBracketOrTrigraph :: RightBracketOrTrigraph}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByEnumeration = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueConstructorByEnumeration")

_ArrayValueConstructorByEnumeration_leftBracketOrTrigraph = (Core.Name "leftBracketOrTrigraph")

_ArrayValueConstructorByEnumeration_arrayElementList = (Core.Name "arrayElementList")

_ArrayValueConstructorByEnumeration_rightBracketOrTrigraph = (Core.Name "rightBracketOrTrigraph")

data ArrayValueExpression = 
  ArrayValueExpression {}
  deriving (Eq, Ord, Read, Show)

_ArrayValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ArrayValueExpression")

data AsSubqueryClause = 
  AsSubqueryClause {}
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.AsSubqueryClause")

data AttributeOrMethodReference = 
  AttributeOrMethodReference {}
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

_BooleanFactor_nOT = (Core.Name "nOT")

_BooleanFactor_booleanTest = (Core.Name "booleanTest")

data BooleanLiteral = 
  BooleanLiteralTRUE  |
  BooleanLiteralFALSE  |
  BooleanLiteralUNKNOWN 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanLiteral")

_BooleanLiteral_tRUE = (Core.Name "tRUE")

_BooleanLiteral_fALSE = (Core.Name "fALSE")

_BooleanLiteral_uNKNOWN = (Core.Name "uNKNOWN")

data BooleanPredicand = 
  BooleanPredicand {}
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

_BooleanTest_booleanPrimary = (Core.Name "booleanPrimary")

_BooleanTest_sequence = (Core.Name "sequence")

data BooleanTest_Sequence_Option = 
  BooleanTest_Sequence_Option {
    booleanTest_Sequence_OptionNOT :: (Maybe ()),
    booleanTest_Sequence_OptionTruthValue :: TruthValue}
  deriving (Eq, Ord, Read, Show)

_BooleanTest_Sequence_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.BooleanTest_Sequence_Option")

_BooleanTest_Sequence_Option_nOT = (Core.Name "nOT")

_BooleanTest_Sequence_Option_truthValue = (Core.Name "truthValue")

data BooleanType = 
  BooleanType {}
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

data CaseExpression = 
  CaseExpression {}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.CaseExpression")

data CastSpecification = 
  CastSpecification {}
  deriving (Eq, Ord, Read, Show)

_CastSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.CastSpecification")

data CharacterSetSpecification = 
  CharacterSetSpecification {}
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

data CollateClause = 
  CollateClause {}
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

data ColumnConstraintDefinition = 
  ColumnConstraintDefinition {}
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

_ColumnDefinition_TypeOrDomain_Option_dataType = (Core.Name "dataType")

_ColumnDefinition_TypeOrDomain_Option_domainName = (Core.Name "domainName")

data ColumnDefinition_DefaultOrIdentityOrGeneration_Option = 
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause DefaultClause |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification IdentityColumnSpecification |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause GenerationClause
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_defaultClause = (Core.Name "defaultClause")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_identityColumnSpecification = (Core.Name "identityColumnSpecification")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_generationClause = (Core.Name "generationClause")

data ColumnNameList = 
  ColumnNameList {
    columnNameListFirst :: ColumnName,
    columnNameListRest :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_ColumnNameList = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnNameList")

_ColumnNameList_first = (Core.Name "first")

_ColumnNameList_rest = (Core.Name "rest")

data ColumnOptions = 
  ColumnOptions {}
  deriving (Eq, Ord, Read, Show)

_ColumnOptions = (Core.Name "hydra.ext.org.ansi.sql.syntax.ColumnOptions")

data ColumnReference = 
  ColumnReference {}
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

data ContextuallyTypedRowValueConstructor = 
  ContextuallyTypedRowValueConstructor {}
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

data DatetimeType = 
  DatetimeType {}
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeType")

data DatetimeValueExpression = 
  DatetimeValueExpression {}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.DatetimeValueExpression")

data DefaultClause = 
  DefaultClause {}
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

_ExactNumericType_Numeric_Option_precision = (Core.Name "precision")

_ExactNumericType_Numeric_Option_sequence = (Core.Name "sequence")

data ExactNumericType_Decimal_Option = 
  ExactNumericType_Decimal_Option {
    exactNumericType_Decimal_OptionPrecision :: Precision,
    exactNumericType_Decimal_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Decimal_Option")

_ExactNumericType_Decimal_Option_precision = (Core.Name "precision")

_ExactNumericType_Decimal_Option_sequence = (Core.Name "sequence")

data ExactNumericType_Dec_Option = 
  ExactNumericType_Dec_Option {
    exactNumericType_Dec_OptionPrecision :: Precision,
    exactNumericType_Dec_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Option = (Core.Name "hydra.ext.org.ansi.sql.syntax.ExactNumericType_Dec_Option")

_ExactNumericType_Dec_Option_precision = (Core.Name "precision")

_ExactNumericType_Dec_Option_sequence = (Core.Name "sequence")

data FieldReference = 
  FieldReference {}
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

data FromDefault = 
  FromDefault {}
  deriving (Eq, Ord, Read, Show)

_FromDefault = (Core.Name "hydra.ext.org.ansi.sql.syntax.FromDefault")

data FromSubquery = 
  FromSubquery {}
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

data GeneralValueSpecification = 
  GeneralValueSpecification {}
  deriving (Eq, Ord, Read, Show)

_GeneralValueSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.GeneralValueSpecification")

data GenerationClause = 
  GenerationClause {}
  deriving (Eq, Ord, Read, Show)

_GenerationClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.GenerationClause")

data GlobalOrLocal = 
  GlobalOrLocalGlobal  |
  GlobalOrLocalLocal 
  deriving (Eq, Ord, Read, Show)

_GlobalOrLocal = (Core.Name "hydra.ext.org.ansi.sql.syntax.GlobalOrLocal")

_GlobalOrLocal_global = (Core.Name "global")

_GlobalOrLocal_local = (Core.Name "local")

data IdentityColumnSpecification = 
  IdentityColumnSpecification {}
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

data IntervalLiteral = 
  IntervalLiteral {}
  deriving (Eq, Ord, Read, Show)

_IntervalLiteral = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalLiteral")

data IntervalType = 
  IntervalType {}
  deriving (Eq, Ord, Read, Show)

_IntervalType = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalType")

data IntervalValueExpression = 
  IntervalValueExpression {}
  deriving (Eq, Ord, Read, Show)

_IntervalValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.IntervalValueExpression")

data LargeObjectLength = 
  LargeObjectLength {}
  deriving (Eq, Ord, Read, Show)

_LargeObjectLength = (Core.Name "hydra.ext.org.ansi.sql.syntax.LargeObjectLength")

newtype Length = 
  Length {
    unLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Length = (Core.Name "hydra.ext.org.ansi.sql.syntax.Length")

data LikeClause = 
  LikeClause {}
  deriving (Eq, Ord, Read, Show)

_LikeClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.LikeClause")

data MethodInvocation = 
  MethodInvocation {}
  deriving (Eq, Ord, Read, Show)

_MethodInvocation = (Core.Name "hydra.ext.org.ansi.sql.syntax.MethodInvocation")

data MultisetElementReference = 
  MultisetElementReference {}
  deriving (Eq, Ord, Read, Show)

_MultisetElementReference = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetElementReference")

newtype MultisetType = 
  MultisetType {
    unMultisetType :: DataType}
  deriving (Eq, Ord, Read, Show)

_MultisetType = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetType")

data MultisetValueConstructor = 
  MultisetValueConstructor {}
  deriving (Eq, Ord, Read, Show)

_MultisetValueConstructor = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueConstructor")

data MultisetValueExpression = 
  MultisetValueExpression {}
  deriving (Eq, Ord, Read, Show)

_MultisetValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.MultisetValueExpression")

data NationalCharacterStringType = 
  NationalCharacterStringType {}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringType = (Core.Name "hydra.ext.org.ansi.sql.syntax.NationalCharacterStringType")

data NewSpecification = 
  NewSpecification {}
  deriving (Eq, Ord, Read, Show)

_NewSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.NewSpecification")

data NextValueExpression = 
  NextValueExpression {}
  deriving (Eq, Ord, Read, Show)

_NextValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.NextValueExpression")

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericType")

_NumericType_exact = (Core.Name "exact")

_NumericType_approximate = (Core.Name "approximate")

data NumericValueExpression = 
  NumericValueExpression {}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.NumericValueExpression")

data OverrideClause = 
  OverrideClauseOVERRIDINGSpUSERSpVALUE  |
  OverrideClauseOVERRIDINGSpSYSTEMSpVALUE 
  deriving (Eq, Ord, Read, Show)

_OverrideClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.OverrideClause")

_OverrideClause_oVERRIDINGSpUSERSpVALUE = (Core.Name "oVERRIDINGSpUSERSpVALUE")

_OverrideClause_oVERRIDINGSpSYSTEMSpVALUE = (Core.Name "oVERRIDINGSpSYSTEMSpVALUE")

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

data Predicate = 
  Predicate {}
  deriving (Eq, Ord, Read, Show)

_Predicate = (Core.Name "hydra.ext.org.ansi.sql.syntax.Predicate")

data QueryExpression = 
  QueryExpression {}
  deriving (Eq, Ord, Read, Show)

_QueryExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.QueryExpression")

data ReferenceScopeCheck = 
  ReferenceScopeCheck {}
  deriving (Eq, Ord, Read, Show)

_ReferenceScopeCheck = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceScopeCheck")

data ReferenceType = 
  ReferenceType {}
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceType")

data RowType = 
  RowType {}
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

data ReferenceResolution = 
  ReferenceResolution {}
  deriving (Eq, Ord, Read, Show)

_ReferenceResolution = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceResolution")

newtype ReferenceValueExpression = 
  ReferenceValueExpression {
    unReferenceValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_ReferenceValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.ReferenceValueExpression")

data RowValueExpression = 
  RowValueExpression {}
  deriving (Eq, Ord, Read, Show)

_RowValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.RowValueExpression")

data RoutineInvocation = 
  RoutineInvocation {}
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

data SelfReferencingColumnSpecification = 
  SelfReferencingColumnSpecification {}
  deriving (Eq, Ord, Read, Show)

_SelfReferencingColumnSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.SelfReferencingColumnSpecification")

data SetFunctionSpecification = 
  SetFunctionSpecification {}
  deriving (Eq, Ord, Read, Show)

_SetFunctionSpecification = (Core.Name "hydra.ext.org.ansi.sql.syntax.SetFunctionSpecification")

data StaticMethodInvocation = 
  StaticMethodInvocation {}
  deriving (Eq, Ord, Read, Show)

_StaticMethodInvocation = (Core.Name "hydra.ext.org.ansi.sql.syntax.StaticMethodInvocation")

data StringValueExpression = 
  StringValueExpression {}
  deriving (Eq, Ord, Read, Show)

_StringValueExpression = (Core.Name "hydra.ext.org.ansi.sql.syntax.StringValueExpression")

newtype Subquery = 
  Subquery {
    unSubquery :: QueryExpression}
  deriving (Eq, Ord, Read, Show)

_Subquery = (Core.Name "hydra.ext.org.ansi.sql.syntax.Subquery")

data SubtableClause = 
  SubtableClause {}
  deriving (Eq, Ord, Read, Show)

_SubtableClause = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtableClause")

data SubtypeTreatment = 
  SubtypeTreatment {}
  deriving (Eq, Ord, Read, Show)

_SubtypeTreatment = (Core.Name "hydra.ext.org.ansi.sql.syntax.SubtypeTreatment")

data TableCommitAction = 
  TableCommitActionPreserve  |
  TableCommitActionDelete 
  deriving (Eq, Ord, Read, Show)

_TableCommitAction = (Core.Name "hydra.ext.org.ansi.sql.syntax.TableCommitAction")

_TableCommitAction_preserve = (Core.Name "preserve")

_TableCommitAction_delete = (Core.Name "delete")

data TableConstraintDefinition = 
  TableConstraintDefinition {}
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

_TruthValue_tRUE = (Core.Name "tRUE")

_TruthValue_fALSE = (Core.Name "fALSE")

_TruthValue_uNKNOWN = (Core.Name "uNKNOWN")

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

data WindowFunction = 
  WindowFunction {}
  deriving (Eq, Ord, Read, Show)

_WindowFunction = (Core.Name "hydra.ext.org.ansi.sql.syntax.WindowFunction")