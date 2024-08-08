-- | A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003

module Hydra.Langs.Sql.Ansi where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype ApproximateNumericLiteral = 
  ApproximateNumericLiteral {
    unApproximateNumericLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericLiteral = (Core.Name "hydra/langs/sql/ansi.ApproximateNumericLiteral")

_ApproximateNumericLiteral_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data BinaryStringLiteral = 
  BinaryStringLiteral {}
  deriving (Eq, Ord, Read, Show)

_BinaryStringLiteral = (Core.Name "hydra/langs/sql/ansi.BinaryStringLiteral")

_BinaryStringLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype CharacterStringLiteral = 
  CharacterStringLiteral {
    unCharacterStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_CharacterStringLiteral = (Core.Name "hydra/langs/sql/ansi.CharacterStringLiteral")

_CharacterStringLiteral_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra/langs/sql/ansi.ColumnName")

_ColumnName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data DateString = 
  DateString {}
  deriving (Eq, Ord, Read, Show)

_DateString = (Core.Name "hydra/langs/sql/ansi.DateString")

_DateString_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype DomainName = 
  DomainName {
    unDomainName :: String}
  deriving (Eq, Ord, Read, Show)

_DomainName = (Core.Name "hydra/langs/sql/ansi.DomainName")

_DomainName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ExactNumericLiteral = 
  ExactNumericLiteral {
    unExactNumericLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ExactNumericLiteral = (Core.Name "hydra/langs/sql/ansi.ExactNumericLiteral")

_ExactNumericLiteral_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype LeftBracketOrTrigraph = 
  LeftBracketOrTrigraph {
    unLeftBracketOrTrigraph :: String}
  deriving (Eq, Ord, Read, Show)

_LeftBracketOrTrigraph = (Core.Name "hydra/langs/sql/ansi.LeftBracketOrTrigraph")

_LeftBracketOrTrigraph_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype RightBracketOrTrigraph = 
  RightBracketOrTrigraph {
    unRightBracketOrTrigraph :: String}
  deriving (Eq, Ord, Read, Show)

_RightBracketOrTrigraph = (Core.Name "hydra/langs/sql/ansi.RightBracketOrTrigraph")

_RightBracketOrTrigraph_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data NationalCharacterStringLiteral = 
  NationalCharacterStringLiteral {}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringLiteral = (Core.Name "hydra/langs/sql/ansi.NationalCharacterStringLiteral")

_NationalCharacterStringLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype PathResolvedUserDefinedTypeName = 
  PathResolvedUserDefinedTypeName {
    unPathResolvedUserDefinedTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_PathResolvedUserDefinedTypeName = (Core.Name "hydra/langs/sql/ansi.PathResolvedUserDefinedTypeName")

_PathResolvedUserDefinedTypeName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype TableName = 
  TableName {
    unTableName :: String}
  deriving (Eq, Ord, Read, Show)

_TableName = (Core.Name "hydra/langs/sql/ansi.TableName")

_TableName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data TimeString = 
  TimeString {}
  deriving (Eq, Ord, Read, Show)

_TimeString = (Core.Name "hydra/langs/sql/ansi.TimeString")

_TimeString_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data TimestampLiteral = 
  TimestampLiteral {}
  deriving (Eq, Ord, Read, Show)

_TimestampLiteral = (Core.Name "hydra/langs/sql/ansi.TimestampLiteral")

_TimestampLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data UnicodeCharacterStringLiteral = 
  UnicodeCharacterStringLiteral {}
  deriving (Eq, Ord, Read, Show)

_UnicodeCharacterStringLiteral = (Core.Name "hydra/langs/sql/ansi.UnicodeCharacterStringLiteral")

_UnicodeCharacterStringLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype UnsignedInteger = 
  UnsignedInteger {
    unUnsignedInteger :: String}
  deriving (Eq, Ord, Read, Show)

_UnsignedInteger = (Core.Name "hydra/langs/sql/ansi.UnsignedInteger")

_UnsignedInteger_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data ApproximateNumericType = 
  ApproximateNumericTypeFloat (Maybe Precision) |
  ApproximateNumericTypeReal  |
  ApproximateNumericTypeDouble 
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra/langs/sql/ansi.ApproximateNumericType")

_ApproximateNumericType_float = (Core.Name "float")

_ApproximateNumericType_real = (Core.Name "real")

_ApproximateNumericType_double = (Core.Name "double")

_ApproximateNumericType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeOptional _Precision_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "real"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype ArrayElement = 
  ArrayElement {
    unArrayElement :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ArrayElement = (Core.Name "hydra/langs/sql/ansi.ArrayElement")

_ArrayElement_type_ = _ValueExpression_type_

data ArrayElementList = 
  ArrayElementList {
    arrayElementListFirst :: ArrayElement,
    arrayElementListRest :: [ArrayElement]}
  deriving (Eq, Ord, Read, Show)

_ArrayElementList = (Core.Name "hydra/langs/sql/ansi.ArrayElementList")

_ArrayElementList_first = (Core.Name "first")

_ArrayElementList_rest = (Core.Name "rest")

_ArrayElementList_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _ArrayElement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _ArrayElement_type_)}]}))

data ArrayElementReference = 
  ArrayElementReference {}
  deriving (Eq, Ord, Read, Show)

_ArrayElementReference = (Core.Name "hydra/langs/sql/ansi.ArrayElementReference")

_ArrayElementReference_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ArrayType = 
  ArrayType {}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/langs/sql/ansi.ArrayType")

_ArrayType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ArrayValueConstructor = 
  ArrayValueConstructorEnumeration ArrayValueConstructorByEnumeration |
  ArrayValueConstructorQuery ArrayValueConstructorByQuery
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructor = (Core.Name "hydra/langs/sql/ansi.ArrayValueConstructor")

_ArrayValueConstructor_enumeration = (Core.Name "enumeration")

_ArrayValueConstructor_query = (Core.Name "query")

_ArrayValueConstructor_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "enumeration"),
      Core.fieldTypeType = _ArrayValueConstructorByEnumeration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "query"),
      Core.fieldTypeType = _ArrayValueConstructorByQuery_type_}]}))

data ArrayValueConstructorByQuery = 
  ArrayValueConstructorByQuery {}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByQuery = (Core.Name "hydra/langs/sql/ansi.ArrayValueConstructorByQuery")

_ArrayValueConstructorByQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ArrayValueConstructorByEnumeration = 
  ArrayValueConstructorByEnumeration {
    arrayValueConstructorByEnumerationLeftBracketOrTrigraph :: LeftBracketOrTrigraph,
    arrayValueConstructorByEnumerationArrayElementList :: ArrayElementList,
    arrayValueConstructorByEnumerationRightBracketOrTrigraph :: RightBracketOrTrigraph}
  deriving (Eq, Ord, Read, Show)

_ArrayValueConstructorByEnumeration = (Core.Name "hydra/langs/sql/ansi.ArrayValueConstructorByEnumeration")

_ArrayValueConstructorByEnumeration_leftBracketOrTrigraph = (Core.Name "leftBracketOrTrigraph")

_ArrayValueConstructorByEnumeration_arrayElementList = (Core.Name "arrayElementList")

_ArrayValueConstructorByEnumeration_rightBracketOrTrigraph = (Core.Name "rightBracketOrTrigraph")

_ArrayValueConstructorByEnumeration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftBracketOrTrigraph"),
      Core.fieldTypeType = _LeftBracketOrTrigraph_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayElementList"),
      Core.fieldTypeType = _ArrayElementList_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightBracketOrTrigraph"),
      Core.fieldTypeType = _RightBracketOrTrigraph_type_}]}))

data ArrayValueExpression = 
  ArrayValueExpression {}
  deriving (Eq, Ord, Read, Show)

_ArrayValueExpression = (Core.Name "hydra/langs/sql/ansi.ArrayValueExpression")

_ArrayValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data AsSubqueryClause = 
  AsSubqueryClause {}
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra/langs/sql/ansi.AsSubqueryClause")

_AsSubqueryClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data AttributeOrMethodReference = 
  AttributeOrMethodReference {}
  deriving (Eq, Ord, Read, Show)

_AttributeOrMethodReference = (Core.Name "hydra/langs/sql/ansi.AttributeOrMethodReference")

_AttributeOrMethodReference_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data BinaryLargeObjectStringType = 
  BinaryLargeObjectStringTypeBinary (Maybe LargeObjectLength) |
  BinaryLargeObjectStringTypeBlob (Maybe LargeObjectLength)
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType = (Core.Name "hydra/langs/sql/ansi.BinaryLargeObjectStringType")

_BinaryLargeObjectStringType_binary = (Core.Name "binary")

_BinaryLargeObjectStringType_blob = (Core.Name "blob")

_BinaryLargeObjectStringType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binary"),
      Core.fieldTypeType = (Core.TypeOptional _LargeObjectLength_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "blob"),
      Core.fieldTypeType = (Core.TypeOptional _LargeObjectLength_type_)}]}))

data BooleanFactor = 
  BooleanFactor {
    booleanFactorNOT :: (Maybe ()),
    booleanFactorBooleanTest :: BooleanTest}
  deriving (Eq, Ord, Read, Show)

_BooleanFactor = (Core.Name "hydra/langs/sql/ansi.BooleanFactor")

_BooleanFactor_nOT = (Core.Name "nOT")

_BooleanFactor_booleanTest = (Core.Name "booleanTest")

_BooleanFactor_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nOT"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "booleanTest"),
      Core.fieldTypeType = _BooleanTest_type_}]}))

data BooleanLiteral = 
  BooleanLiteralTRUE  |
  BooleanLiteralFALSE  |
  BooleanLiteralUNKNOWN 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra/langs/sql/ansi.BooleanLiteral")

_BooleanLiteral_tRUE = (Core.Name "tRUE")

_BooleanLiteral_fALSE = (Core.Name "fALSE")

_BooleanLiteral_uNKNOWN = (Core.Name "uNKNOWN")

_BooleanLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tRUE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fALSE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "uNKNOWN"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data BooleanPredicand = 
  BooleanPredicand {}
  deriving (Eq, Ord, Read, Show)

_BooleanPredicand = (Core.Name "hydra/langs/sql/ansi.BooleanPredicand")

_BooleanPredicand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data BooleanPrimary = 
  BooleanPrimaryPredicate Predicate |
  BooleanPrimaryPredicand BooleanPredicand
  deriving (Eq, Ord, Read, Show)

_BooleanPrimary = (Core.Name "hydra/langs/sql/ansi.BooleanPrimary")

_BooleanPrimary_predicate = (Core.Name "predicate")

_BooleanPrimary_predicand = (Core.Name "predicand")

_BooleanPrimary_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _Predicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicand"),
      Core.fieldTypeType = _BooleanPredicand_type_}]}))

data BooleanTerm = 
  BooleanTermFactor BooleanFactor |
  BooleanTermAnd BooleanTerm_And
  deriving (Eq, Ord, Read, Show)

_BooleanTerm = (Core.Name "hydra/langs/sql/ansi.BooleanTerm")

_BooleanTerm_factor = (Core.Name "factor")

_BooleanTerm_and = (Core.Name "and")

_BooleanTerm_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "factor"),
      Core.fieldTypeType = _BooleanFactor_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = _BooleanTerm_And_type_}]}))

data BooleanTerm_And = 
  BooleanTerm_And {
    booleanTerm_AndLhs :: BooleanTerm,
    booleanTerm_AndRhs :: BooleanFactor}
  deriving (Eq, Ord, Read, Show)

_BooleanTerm_And = (Core.Name "hydra/langs/sql/ansi.BooleanTerm.And")

_BooleanTerm_And_lhs = (Core.Name "lhs")

_BooleanTerm_And_rhs = (Core.Name "rhs")

_BooleanTerm_And_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _BooleanTerm_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _BooleanFactor_type_}]}))

data BooleanTest = 
  BooleanTest {
    booleanTestBooleanPrimary :: BooleanPrimary,
    booleanTestSequence :: (Maybe BooleanTest_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BooleanTest = (Core.Name "hydra/langs/sql/ansi.BooleanTest")

_BooleanTest_booleanPrimary = (Core.Name "booleanPrimary")

_BooleanTest_sequence = (Core.Name "sequence")

_BooleanTest_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "booleanPrimary"),
      Core.fieldTypeType = _BooleanPrimary_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeOptional _BooleanTest_Sequence_Option_type_)}]}))

data BooleanTest_Sequence_Option = 
  BooleanTest_Sequence_Option {
    booleanTest_Sequence_OptionNOT :: (Maybe ()),
    booleanTest_Sequence_OptionTruthValue :: TruthValue}
  deriving (Eq, Ord, Read, Show)

_BooleanTest_Sequence_Option = (Core.Name "hydra/langs/sql/ansi.BooleanTest.Sequence.Option")

_BooleanTest_Sequence_Option_nOT = (Core.Name "nOT")

_BooleanTest_Sequence_Option_truthValue = (Core.Name "truthValue")

_BooleanTest_Sequence_Option_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nOT"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "truthValue"),
      Core.fieldTypeType = _TruthValue_type_}]}))

data BooleanType = 
  BooleanType {}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra/langs/sql/ansi.BooleanType")

_BooleanType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data BooleanValueExpression = 
  BooleanValueExpressionTerm BooleanTerm |
  BooleanValueExpressionOr BooleanValueExpression_Or
  deriving (Eq, Ord, Read, Show)

_BooleanValueExpression = (Core.Name "hydra/langs/sql/ansi.BooleanValueExpression")

_BooleanValueExpression_term = (Core.Name "term")

_BooleanValueExpression_or = (Core.Name "or")

_BooleanValueExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "term"),
      Core.fieldTypeType = _BooleanTerm_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = _BooleanValueExpression_Or_type_}]}))

data BooleanValueExpression_Or = 
  BooleanValueExpression_Or {
    booleanValueExpression_OrLhs :: BooleanValueExpression,
    booleanValueExpression_OrRhs :: BooleanTerm}
  deriving (Eq, Ord, Read, Show)

_BooleanValueExpression_Or = (Core.Name "hydra/langs/sql/ansi.BooleanValueExpression.Or")

_BooleanValueExpression_Or_lhs = (Core.Name "lhs")

_BooleanValueExpression_Or_rhs = (Core.Name "rhs")

_BooleanValueExpression_Or_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _BooleanValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _BooleanTerm_type_}]}))

data CaseExpression = 
  CaseExpression {}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra/langs/sql/ansi.CaseExpression")

_CaseExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data CastSpecification = 
  CastSpecification {}
  deriving (Eq, Ord, Read, Show)

_CastSpecification = (Core.Name "hydra/langs/sql/ansi.CastSpecification")

_CastSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data CharacterSetSpecification = 
  CharacterSetSpecification {}
  deriving (Eq, Ord, Read, Show)

_CharacterSetSpecification = (Core.Name "hydra/langs/sql/ansi.CharacterSetSpecification")

_CharacterSetSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

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

_CharacterStringType = (Core.Name "hydra/langs/sql/ansi.CharacterStringType")

_CharacterStringType_character = (Core.Name "character")

_CharacterStringType_char = (Core.Name "char")

_CharacterStringType_characterVarying = (Core.Name "characterVarying")

_CharacterStringType_charVarying = (Core.Name "charVarying")

_CharacterStringType_varchar = (Core.Name "varchar")

_CharacterStringType_characterLargeObject = (Core.Name "characterLargeObject")

_CharacterStringType_charLargeObject = (Core.Name "charLargeObject")

_CharacterStringType_clob = (Core.Name "clob")

_CharacterStringType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "character"),
      Core.fieldTypeType = (Core.TypeOptional _Length_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "char"),
      Core.fieldTypeType = (Core.TypeOptional _Length_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "characterVarying"),
      Core.fieldTypeType = _Length_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "charVarying"),
      Core.fieldTypeType = _Length_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "varchar"),
      Core.fieldTypeType = _Length_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "characterLargeObject"),
      Core.fieldTypeType = (Core.TypeOptional _LargeObjectLength_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "charLargeObject"),
      Core.fieldTypeType = (Core.TypeOptional _LargeObjectLength_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "clob"),
      Core.fieldTypeType = (Core.TypeOptional _LargeObjectLength_type_)}]}))

data CollateClause = 
  CollateClause {}
  deriving (Eq, Ord, Read, Show)

_CollateClause = (Core.Name "hydra/langs/sql/ansi.CollateClause")

_CollateClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data CollectionType = 
  CollectionTypeArray ArrayType |
  CollectionTypeMultiset MultisetType
  deriving (Eq, Ord, Read, Show)

_CollectionType = (Core.Name "hydra/langs/sql/ansi.CollectionType")

_CollectionType_array = (Core.Name "array")

_CollectionType_multiset = (Core.Name "multiset")

_CollectionType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multiset"),
      Core.fieldTypeType = _MultisetType_type_}]}))

data CollectionValueConstructor = 
  CollectionValueConstructorArray ArrayValueConstructor |
  CollectionValueConstructorMultiset MultisetValueConstructor
  deriving (Eq, Ord, Read, Show)

_CollectionValueConstructor = (Core.Name "hydra/langs/sql/ansi.CollectionValueConstructor")

_CollectionValueConstructor_array = (Core.Name "array")

_CollectionValueConstructor_multiset = (Core.Name "multiset")

_CollectionValueConstructor_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayValueConstructor_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multiset"),
      Core.fieldTypeType = _MultisetValueConstructor_type_}]}))

data CollectionValueExpression = 
  CollectionValueExpressionArray ArrayValueExpression |
  CollectionValueExpressionMultiset MultisetValueExpression
  deriving (Eq, Ord, Read, Show)

_CollectionValueExpression = (Core.Name "hydra/langs/sql/ansi.CollectionValueExpression")

_CollectionValueExpression_array = (Core.Name "array")

_CollectionValueExpression_multiset = (Core.Name "multiset")

_CollectionValueExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "array"),
      Core.fieldTypeType = _ArrayValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multiset"),
      Core.fieldTypeType = _MultisetValueExpression_type_}]}))

data ColumnConstraintDefinition = 
  ColumnConstraintDefinition {}
  deriving (Eq, Ord, Read, Show)

_ColumnConstraintDefinition = (Core.Name "hydra/langs/sql/ansi.ColumnConstraintDefinition")

_ColumnConstraintDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ColumnDefinition = 
  ColumnDefinition {
    columnDefinitionName :: ColumnName,
    columnDefinitionTypeOrDomain :: (Maybe ColumnDefinition_TypeOrDomain_Option),
    columnDefinitionRefScope :: (Maybe ReferenceScopeCheck),
    columnDefinitionDefaultOrIdentityOrGeneration :: (Maybe ColumnDefinition_DefaultOrIdentityOrGeneration_Option),
    columnDefinitionConstraints :: [ColumnConstraintDefinition],
    columnDefinitionCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition = (Core.Name "hydra/langs/sql/ansi.ColumnDefinition")

_ColumnDefinition_name = (Core.Name "name")

_ColumnDefinition_typeOrDomain = (Core.Name "typeOrDomain")

_ColumnDefinition_refScope = (Core.Name "refScope")

_ColumnDefinition_defaultOrIdentityOrGeneration = (Core.Name "defaultOrIdentityOrGeneration")

_ColumnDefinition_constraints = (Core.Name "constraints")

_ColumnDefinition_collate = (Core.Name "collate")

_ColumnDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "typeOrDomain"),
      Core.fieldTypeType = (Core.TypeOptional _ColumnDefinition_TypeOrDomain_Option_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "refScope"),
      Core.fieldTypeType = (Core.TypeOptional _ReferenceScopeCheck_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "defaultOrIdentityOrGeneration"),
      Core.fieldTypeType = (Core.TypeOptional _ColumnDefinition_DefaultOrIdentityOrGeneration_Option_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constraints"),
      Core.fieldTypeType = (Core.TypeList _ColumnConstraintDefinition_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collate"),
      Core.fieldTypeType = (Core.TypeOptional _CollateClause_type_)}]}))

data ColumnDefinition_TypeOrDomain_Option = 
  ColumnDefinition_TypeOrDomain_OptionDataType DataType |
  ColumnDefinition_TypeOrDomain_OptionDomainName DomainName
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_TypeOrDomain_Option = (Core.Name "hydra/langs/sql/ansi.ColumnDefinition.TypeOrDomain.Option")

_ColumnDefinition_TypeOrDomain_Option_dataType = (Core.Name "dataType")

_ColumnDefinition_TypeOrDomain_Option_domainName = (Core.Name "domainName")

_ColumnDefinition_TypeOrDomain_Option_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataType"),
      Core.fieldTypeType = _DataType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "domainName"),
      Core.fieldTypeType = _DomainName_type_}]}))

data ColumnDefinition_DefaultOrIdentityOrGeneration_Option = 
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause DefaultClause |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification IdentityColumnSpecification |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause GenerationClause
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option = (Core.Name "hydra/langs/sql/ansi.ColumnDefinition.DefaultOrIdentityOrGeneration.Option")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_defaultClause = (Core.Name "defaultClause")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_identityColumnSpecification = (Core.Name "identityColumnSpecification")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_generationClause = (Core.Name "generationClause")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "defaultClause"),
      Core.fieldTypeType = _DefaultClause_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "identityColumnSpecification"),
      Core.fieldTypeType = _IdentityColumnSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "generationClause"),
      Core.fieldTypeType = _GenerationClause_type_}]}))

data ColumnNameList = 
  ColumnNameList {
    columnNameListFirst :: ColumnName,
    columnNameListRest :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_ColumnNameList = (Core.Name "hydra/langs/sql/ansi.ColumnNameList")

_ColumnNameList_first = (Core.Name "first")

_ColumnNameList_rest = (Core.Name "rest")

_ColumnNameList_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _ColumnName_type_)}]}))

data ColumnOptions = 
  ColumnOptions {}
  deriving (Eq, Ord, Read, Show)

_ColumnOptions = (Core.Name "hydra/langs/sql/ansi.ColumnOptions")

_ColumnOptions_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ColumnReference = 
  ColumnReference {}
  deriving (Eq, Ord, Read, Show)

_ColumnReference = (Core.Name "hydra/langs/sql/ansi.ColumnReference")

_ColumnReference_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data CommonValueExpression = 
  CommonValueExpressionNumeric NumericValueExpression |
  CommonValueExpressionString StringValueExpression |
  CommonValueExpressionDatetime DatetimeValueExpression |
  CommonValueExpressionInterval IntervalValueExpression |
  CommonValueExpressionUserDefined UserDefinedTypeValueExpression |
  CommonValueExpressionReference ReferenceValueExpression |
  CommonValueExpressionCollection CollectionValueExpression
  deriving (Eq, Ord, Read, Show)

_CommonValueExpression = (Core.Name "hydra/langs/sql/ansi.CommonValueExpression")

_CommonValueExpression_numeric = (Core.Name "numeric")

_CommonValueExpression_string = (Core.Name "string")

_CommonValueExpression_datetime = (Core.Name "datetime")

_CommonValueExpression_interval = (Core.Name "interval")

_CommonValueExpression_userDefined = (Core.Name "userDefined")

_CommonValueExpression_reference = (Core.Name "reference")

_CommonValueExpression_collection = (Core.Name "collection")

_CommonValueExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _NumericValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datetime"),
      Core.fieldTypeType = _DatetimeValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interval"),
      Core.fieldTypeType = _IntervalValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "userDefined"),
      Core.fieldTypeType = _UserDefinedTypeValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reference"),
      Core.fieldTypeType = _ReferenceValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collection"),
      Core.fieldTypeType = _CollectionValueExpression_type_}]}))

data ContextuallyTypedRowValueExpression = 
  ContextuallyTypedRowValueExpressionSpecialCase RowValueSpecialCase |
  ContextuallyTypedRowValueExpressionConstructor ContextuallyTypedRowValueConstructor
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueExpression = (Core.Name "hydra/langs/sql/ansi.ContextuallyTypedRowValueExpression")

_ContextuallyTypedRowValueExpression_specialCase = (Core.Name "specialCase")

_ContextuallyTypedRowValueExpression_constructor = (Core.Name "constructor")

_ContextuallyTypedRowValueExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "specialCase"),
      Core.fieldTypeType = _RowValueSpecialCase_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructor"),
      Core.fieldTypeType = _ContextuallyTypedRowValueConstructor_type_}]}))

data ContextuallyTypedRowValueConstructor = 
  ContextuallyTypedRowValueConstructor {}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueConstructor = (Core.Name "hydra/langs/sql/ansi.ContextuallyTypedRowValueConstructor")

_ContextuallyTypedRowValueConstructor_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ContextuallyTypedRowValueExpressionList = 
  ContextuallyTypedRowValueExpressionList {
    contextuallyTypedRowValueExpressionListFirst :: ContextuallyTypedRowValueExpression,
    contextuallyTypedRowValueExpressionListRest :: [ContextuallyTypedRowValueExpression]}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedRowValueExpressionList = (Core.Name "hydra/langs/sql/ansi.ContextuallyTypedRowValueExpressionList")

_ContextuallyTypedRowValueExpressionList_first = (Core.Name "first")

_ContextuallyTypedRowValueExpressionList_rest = (Core.Name "rest")

_ContextuallyTypedRowValueExpressionList_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _ContextuallyTypedRowValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _ContextuallyTypedRowValueExpression_type_)}]}))

newtype ContextuallyTypedTableValueConstructor = 
  ContextuallyTypedTableValueConstructor {
    unContextuallyTypedTableValueConstructor :: ContextuallyTypedRowValueExpressionList}
  deriving (Eq, Ord, Read, Show)

_ContextuallyTypedTableValueConstructor = (Core.Name "hydra/langs/sql/ansi.ContextuallyTypedTableValueConstructor")

_ContextuallyTypedTableValueConstructor_type_ = _ContextuallyTypedRowValueExpressionList_type_

data DataType = 
  DataTypePredefined PredefinedType |
  DataTypeRow RowType |
  DataTypeNamed PathResolvedUserDefinedTypeName |
  DataTypeReference ReferenceType |
  DataTypeCollection CollectionType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra/langs/sql/ansi.DataType")

_DataType_predefined = (Core.Name "predefined")

_DataType_row = (Core.Name "row")

_DataType_named = (Core.Name "named")

_DataType_reference = (Core.Name "reference")

_DataType_collection = (Core.Name "collection")

_DataType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predefined"),
      Core.fieldTypeType = _PredefinedType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "row"),
      Core.fieldTypeType = _RowType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "named"),
      Core.fieldTypeType = _PathResolvedUserDefinedTypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reference"),
      Core.fieldTypeType = _ReferenceType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collection"),
      Core.fieldTypeType = _CollectionType_type_}]}))

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: DateString}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra/langs/sql/ansi.DateLiteral")

_DateLiteral_type_ = _DateString_type_

data DatetimeLiteral = 
  DatetimeLiteralDate DateLiteral |
  DatetimeLiteralTime TimeLiteral |
  DatetimeLiteralTimestamp TimestampLiteral
  deriving (Eq, Ord, Read, Show)

_DatetimeLiteral = (Core.Name "hydra/langs/sql/ansi.DatetimeLiteral")

_DatetimeLiteral_date = (Core.Name "date")

_DatetimeLiteral_time = (Core.Name "time")

_DatetimeLiteral_timestamp = (Core.Name "timestamp")

_DatetimeLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = _DateLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "time"),
      Core.fieldTypeType = _TimeLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "timestamp"),
      Core.fieldTypeType = _TimestampLiteral_type_}]}))

data DatetimeType = 
  DatetimeType {}
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra/langs/sql/ansi.DatetimeType")

_DatetimeType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data DatetimeValueExpression = 
  DatetimeValueExpression {}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression = (Core.Name "hydra/langs/sql/ansi.DatetimeValueExpression")

_DatetimeValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data DefaultClause = 
  DefaultClause {}
  deriving (Eq, Ord, Read, Show)

_DefaultClause = (Core.Name "hydra/langs/sql/ansi.DefaultClause")

_DefaultClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ExactNumericType = 
  ExactNumericTypeNumeric (Maybe ExactNumericType_Numeric_Option) |
  ExactNumericTypeDecimal (Maybe ExactNumericType_Decimal_Option) |
  ExactNumericTypeDec (Maybe ExactNumericType_Dec_Option) |
  ExactNumericTypeSmallint  |
  ExactNumericTypeInteger  |
  ExactNumericTypeInt  |
  ExactNumericTypeBigint 
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = (Core.Name "hydra/langs/sql/ansi.ExactNumericType")

_ExactNumericType_numeric = (Core.Name "numeric")

_ExactNumericType_decimal = (Core.Name "decimal")

_ExactNumericType_dec = (Core.Name "dec")

_ExactNumericType_smallint = (Core.Name "smallint")

_ExactNumericType_integer = (Core.Name "integer")

_ExactNumericType_int = (Core.Name "int")

_ExactNumericType_bigint = (Core.Name "bigint")

_ExactNumericType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = (Core.TypeOptional _ExactNumericType_Numeric_Option_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "decimal"),
      Core.fieldTypeType = (Core.TypeOptional _ExactNumericType_Decimal_Option_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dec"),
      Core.fieldTypeType = (Core.TypeOptional _ExactNumericType_Dec_Option_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "smallint"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bigint"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ExactNumericType_Numeric_Option = 
  ExactNumericType_Numeric_Option {
    exactNumericType_Numeric_OptionPrecision :: Precision,
    exactNumericType_Numeric_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric_Option = (Core.Name "hydra/langs/sql/ansi.ExactNumericType.Numeric.Option")

_ExactNumericType_Numeric_Option_precision = (Core.Name "precision")

_ExactNumericType_Numeric_Option_sequence = (Core.Name "sequence")

_ExactNumericType_Numeric_Option_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "precision"),
      Core.fieldTypeType = _Precision_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeOptional _Scale_type_)}]}))

data ExactNumericType_Decimal_Option = 
  ExactNumericType_Decimal_Option {
    exactNumericType_Decimal_OptionPrecision :: Precision,
    exactNumericType_Decimal_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Option = (Core.Name "hydra/langs/sql/ansi.ExactNumericType.Decimal.Option")

_ExactNumericType_Decimal_Option_precision = (Core.Name "precision")

_ExactNumericType_Decimal_Option_sequence = (Core.Name "sequence")

_ExactNumericType_Decimal_Option_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "precision"),
      Core.fieldTypeType = _Precision_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeOptional _Scale_type_)}]}))

data ExactNumericType_Dec_Option = 
  ExactNumericType_Dec_Option {
    exactNumericType_Dec_OptionPrecision :: Precision,
    exactNumericType_Dec_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Option = (Core.Name "hydra/langs/sql/ansi.ExactNumericType.Dec.Option")

_ExactNumericType_Dec_Option_precision = (Core.Name "precision")

_ExactNumericType_Dec_Option_sequence = (Core.Name "sequence")

_ExactNumericType_Dec_Option_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "precision"),
      Core.fieldTypeType = _Precision_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sequence"),
      Core.fieldTypeType = (Core.TypeOptional _Scale_type_)}]}))

data FieldReference = 
  FieldReference {}
  deriving (Eq, Ord, Read, Show)

_FieldReference = (Core.Name "hydra/langs/sql/ansi.FieldReference")

_FieldReference_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data FromConstructor = 
  FromConstructor {
    fromConstructorColumns :: (Maybe InsertColumnList),
    fromConstructorOverride :: (Maybe OverrideClause),
    fromConstructorValues :: ContextuallyTypedTableValueConstructor}
  deriving (Eq, Ord, Read, Show)

_FromConstructor = (Core.Name "hydra/langs/sql/ansi.FromConstructor")

_FromConstructor_columns = (Core.Name "columns")

_FromConstructor_override = (Core.Name "override")

_FromConstructor_values = (Core.Name "values")

_FromConstructor_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "columns"),
      Core.fieldTypeType = (Core.TypeOptional _InsertColumnList_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "override"),
      Core.fieldTypeType = (Core.TypeOptional _OverrideClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "values"),
      Core.fieldTypeType = _ContextuallyTypedTableValueConstructor_type_}]}))

data FromDefault = 
  FromDefault {}
  deriving (Eq, Ord, Read, Show)

_FromDefault = (Core.Name "hydra/langs/sql/ansi.FromDefault")

_FromDefault_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data FromSubquery = 
  FromSubquery {}
  deriving (Eq, Ord, Read, Show)

_FromSubquery = (Core.Name "hydra/langs/sql/ansi.FromSubquery")

_FromSubquery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data GeneralLiteral = 
  GeneralLiteralString CharacterStringLiteral |
  GeneralLiteralNationalString NationalCharacterStringLiteral |
  GeneralLiteralUnicode UnicodeCharacterStringLiteral |
  GeneralLiteralBinary BinaryStringLiteral |
  GeneralLiteralDateTime DatetimeLiteral |
  GeneralLiteralInterval IntervalLiteral |
  GeneralLiteralBoolean BooleanLiteral
  deriving (Eq, Ord, Read, Show)

_GeneralLiteral = (Core.Name "hydra/langs/sql/ansi.GeneralLiteral")

_GeneralLiteral_string = (Core.Name "string")

_GeneralLiteral_nationalString = (Core.Name "nationalString")

_GeneralLiteral_unicode = (Core.Name "unicode")

_GeneralLiteral_binary = (Core.Name "binary")

_GeneralLiteral_dateTime = (Core.Name "dateTime")

_GeneralLiteral_interval = (Core.Name "interval")

_GeneralLiteral_boolean = (Core.Name "boolean")

_GeneralLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _CharacterStringLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nationalString"),
      Core.fieldTypeType = _NationalCharacterStringLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unicode"),
      Core.fieldTypeType = _UnicodeCharacterStringLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binary"),
      Core.fieldTypeType = _BinaryStringLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dateTime"),
      Core.fieldTypeType = _DatetimeLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interval"),
      Core.fieldTypeType = _IntervalLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BooleanLiteral_type_}]}))

data GeneralValueSpecification = 
  GeneralValueSpecification {}
  deriving (Eq, Ord, Read, Show)

_GeneralValueSpecification = (Core.Name "hydra/langs/sql/ansi.GeneralValueSpecification")

_GeneralValueSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data GenerationClause = 
  GenerationClause {}
  deriving (Eq, Ord, Read, Show)

_GenerationClause = (Core.Name "hydra/langs/sql/ansi.GenerationClause")

_GenerationClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data GlobalOrLocal = 
  GlobalOrLocalGlobal  |
  GlobalOrLocalLocal 
  deriving (Eq, Ord, Read, Show)

_GlobalOrLocal = (Core.Name "hydra/langs/sql/ansi.GlobalOrLocal")

_GlobalOrLocal_global = (Core.Name "global")

_GlobalOrLocal_local = (Core.Name "local")

_GlobalOrLocal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "global"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data IdentityColumnSpecification = 
  IdentityColumnSpecification {}
  deriving (Eq, Ord, Read, Show)

_IdentityColumnSpecification = (Core.Name "hydra/langs/sql/ansi.IdentityColumnSpecification")

_IdentityColumnSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype InsertColumnList = 
  InsertColumnList {
    unInsertColumnList :: ColumnNameList}
  deriving (Eq, Ord, Read, Show)

_InsertColumnList = (Core.Name "hydra/langs/sql/ansi.InsertColumnList")

_InsertColumnList_type_ = _ColumnNameList_type_

data InsertColumnsAndSource = 
  InsertColumnsAndSourceSubquery FromSubquery |
  InsertColumnsAndSourceConstructor FromConstructor |
  InsertColumnsAndSourceDefault FromDefault
  deriving (Eq, Ord, Read, Show)

_InsertColumnsAndSource = (Core.Name "hydra/langs/sql/ansi.InsertColumnsAndSource")

_InsertColumnsAndSource_subquery = (Core.Name "subquery")

_InsertColumnsAndSource_constructor = (Core.Name "constructor")

_InsertColumnsAndSource_default = (Core.Name "default")

_InsertColumnsAndSource_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subquery"),
      Core.fieldTypeType = _FromSubquery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constructor"),
      Core.fieldTypeType = _FromConstructor_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "default"),
      Core.fieldTypeType = _FromDefault_type_}]}))

data InsertStatement = 
  InsertStatement {
    insertStatementTarget :: InsertionTarget,
    insertStatementColumnsAndSource :: InsertColumnsAndSource}
  deriving (Eq, Ord, Read, Show)

_InsertStatement = (Core.Name "hydra/langs/sql/ansi.InsertStatement")

_InsertStatement_target = (Core.Name "target")

_InsertStatement_columnsAndSource = (Core.Name "columnsAndSource")

_InsertStatement_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = _InsertionTarget_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "columnsAndSource"),
      Core.fieldTypeType = _InsertColumnsAndSource_type_}]}))

newtype InsertionTarget = 
  InsertionTarget {
    unInsertionTarget :: TableName}
  deriving (Eq, Ord, Read, Show)

_InsertionTarget = (Core.Name "hydra/langs/sql/ansi.InsertionTarget")

_InsertionTarget_type_ = _TableName_type_

data IntervalLiteral = 
  IntervalLiteral {}
  deriving (Eq, Ord, Read, Show)

_IntervalLiteral = (Core.Name "hydra/langs/sql/ansi.IntervalLiteral")

_IntervalLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data IntervalType = 
  IntervalType {}
  deriving (Eq, Ord, Read, Show)

_IntervalType = (Core.Name "hydra/langs/sql/ansi.IntervalType")

_IntervalType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data IntervalValueExpression = 
  IntervalValueExpression {}
  deriving (Eq, Ord, Read, Show)

_IntervalValueExpression = (Core.Name "hydra/langs/sql/ansi.IntervalValueExpression")

_IntervalValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data LargeObjectLength = 
  LargeObjectLength {}
  deriving (Eq, Ord, Read, Show)

_LargeObjectLength = (Core.Name "hydra/langs/sql/ansi.LargeObjectLength")

_LargeObjectLength_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype Length = 
  Length {
    unLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Length = (Core.Name "hydra/langs/sql/ansi.Length")

_Length_type_ = _UnsignedInteger_type_

data LikeClause = 
  LikeClause {}
  deriving (Eq, Ord, Read, Show)

_LikeClause = (Core.Name "hydra/langs/sql/ansi.LikeClause")

_LikeClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data MethodInvocation = 
  MethodInvocation {}
  deriving (Eq, Ord, Read, Show)

_MethodInvocation = (Core.Name "hydra/langs/sql/ansi.MethodInvocation")

_MethodInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data MultisetElementReference = 
  MultisetElementReference {}
  deriving (Eq, Ord, Read, Show)

_MultisetElementReference = (Core.Name "hydra/langs/sql/ansi.MultisetElementReference")

_MultisetElementReference_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype MultisetType = 
  MultisetType {
    unMultisetType :: DataType}
  deriving (Eq, Ord, Read, Show)

_MultisetType = (Core.Name "hydra/langs/sql/ansi.MultisetType")

_MultisetType_type_ = _DataType_type_

data MultisetValueConstructor = 
  MultisetValueConstructor {}
  deriving (Eq, Ord, Read, Show)

_MultisetValueConstructor = (Core.Name "hydra/langs/sql/ansi.MultisetValueConstructor")

_MultisetValueConstructor_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data MultisetValueExpression = 
  MultisetValueExpression {}
  deriving (Eq, Ord, Read, Show)

_MultisetValueExpression = (Core.Name "hydra/langs/sql/ansi.MultisetValueExpression")

_MultisetValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data NationalCharacterStringType = 
  NationalCharacterStringType {}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringType = (Core.Name "hydra/langs/sql/ansi.NationalCharacterStringType")

_NationalCharacterStringType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data NewSpecification = 
  NewSpecification {}
  deriving (Eq, Ord, Read, Show)

_NewSpecification = (Core.Name "hydra/langs/sql/ansi.NewSpecification")

_NewSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data NextValueExpression = 
  NextValueExpression {}
  deriving (Eq, Ord, Read, Show)

_NextValueExpression = (Core.Name "hydra/langs/sql/ansi.NextValueExpression")

_NextValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/langs/sql/ansi.NumericType")

_NumericType_exact = (Core.Name "exact")

_NumericType_approximate = (Core.Name "approximate")

_NumericType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "exact"),
      Core.fieldTypeType = _ExactNumericType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "approximate"),
      Core.fieldTypeType = _ApproximateNumericType_type_}]}))

data NumericValueExpression = 
  NumericValueExpression {}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpression = (Core.Name "hydra/langs/sql/ansi.NumericValueExpression")

_NumericValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data OverrideClause = 
  OverrideClauseOVERRIDINGSpUSERSpVALUE  |
  OverrideClauseOVERRIDINGSpSYSTEMSpVALUE 
  deriving (Eq, Ord, Read, Show)

_OverrideClause = (Core.Name "hydra/langs/sql/ansi.OverrideClause")

_OverrideClause_oVERRIDINGSpUSERSpVALUE = (Core.Name "oVERRIDINGSpUSERSpVALUE")

_OverrideClause_oVERRIDINGSpSYSTEMSpVALUE = (Core.Name "oVERRIDINGSpSYSTEMSpVALUE")

_OverrideClause_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "oVERRIDINGSpUSERSpVALUE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "oVERRIDINGSpSYSTEMSpVALUE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype ParenthesizedValueExpression = 
  ParenthesizedValueExpression {
    unParenthesizedValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedValueExpression = (Core.Name "hydra/langs/sql/ansi.ParenthesizedValueExpression")

_ParenthesizedValueExpression_type_ = _ValueExpression_type_

newtype Precision = 
  Precision {
    unPrecision :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra/langs/sql/ansi.Precision")

_Precision_type_ = _UnsignedInteger_type_

data PredefinedType = 
  PredefinedTypeString PredefinedType_String |
  PredefinedTypeNationalString PredefinedType_NationalString |
  PredefinedTypeBlob BinaryLargeObjectStringType |
  PredefinedTypeNumeric NumericType |
  PredefinedTypeBoolean BooleanType |
  PredefinedTypeDatetime DatetimeType |
  PredefinedTypeInterval IntervalType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = (Core.Name "hydra/langs/sql/ansi.PredefinedType")

_PredefinedType_string = (Core.Name "string")

_PredefinedType_nationalString = (Core.Name "nationalString")

_PredefinedType_blob = (Core.Name "blob")

_PredefinedType_numeric = (Core.Name "numeric")

_PredefinedType_boolean = (Core.Name "boolean")

_PredefinedType_datetime = (Core.Name "datetime")

_PredefinedType_interval = (Core.Name "interval")

_PredefinedType_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _PredefinedType_String_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nationalString"),
      Core.fieldTypeType = _PredefinedType_NationalString_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "blob"),
      Core.fieldTypeType = _BinaryLargeObjectStringType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _NumericType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BooleanType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datetime"),
      Core.fieldTypeType = _DatetimeType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "interval"),
      Core.fieldTypeType = _IntervalType_type_}]}))

data PredefinedType_String = 
  PredefinedType_String {
    predefinedType_StringType :: CharacterStringType,
    predefinedType_StringCharacters :: (Maybe CharacterSetSpecification),
    predefinedType_StringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_String = (Core.Name "hydra/langs/sql/ansi.PredefinedType.String")

_PredefinedType_String_type = (Core.Name "type")

_PredefinedType_String_characters = (Core.Name "characters")

_PredefinedType_String_collate = (Core.Name "collate")

_PredefinedType_String_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _CharacterStringType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "characters"),
      Core.fieldTypeType = (Core.TypeOptional _CharacterSetSpecification_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collate"),
      Core.fieldTypeType = (Core.TypeOptional _CollateClause_type_)}]}))

data PredefinedType_NationalString = 
  PredefinedType_NationalString {
    predefinedType_NationalStringType :: NationalCharacterStringType,
    predefinedType_NationalStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_NationalString = (Core.Name "hydra/langs/sql/ansi.PredefinedType.NationalString")

_PredefinedType_NationalString_type = (Core.Name "type")

_PredefinedType_NationalString_collate = (Core.Name "collate")

_PredefinedType_NationalString_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _NationalCharacterStringType_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collate"),
      Core.fieldTypeType = (Core.TypeOptional _CollateClause_type_)}]}))

data Predicate = 
  Predicate {}
  deriving (Eq, Ord, Read, Show)

_Predicate = (Core.Name "hydra/langs/sql/ansi.Predicate")

_Predicate_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data QueryExpression = 
  QueryExpression {}
  deriving (Eq, Ord, Read, Show)

_QueryExpression = (Core.Name "hydra/langs/sql/ansi.QueryExpression")

_QueryExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ReferenceScopeCheck = 
  ReferenceScopeCheck {}
  deriving (Eq, Ord, Read, Show)

_ReferenceScopeCheck = (Core.Name "hydra/langs/sql/ansi.ReferenceScopeCheck")

_ReferenceScopeCheck_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ReferenceType = 
  ReferenceType {}
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra/langs/sql/ansi.ReferenceType")

_ReferenceType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data RowType = 
  RowType {}
  deriving (Eq, Ord, Read, Show)

_RowType = (Core.Name "hydra/langs/sql/ansi.RowType")

_RowType_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype RowValueSpecialCase = 
  RowValueSpecialCase {
    unRowValueSpecialCase :: NonparenthesizedValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_RowValueSpecialCase = (Core.Name "hydra/langs/sql/ansi.RowValueSpecialCase")

_RowValueSpecialCase_type_ = _NonparenthesizedValueExpressionPrimary_type_

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

_NonparenthesizedValueExpressionPrimary = (Core.Name "hydra/langs/sql/ansi.NonparenthesizedValueExpressionPrimary")

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

_NonparenthesizedValueExpressionPrimary_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unsigned"),
      Core.fieldTypeType = _UnsignedValueSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnReference_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "setFunction"),
      Core.fieldTypeType = _SetFunctionSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "windowFunction"),
      Core.fieldTypeType = _WindowFunction_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scalarSubquery"),
      Core.fieldTypeType = _ScalarSubquery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cases"),
      Core.fieldTypeType = _CaseExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "cast"),
      Core.fieldTypeType = _CastSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "field"),
      Core.fieldTypeType = _FieldReference_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subtype"),
      Core.fieldTypeType = _SubtypeTreatment_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "method"),
      Core.fieldTypeType = _MethodInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "staticMethod"),
      Core.fieldTypeType = _StaticMethodInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "new"),
      Core.fieldTypeType = _NewSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "attributeOrMethod"),
      Core.fieldTypeType = _AttributeOrMethodReference_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reference"),
      Core.fieldTypeType = _ReferenceResolution_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "collection"),
      Core.fieldTypeType = _CollectionValueConstructor_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arrayElement"),
      Core.fieldTypeType = _ArrayElementReference_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multisetElement"),
      Core.fieldTypeType = _MultisetElementReference_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "routine"),
      Core.fieldTypeType = _RoutineInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "next"),
      Core.fieldTypeType = _NextValueExpression_type_}]}))

data ReferenceResolution = 
  ReferenceResolution {}
  deriving (Eq, Ord, Read, Show)

_ReferenceResolution = (Core.Name "hydra/langs/sql/ansi.ReferenceResolution")

_ReferenceResolution_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype ReferenceValueExpression = 
  ReferenceValueExpression {
    unReferenceValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_ReferenceValueExpression = (Core.Name "hydra/langs/sql/ansi.ReferenceValueExpression")

_ReferenceValueExpression_type_ = _ValueExpressionPrimary_type_

data RowValueExpression = 
  RowValueExpression {}
  deriving (Eq, Ord, Read, Show)

_RowValueExpression = (Core.Name "hydra/langs/sql/ansi.RowValueExpression")

_RowValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data RoutineInvocation = 
  RoutineInvocation {}
  deriving (Eq, Ord, Read, Show)

_RoutineInvocation = (Core.Name "hydra/langs/sql/ansi.RoutineInvocation")

_RoutineInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype ScalarSubquery = 
  ScalarSubquery {
    unScalarSubquery :: Subquery}
  deriving (Eq, Ord, Read, Show)

_ScalarSubquery = (Core.Name "hydra/langs/sql/ansi.ScalarSubquery")

_ScalarSubquery_type_ = _Subquery_type_

newtype Scale = 
  Scale {
    unScale :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Scale = (Core.Name "hydra/langs/sql/ansi.Scale")

_Scale_type_ = _UnsignedInteger_type_

data SelfReferencingColumnSpecification = 
  SelfReferencingColumnSpecification {}
  deriving (Eq, Ord, Read, Show)

_SelfReferencingColumnSpecification = (Core.Name "hydra/langs/sql/ansi.SelfReferencingColumnSpecification")

_SelfReferencingColumnSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data SetFunctionSpecification = 
  SetFunctionSpecification {}
  deriving (Eq, Ord, Read, Show)

_SetFunctionSpecification = (Core.Name "hydra/langs/sql/ansi.SetFunctionSpecification")

_SetFunctionSpecification_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data StaticMethodInvocation = 
  StaticMethodInvocation {}
  deriving (Eq, Ord, Read, Show)

_StaticMethodInvocation = (Core.Name "hydra/langs/sql/ansi.StaticMethodInvocation")

_StaticMethodInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data StringValueExpression = 
  StringValueExpression {}
  deriving (Eq, Ord, Read, Show)

_StringValueExpression = (Core.Name "hydra/langs/sql/ansi.StringValueExpression")

_StringValueExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype Subquery = 
  Subquery {
    unSubquery :: QueryExpression}
  deriving (Eq, Ord, Read, Show)

_Subquery = (Core.Name "hydra/langs/sql/ansi.Subquery")

_Subquery_type_ = _QueryExpression_type_

data SubtableClause = 
  SubtableClause {}
  deriving (Eq, Ord, Read, Show)

_SubtableClause = (Core.Name "hydra/langs/sql/ansi.SubtableClause")

_SubtableClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data SubtypeTreatment = 
  SubtypeTreatment {}
  deriving (Eq, Ord, Read, Show)

_SubtypeTreatment = (Core.Name "hydra/langs/sql/ansi.SubtypeTreatment")

_SubtypeTreatment_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data TableCommitAction = 
  TableCommitActionPreserve  |
  TableCommitActionDelete 
  deriving (Eq, Ord, Read, Show)

_TableCommitAction = (Core.Name "hydra/langs/sql/ansi.TableCommitAction")

_TableCommitAction_preserve = (Core.Name "preserve")

_TableCommitAction_delete = (Core.Name "delete")

_TableCommitAction_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "preserve"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "delete"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data TableConstraintDefinition = 
  TableConstraintDefinition {}
  deriving (Eq, Ord, Read, Show)

_TableConstraintDefinition = (Core.Name "hydra/langs/sql/ansi.TableConstraintDefinition")

_TableConstraintDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data TableContentsSource = 
  TableContentsSourceList TableElementList |
  TableContentsSourceSubtable TableContentsSource_Subtable |
  TableContentsSourceSubquery AsSubqueryClause
  deriving (Eq, Ord, Read, Show)

_TableContentsSource = (Core.Name "hydra/langs/sql/ansi.TableContentsSource")

_TableContentsSource_list = (Core.Name "list")

_TableContentsSource_subtable = (Core.Name "subtable")

_TableContentsSource_subquery = (Core.Name "subquery")

_TableContentsSource_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _TableElementList_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subtable"),
      Core.fieldTypeType = _TableContentsSource_Subtable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subquery"),
      Core.fieldTypeType = _AsSubqueryClause_type_}]}))

data TableContentsSource_Subtable = 
  TableContentsSource_Subtable {
    tableContentsSource_SubtableType :: PathResolvedUserDefinedTypeName,
    tableContentsSource_SubtableSubtable :: (Maybe SubtableClause),
    tableContentsSource_SubtableElements :: (Maybe TableElementList)}
  deriving (Eq, Ord, Read, Show)

_TableContentsSource_Subtable = (Core.Name "hydra/langs/sql/ansi.TableContentsSource.Subtable")

_TableContentsSource_Subtable_type = (Core.Name "type")

_TableContentsSource_Subtable_subtable = (Core.Name "subtable")

_TableContentsSource_Subtable_elements = (Core.Name "elements")

_TableContentsSource_Subtable_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "type"),
      Core.fieldTypeType = _PathResolvedUserDefinedTypeName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subtable"),
      Core.fieldTypeType = (Core.TypeOptional _SubtableClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "elements"),
      Core.fieldTypeType = (Core.TypeOptional _TableElementList_type_)}]}))

data TableDefinition = 
  TableDefinition {
    tableDefinitionScope :: (Maybe TableScope),
    tableDefinitionName :: TableName,
    tableDefinitionSource :: TableContentsSource,
    tableDefinitionCommitActions :: (Maybe TableCommitAction)}
  deriving (Eq, Ord, Read, Show)

_TableDefinition = (Core.Name "hydra/langs/sql/ansi.TableDefinition")

_TableDefinition_scope = (Core.Name "scope")

_TableDefinition_name = (Core.Name "name")

_TableDefinition_source = (Core.Name "source")

_TableDefinition_commitActions = (Core.Name "commitActions")

_TableDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "scope"),
      Core.fieldTypeType = (Core.TypeOptional _TableScope_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _TableName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _TableContentsSource_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "commitActions"),
      Core.fieldTypeType = (Core.TypeOptional _TableCommitAction_type_)}]}))

data TableElement = 
  TableElementColumn ColumnDefinition |
  TableElementTableConstraint TableConstraintDefinition |
  TableElementLike LikeClause |
  TableElementSelfReferencingColumn SelfReferencingColumnSpecification |
  TableElementColumOptions ColumnOptions
  deriving (Eq, Ord, Read, Show)

_TableElement = (Core.Name "hydra/langs/sql/ansi.TableElement")

_TableElement_column = (Core.Name "column")

_TableElement_tableConstraint = (Core.Name "tableConstraint")

_TableElement_like = (Core.Name "like")

_TableElement_selfReferencingColumn = (Core.Name "selfReferencingColumn")

_TableElement_columOptions = (Core.Name "columOptions")

_TableElement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tableConstraint"),
      Core.fieldTypeType = _TableConstraintDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "like"),
      Core.fieldTypeType = _LikeClause_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "selfReferencingColumn"),
      Core.fieldTypeType = _SelfReferencingColumnSpecification_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "columOptions"),
      Core.fieldTypeType = _ColumnOptions_type_}]}))

data TableElementList = 
  TableElementList {
    tableElementListFirst :: TableElement,
    tableElementListRest :: [TableElement]}
  deriving (Eq, Ord, Read, Show)

_TableElementList = (Core.Name "hydra/langs/sql/ansi.TableElementList")

_TableElementList_first = (Core.Name "first")

_TableElementList_rest = (Core.Name "rest")

_TableElementList_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "first"),
      Core.fieldTypeType = _TableElement_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _TableElement_type_)}]}))

newtype TableScope = 
  TableScope {
    unTableScope :: GlobalOrLocal}
  deriving (Eq, Ord, Read, Show)

_TableScope = (Core.Name "hydra/langs/sql/ansi.TableScope")

_TableScope_type_ = _GlobalOrLocal_type_

newtype TimeLiteral = 
  TimeLiteral {
    unTimeLiteral :: TimeString}
  deriving (Eq, Ord, Read, Show)

_TimeLiteral = (Core.Name "hydra/langs/sql/ansi.TimeLiteral")

_TimeLiteral_type_ = _TimeString_type_

data TruthValue = 
  TruthValueTRUE  |
  TruthValueFALSE  |
  TruthValueUNKNOWN 
  deriving (Eq, Ord, Read, Show)

_TruthValue = (Core.Name "hydra/langs/sql/ansi.TruthValue")

_TruthValue_tRUE = (Core.Name "tRUE")

_TruthValue_fALSE = (Core.Name "fALSE")

_TruthValue_uNKNOWN = (Core.Name "uNKNOWN")

_TruthValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tRUE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fALSE"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "uNKNOWN"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data UnsignedLiteral = 
  UnsignedLiteralNumeric UnsignedNumericLiteral |
  UnsignedLiteralGeneral GeneralLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedLiteral = (Core.Name "hydra/langs/sql/ansi.UnsignedLiteral")

_UnsignedLiteral_numeric = (Core.Name "numeric")

_UnsignedLiteral_general = (Core.Name "general")

_UnsignedLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "numeric"),
      Core.fieldTypeType = _UnsignedNumericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "general"),
      Core.fieldTypeType = _GeneralLiteral_type_}]}))

data UnsignedNumericLiteral = 
  UnsignedNumericLiteralExact ExactNumericLiteral |
  UnsignedNumericLiteralApproximate ApproximateNumericLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedNumericLiteral = (Core.Name "hydra/langs/sql/ansi.UnsignedNumericLiteral")

_UnsignedNumericLiteral_exact = (Core.Name "exact")

_UnsignedNumericLiteral_approximate = (Core.Name "approximate")

_UnsignedNumericLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "exact"),
      Core.fieldTypeType = _ExactNumericLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "approximate"),
      Core.fieldTypeType = _ApproximateNumericLiteral_type_}]}))

data UnsignedValueSpecification = 
  UnsignedValueSpecificationLiteral UnsignedLiteral |
  UnsignedValueSpecificationGeneral GeneralValueSpecification
  deriving (Eq, Ord, Read, Show)

_UnsignedValueSpecification = (Core.Name "hydra/langs/sql/ansi.UnsignedValueSpecification")

_UnsignedValueSpecification_literal = (Core.Name "literal")

_UnsignedValueSpecification_general = (Core.Name "general")

_UnsignedValueSpecification_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _UnsignedLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "general"),
      Core.fieldTypeType = _GeneralValueSpecification_type_}]}))

newtype UserDefinedTypeValueExpression = 
  UserDefinedTypeValueExpression {
    unUserDefinedTypeValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_UserDefinedTypeValueExpression = (Core.Name "hydra/langs/sql/ansi.UserDefinedTypeValueExpression")

_UserDefinedTypeValueExpression_type_ = _ValueExpressionPrimary_type_

data ValueExpression = 
  ValueExpressionCommon CommonValueExpression |
  ValueExpressionBoolean BooleanValueExpression |
  ValueExpressionRow RowValueExpression
  deriving (Eq, Ord, Read, Show)

_ValueExpression = (Core.Name "hydra/langs/sql/ansi.ValueExpression")

_ValueExpression_common = (Core.Name "common")

_ValueExpression_boolean = (Core.Name "boolean")

_ValueExpression_row = (Core.Name "row")

_ValueExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "common"),
      Core.fieldTypeType = _CommonValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = _BooleanValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "row"),
      Core.fieldTypeType = _RowValueExpression_type_}]}))

data ValueExpressionPrimary = 
  ValueExpressionPrimaryParens ParenthesizedValueExpression |
  ValueExpressionPrimaryNoparens NonparenthesizedValueExpressionPrimary
  deriving (Eq, Ord, Read, Show)

_ValueExpressionPrimary = (Core.Name "hydra/langs/sql/ansi.ValueExpressionPrimary")

_ValueExpressionPrimary_parens = (Core.Name "parens")

_ValueExpressionPrimary_noparens = (Core.Name "noparens")

_ValueExpressionPrimary_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "Placeholder"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parens"),
      Core.fieldTypeType = _ParenthesizedValueExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "noparens"),
      Core.fieldTypeType = _NonparenthesizedValueExpressionPrimary_type_}]}))

data WindowFunction = 
  WindowFunction {}
  deriving (Eq, Ord, Read, Show)

_WindowFunction = (Core.Name "hydra/langs/sql/ansi.WindowFunction")

_WindowFunction_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))