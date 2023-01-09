-- | A subset of ANSI SQL:2003, capturing selected productions of the BNF grammar provided at https://ronsavage.github.io/SQL/sql-2003-2.bnf.html, which is based on the Final Committee Draft (FCD) of ISO/IEC 9075-2:2003

module Hydra.Ext.Sql.Ansi where

import qualified Hydra.Core as Core
import Data.List
import Data.Map
import Data.Set

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra/ext/sql/ansi.ColumnName")

newtype DomainName = 
  DomainName {
    unDomainName :: String}
  deriving (Eq, Ord, Read, Show)

_DomainName = (Core.Name "hydra/ext/sql/ansi.DomainName")

newtype PathResolvedUserDefinedTypeName = 
  PathResolvedUserDefinedTypeName {
    unPathResolvedUserDefinedTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_PathResolvedUserDefinedTypeName = (Core.Name "hydra/ext/sql/ansi.PathResolvedUserDefinedTypeName")

newtype TableName = 
  TableName {
    unTableName :: String}
  deriving (Eq, Ord, Read, Show)

_TableName = (Core.Name "hydra/ext/sql/ansi.TableName")

newtype UnsignedInteger = 
  UnsignedInteger {
    unUnsignedInteger :: String}
  deriving (Eq, Ord, Read, Show)

_UnsignedInteger = (Core.Name "hydra/ext/sql/ansi.UnsignedInteger")

data ApproximateNumericType = 
  ApproximateNumericTypeFloat (Maybe Precision) |
  ApproximateNumericTypeReal  |
  ApproximateNumericTypeDouble 
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType")

_ApproximateNumericType_float = (Core.FieldName "float")

_ApproximateNumericType_real = (Core.FieldName "real")

_ApproximateNumericType_double = (Core.FieldName "double")

data ArrayType = 
  ArrayType {}
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/ext/sql/ansi.ArrayType")

data AsSubqueryClause = 
  AsSubqueryClause {}
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra/ext/sql/ansi.AsSubqueryClause")

data BinaryLargeObjectStringType = 
  BinaryLargeObjectStringTypeBinary (Maybe LargeObjectLength) |
  BinaryLargeObjectStringTypeBlob (Maybe LargeObjectLength)
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType")

_BinaryLargeObjectStringType_binary = (Core.FieldName "binary")

_BinaryLargeObjectStringType_blob = (Core.FieldName "blob")

data BooleanType = 
  BooleanType {}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra/ext/sql/ansi.BooleanType")

data CharacterSetSpecification = 
  CharacterSetSpecification {}
  deriving (Eq, Ord, Read, Show)

_CharacterSetSpecification = (Core.Name "hydra/ext/sql/ansi.CharacterSetSpecification")

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

_CharacterStringType = (Core.Name "hydra/ext/sql/ansi.CharacterStringType")

_CharacterStringType_character = (Core.FieldName "character")

_CharacterStringType_char = (Core.FieldName "char")

_CharacterStringType_characterVarying = (Core.FieldName "characterVarying")

_CharacterStringType_charVarying = (Core.FieldName "charVarying")

_CharacterStringType_varchar = (Core.FieldName "varchar")

_CharacterStringType_characterLargeObject = (Core.FieldName "characterLargeObject")

_CharacterStringType_charLargeObject = (Core.FieldName "charLargeObject")

_CharacterStringType_clob = (Core.FieldName "clob")

data CollateClause = 
  CollateClause {}
  deriving (Eq, Ord, Read, Show)

_CollateClause = (Core.Name "hydra/ext/sql/ansi.CollateClause")

data CollectionType = 
  CollectionTypeArray ArrayType |
  CollectionTypeMultiset MultisetType
  deriving (Eq, Ord, Read, Show)

_CollectionType = (Core.Name "hydra/ext/sql/ansi.CollectionType")

_CollectionType_array = (Core.FieldName "array")

_CollectionType_multiset = (Core.FieldName "multiset")

data ColumnConstraintDefinition = 
  ColumnConstraintDefinition {}
  deriving (Eq, Ord, Read, Show)

_ColumnConstraintDefinition = (Core.Name "hydra/ext/sql/ansi.ColumnConstraintDefinition")

data ColumnDefinition = 
  ColumnDefinition {
    columnDefinitionName :: ColumnName,
    columnDefinitionTypeOrDomain :: (Maybe ColumnDefinition_TypeOrDomain_Option),
    columnDefinitionRefScope :: (Maybe ReferenceScopeCheck),
    columnDefinitionDefaultOrIdentityOrGeneration :: (Maybe ColumnDefinition_DefaultOrIdentityOrGeneration_Option),
    columnDefinitionConstraints :: [ColumnConstraintDefinition],
    columnDefinitionCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition")

_ColumnDefinition_name = (Core.FieldName "name")

_ColumnDefinition_typeOrDomain = (Core.FieldName "typeOrDomain")

_ColumnDefinition_refScope = (Core.FieldName "refScope")

_ColumnDefinition_defaultOrIdentityOrGeneration = (Core.FieldName "defaultOrIdentityOrGeneration")

_ColumnDefinition_constraints = (Core.FieldName "constraints")

_ColumnDefinition_collate = (Core.FieldName "collate")

data ColumnDefinition_TypeOrDomain_Option = 
  ColumnDefinition_TypeOrDomain_OptionDataType DataType |
  ColumnDefinition_TypeOrDomain_OptionDomainName DomainName
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_TypeOrDomain_Option = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition.TypeOrDomain.Option")

_ColumnDefinition_TypeOrDomain_Option_dataType = (Core.FieldName "dataType")

_ColumnDefinition_TypeOrDomain_Option_domainName = (Core.FieldName "domainName")

data ColumnDefinition_DefaultOrIdentityOrGeneration_Option = 
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionDefaultClause DefaultClause |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionIdentityColumnSpecification IdentityColumnSpecification |
  ColumnDefinition_DefaultOrIdentityOrGeneration_OptionGenerationClause GenerationClause
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition.DefaultOrIdentityOrGeneration.Option")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_defaultClause = (Core.FieldName "defaultClause")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_identityColumnSpecification = (Core.FieldName "identityColumnSpecification")

_ColumnDefinition_DefaultOrIdentityOrGeneration_Option_generationClause = (Core.FieldName "generationClause")

data ColumnOptions = 
  ColumnOptions {}
  deriving (Eq, Ord, Read, Show)

_ColumnOptions = (Core.Name "hydra/ext/sql/ansi.ColumnOptions")

data DataType = 
  DataTypePredefined PredefinedType |
  DataTypeRow RowType |
  DataTypeNamed PathResolvedUserDefinedTypeName |
  DataTypeReference ReferenceType |
  DataTypeCollection CollectionType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra/ext/sql/ansi.DataType")

_DataType_predefined = (Core.FieldName "predefined")

_DataType_row = (Core.FieldName "row")

_DataType_named = (Core.FieldName "named")

_DataType_reference = (Core.FieldName "reference")

_DataType_collection = (Core.FieldName "collection")

data DatetimeType = 
  DatetimeType {}
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra/ext/sql/ansi.DatetimeType")

data DefaultClause = 
  DefaultClause {}
  deriving (Eq, Ord, Read, Show)

_DefaultClause = (Core.Name "hydra/ext/sql/ansi.DefaultClause")

data ExactNumericType = 
  ExactNumericTypeNumeric (Maybe ExactNumericType_Numeric_Option) |
  ExactNumericTypeDecimal (Maybe ExactNumericType_Decimal_Option) |
  ExactNumericTypeDec (Maybe ExactNumericType_Dec_Option) |
  ExactNumericTypeSmallint  |
  ExactNumericTypeInteger  |
  ExactNumericTypeInt  |
  ExactNumericTypeBigint 
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = (Core.Name "hydra/ext/sql/ansi.ExactNumericType")

_ExactNumericType_numeric = (Core.FieldName "numeric")

_ExactNumericType_decimal = (Core.FieldName "decimal")

_ExactNumericType_dec = (Core.FieldName "dec")

_ExactNumericType_smallint = (Core.FieldName "smallint")

_ExactNumericType_integer = (Core.FieldName "integer")

_ExactNumericType_int = (Core.FieldName "int")

_ExactNumericType_bigint = (Core.FieldName "bigint")

data ExactNumericType_Numeric_Option = 
  ExactNumericType_Numeric_Option {
    exactNumericType_Numeric_OptionPrecision :: Precision,
    exactNumericType_Numeric_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Numeric.Option")

_ExactNumericType_Numeric_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Numeric_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Decimal_Option = 
  ExactNumericType_Decimal_Option {
    exactNumericType_Decimal_OptionPrecision :: Precision,
    exactNumericType_Decimal_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Decimal.Option")

_ExactNumericType_Decimal_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Decimal_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Dec_Option = 
  ExactNumericType_Dec_Option {
    exactNumericType_Dec_OptionPrecision :: Precision,
    exactNumericType_Dec_OptionSequence :: (Maybe Scale)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Dec.Option")

_ExactNumericType_Dec_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Dec_Option_sequence = (Core.FieldName "sequence")

data GenerationClause = 
  GenerationClause {}
  deriving (Eq, Ord, Read, Show)

_GenerationClause = (Core.Name "hydra/ext/sql/ansi.GenerationClause")

data GlobalOrLocal = 
  GlobalOrLocalGlobal  |
  GlobalOrLocalLocal 
  deriving (Eq, Ord, Read, Show)

_GlobalOrLocal = (Core.Name "hydra/ext/sql/ansi.GlobalOrLocal")

_GlobalOrLocal_global = (Core.FieldName "global")

_GlobalOrLocal_local = (Core.FieldName "local")

data IdentityColumnSpecification = 
  IdentityColumnSpecification {}
  deriving (Eq, Ord, Read, Show)

_IdentityColumnSpecification = (Core.Name "hydra/ext/sql/ansi.IdentityColumnSpecification")

data IntervalType = 
  IntervalType {}
  deriving (Eq, Ord, Read, Show)

_IntervalType = (Core.Name "hydra/ext/sql/ansi.IntervalType")

data LargeObjectLength = 
  LargeObjectLength {}
  deriving (Eq, Ord, Read, Show)

_LargeObjectLength = (Core.Name "hydra/ext/sql/ansi.LargeObjectLength")

newtype Length = 
  Length {
    unLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Length = (Core.Name "hydra/ext/sql/ansi.Length")

data LikeClause = 
  LikeClause {}
  deriving (Eq, Ord, Read, Show)

_LikeClause = (Core.Name "hydra/ext/sql/ansi.LikeClause")

newtype MultisetType = 
  MultisetType {
    unMultisetType :: DataType}
  deriving (Eq, Ord, Read, Show)

_MultisetType = (Core.Name "hydra/ext/sql/ansi.MultisetType")

data NationalCharacterStringType = 
  NationalCharacterStringType {}
  deriving (Eq, Ord, Read, Show)

_NationalCharacterStringType = (Core.Name "hydra/ext/sql/ansi.NationalCharacterStringType")

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/ext/sql/ansi.NumericType")

_NumericType_exact = (Core.FieldName "exact")

_NumericType_approximate = (Core.FieldName "approximate")

newtype Precision = 
  Precision {
    unPrecision :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra/ext/sql/ansi.Precision")

data PredefinedType = 
  PredefinedTypeCharacterString PredefinedType_CharacterString |
  PredefinedTypeNationalCharacterString PredefinedType_NationalCharacterString |
  PredefinedTypeBlob BinaryLargeObjectStringType |
  PredefinedTypeNumeric NumericType |
  PredefinedTypeBoolean BooleanType |
  PredefinedTypeDatetime DatetimeType |
  PredefinedTypeInterval IntervalType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = (Core.Name "hydra/ext/sql/ansi.PredefinedType")

_PredefinedType_characterString = (Core.FieldName "characterString")

_PredefinedType_nationalCharacterString = (Core.FieldName "nationalCharacterString")

_PredefinedType_blob = (Core.FieldName "blob")

_PredefinedType_numeric = (Core.FieldName "numeric")

_PredefinedType_boolean = (Core.FieldName "boolean")

_PredefinedType_datetime = (Core.FieldName "datetime")

_PredefinedType_interval = (Core.FieldName "interval")

data PredefinedType_CharacterString = 
  PredefinedType_CharacterString {
    predefinedType_CharacterStringType :: CharacterStringType,
    predefinedType_CharacterStringCharacters :: (Maybe CharacterSetSpecification),
    predefinedType_CharacterStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_CharacterString = (Core.Name "hydra/ext/sql/ansi.PredefinedType.CharacterString")

_PredefinedType_CharacterString_type = (Core.FieldName "type")

_PredefinedType_CharacterString_characters = (Core.FieldName "characters")

_PredefinedType_CharacterString_collate = (Core.FieldName "collate")

data PredefinedType_NationalCharacterString = 
  PredefinedType_NationalCharacterString {
    predefinedType_NationalCharacterStringType :: NationalCharacterStringType,
    predefinedType_NationalCharacterStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_NationalCharacterString = (Core.Name "hydra/ext/sql/ansi.PredefinedType.NationalCharacterString")

_PredefinedType_NationalCharacterString_type = (Core.FieldName "type")

_PredefinedType_NationalCharacterString_collate = (Core.FieldName "collate")

data ReferenceScopeCheck = 
  ReferenceScopeCheck {}
  deriving (Eq, Ord, Read, Show)

_ReferenceScopeCheck = (Core.Name "hydra/ext/sql/ansi.ReferenceScopeCheck")

data ReferenceType = 
  ReferenceType {}
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra/ext/sql/ansi.ReferenceType")

data RowType = 
  RowType {}
  deriving (Eq, Ord, Read, Show)

_RowType = (Core.Name "hydra/ext/sql/ansi.RowType")

newtype Scale = 
  Scale {
    unScale :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Scale = (Core.Name "hydra/ext/sql/ansi.Scale")

data SelfReferencingColumnSpecification = 
  SelfReferencingColumnSpecification {}
  deriving (Eq, Ord, Read, Show)

_SelfReferencingColumnSpecification = (Core.Name "hydra/ext/sql/ansi.SelfReferencingColumnSpecification")

data SubtableClause = 
  SubtableClause {}
  deriving (Eq, Ord, Read, Show)

_SubtableClause = (Core.Name "hydra/ext/sql/ansi.SubtableClause")

data TableCommitAction = 
  TableCommitActionPreserve  |
  TableCommitActionDelete 
  deriving (Eq, Ord, Read, Show)

_TableCommitAction = (Core.Name "hydra/ext/sql/ansi.TableCommitAction")

_TableCommitAction_preserve = (Core.FieldName "preserve")

_TableCommitAction_delete = (Core.FieldName "delete")

data TableConstraintDefinition = 
  TableConstraintDefinition {}
  deriving (Eq, Ord, Read, Show)

_TableConstraintDefinition = (Core.Name "hydra/ext/sql/ansi.TableConstraintDefinition")

data TableContentsSource = 
  TableContentsSourceList TableElementList |
  TableContentsSourceSubtable TableContentsSource_Subtable |
  TableContentsSourceSubquery AsSubqueryClause
  deriving (Eq, Ord, Read, Show)

_TableContentsSource = (Core.Name "hydra/ext/sql/ansi.TableContentsSource")

_TableContentsSource_list = (Core.FieldName "list")

_TableContentsSource_subtable = (Core.FieldName "subtable")

_TableContentsSource_subquery = (Core.FieldName "subquery")

data TableContentsSource_Subtable = 
  TableContentsSource_Subtable {
    tableContentsSource_SubtableType :: PathResolvedUserDefinedTypeName,
    tableContentsSource_SubtableSubtable :: (Maybe SubtableClause),
    tableContentsSource_SubtableElements :: (Maybe TableElementList)}
  deriving (Eq, Ord, Read, Show)

_TableContentsSource_Subtable = (Core.Name "hydra/ext/sql/ansi.TableContentsSource.Subtable")

_TableContentsSource_Subtable_type = (Core.FieldName "type")

_TableContentsSource_Subtable_subtable = (Core.FieldName "subtable")

_TableContentsSource_Subtable_elements = (Core.FieldName "elements")

data TableDefinition = 
  TableDefinition {
    tableDefinitionScope :: (Maybe TableScope),
    tableDefinitionName :: TableName,
    tableDefinitionSource :: TableContentsSource,
    tableDefinitionCommitActions :: (Maybe TableCommitAction)}
  deriving (Eq, Ord, Read, Show)

_TableDefinition = (Core.Name "hydra/ext/sql/ansi.TableDefinition")

_TableDefinition_scope = (Core.FieldName "scope")

_TableDefinition_name = (Core.FieldName "name")

_TableDefinition_source = (Core.FieldName "source")

_TableDefinition_commitActions = (Core.FieldName "commitActions")

data TableElement = 
  TableElementColumn ColumnDefinition |
  TableElementTableConstraint TableConstraintDefinition |
  TableElementLike LikeClause |
  TableElementSelfReferencingColumn SelfReferencingColumnSpecification |
  TableElementColumOptions ColumnOptions
  deriving (Eq, Ord, Read, Show)

_TableElement = (Core.Name "hydra/ext/sql/ansi.TableElement")

_TableElement_column = (Core.FieldName "column")

_TableElement_tableConstraint = (Core.FieldName "tableConstraint")

_TableElement_like = (Core.FieldName "like")

_TableElement_selfReferencingColumn = (Core.FieldName "selfReferencingColumn")

_TableElement_columOptions = (Core.FieldName "columOptions")

data TableElementList = 
  TableElementList {
    tableElementListFirst :: TableElement,
    tableElementListRest :: [TableElement]}
  deriving (Eq, Ord, Read, Show)

_TableElementList = (Core.Name "hydra/ext/sql/ansi.TableElementList")

_TableElementList_first = (Core.FieldName "first")

_TableElementList_rest = (Core.FieldName "rest")

newtype TableScope = 
  TableScope {
    unTableScope :: GlobalOrLocal}
  deriving (Eq, Ord, Read, Show)

_TableScope = (Core.Name "hydra/ext/sql/ansi.TableScope")