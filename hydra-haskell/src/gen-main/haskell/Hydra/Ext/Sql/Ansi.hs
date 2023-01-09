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
  ApproximateNumericTypeFloat ApproximateNumericType_Float |
  ApproximateNumericTypeReal  |
  ApproximateNumericTypeDouble 
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType")

_ApproximateNumericType_float = (Core.FieldName "float")

_ApproximateNumericType_real = (Core.FieldName "real")

_ApproximateNumericType_double = (Core.FieldName "double")

data ApproximateNumericType_Float = 
  ApproximateNumericType_Float {
    approximateNumericType_FloatSequence :: (Maybe ApproximateNumericType_Float_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType_Float = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType.Float")

_ApproximateNumericType_Float_sequence = (Core.FieldName "sequence")

data ApproximateNumericType_Float_Sequence_Option = 
  ApproximateNumericType_Float_Sequence_Option {
    approximateNumericType_Float_Sequence_OptionPrecision :: Precision}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType_Float_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType.Float.Sequence.Option")

_ApproximateNumericType_Float_Sequence_Option_precision = (Core.FieldName "precision")

data ArrayType = 
  
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/ext/sql/ansi.ArrayType")

data AsSubqueryClause = 
  
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra/ext/sql/ansi.AsSubqueryClause")

data BinaryLargeObjectStringType = 
  BinaryLargeObjectStringTypeBinary BinaryLargeObjectStringType_Binary |
  BinaryLargeObjectStringTypeBlob BinaryLargeObjectStringType_Blob
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType")

_BinaryLargeObjectStringType_binary = (Core.FieldName "binary")

_BinaryLargeObjectStringType_blob = (Core.FieldName "blob")

data BinaryLargeObjectStringType_Binary = 
  BinaryLargeObjectStringType_Binary {
    binaryLargeObjectStringType_BinarySequence :: (Maybe BinaryLargeObjectStringType_Binary_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Binary = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Binary")

_BinaryLargeObjectStringType_Binary_sequence = (Core.FieldName "sequence")

data BinaryLargeObjectStringType_Binary_Sequence_Option = 
  BinaryLargeObjectStringType_Binary_Sequence_Option {
    binaryLargeObjectStringType_Binary_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Binary_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Binary.Sequence.Option")

_BinaryLargeObjectStringType_Binary_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data BinaryLargeObjectStringType_Blob = 
  BinaryLargeObjectStringType_Blob {
    binaryLargeObjectStringType_BlobSequence :: (Maybe BinaryLargeObjectStringType_Blob_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Blob = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Blob")

_BinaryLargeObjectStringType_Blob_sequence = (Core.FieldName "sequence")

data BinaryLargeObjectStringType_Blob_Sequence_Option = 
  BinaryLargeObjectStringType_Blob_Sequence_Option {
    binaryLargeObjectStringType_Blob_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Blob_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Blob.Sequence.Option")

_BinaryLargeObjectStringType_Blob_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data BooleanType = 
  BooleanType {}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra/ext/sql/ansi.BooleanType")

data CharacterSetSpecification = 
  
  deriving (Eq, Ord, Read, Show)

_CharacterSetSpecification = (Core.Name "hydra/ext/sql/ansi.CharacterSetSpecification")

data CharacterStringType = 
  CharacterStringTypeCharacter CharacterStringType_Character |
  CharacterStringTypeChar CharacterStringType_Char |
  CharacterStringTypeCharacterVarying CharacterStringType_CharacterVarying |
  CharacterStringTypeCharVarying CharacterStringType_CharVarying |
  CharacterStringTypeVarchar CharacterStringType_Varchar |
  CharacterStringTypeCharacterLargeObject CharacterStringType_CharacterLargeObject |
  CharacterStringTypeCharLargeObject CharacterStringType_CharLargeObject |
  CharacterStringTypeClob CharacterStringType_Clob
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

data CharacterStringType_Character = 
  CharacterStringType_Character {
    characterStringType_CharacterSequence :: (Maybe CharacterStringType_Character_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Character = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Character")

_CharacterStringType_Character_sequence = (Core.FieldName "sequence")

data CharacterStringType_Character_Sequence_Option = 
  CharacterStringType_Character_Sequence_Option {
    characterStringType_Character_Sequence_OptionLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Character_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Character.Sequence.Option")

_CharacterStringType_Character_Sequence_Option_length = (Core.FieldName "length")

data CharacterStringType_Char = 
  CharacterStringType_Char {
    characterStringType_CharSequence :: (Maybe CharacterStringType_Char_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Char = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Char")

_CharacterStringType_Char_sequence = (Core.FieldName "sequence")

data CharacterStringType_Char_Sequence_Option = 
  CharacterStringType_Char_Sequence_Option {
    characterStringType_Char_Sequence_OptionLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Char_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Char.Sequence.Option")

_CharacterStringType_Char_Sequence_Option_length = (Core.FieldName "length")

data CharacterStringType_CharacterVarying = 
  CharacterStringType_CharacterVarying {
    characterStringType_CharacterVaryingLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharacterVarying = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharacterVarying")

_CharacterStringType_CharacterVarying_length = (Core.FieldName "length")

data CharacterStringType_CharVarying = 
  CharacterStringType_CharVarying {
    characterStringType_CharVaryingLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharVarying = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharVarying")

_CharacterStringType_CharVarying_length = (Core.FieldName "length")

data CharacterStringType_Varchar = 
  CharacterStringType_Varchar {
    characterStringType_VarcharLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Varchar = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Varchar")

_CharacterStringType_Varchar_length = (Core.FieldName "length")

data CharacterStringType_CharacterLargeObject = 
  CharacterStringType_CharacterLargeObject {
    characterStringType_CharacterLargeObjectSequence :: (Maybe CharacterStringType_CharacterLargeObject_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharacterLargeObject = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharacterLargeObject")

_CharacterStringType_CharacterLargeObject_sequence = (Core.FieldName "sequence")

data CharacterStringType_CharacterLargeObject_Sequence_Option = 
  CharacterStringType_CharacterLargeObject_Sequence_Option {
    characterStringType_CharacterLargeObject_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharacterLargeObject_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharacterLargeObject.Sequence.Option")

_CharacterStringType_CharacterLargeObject_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CharacterStringType_CharLargeObject = 
  CharacterStringType_CharLargeObject {
    characterStringType_CharLargeObjectSequence :: (Maybe CharacterStringType_CharLargeObject_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharLargeObject = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharLargeObject")

_CharacterStringType_CharLargeObject_sequence = (Core.FieldName "sequence")

data CharacterStringType_CharLargeObject_Sequence_Option = 
  CharacterStringType_CharLargeObject_Sequence_Option {
    characterStringType_CharLargeObject_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_CharLargeObject_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.CharLargeObject.Sequence.Option")

_CharacterStringType_CharLargeObject_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CharacterStringType_Clob = 
  CharacterStringType_Clob {
    characterStringType_ClobSequence :: (Maybe CharacterStringType_Clob_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Clob = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Clob")

_CharacterStringType_Clob_sequence = (Core.FieldName "sequence")

data CharacterStringType_Clob_Sequence_Option = 
  CharacterStringType_Clob_Sequence_Option {
    characterStringType_Clob_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Clob_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Clob.Sequence.Option")

_CharacterStringType_Clob_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CollateClause = 
  
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
  
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra/ext/sql/ansi.DatetimeType")

data DefaultClause = 
  
  deriving (Eq, Ord, Read, Show)

_DefaultClause = (Core.Name "hydra/ext/sql/ansi.DefaultClause")

data ExactNumericType = 
  ExactNumericTypeNumeric ExactNumericType_Numeric |
  ExactNumericTypeDecimal ExactNumericType_Decimal |
  ExactNumericTypeDec ExactNumericType_Dec |
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

data ExactNumericType_Numeric = 
  ExactNumericType_Numeric {
    exactNumericType_NumericSequence :: (Maybe ExactNumericType_Numeric_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Numeric")

_ExactNumericType_Numeric_sequence = (Core.FieldName "sequence")

data ExactNumericType_Numeric_Sequence_Option = 
  ExactNumericType_Numeric_Sequence_Option {
    exactNumericType_Numeric_Sequence_OptionPrecision :: Precision,
    exactNumericType_Numeric_Sequence_OptionSequence :: (Maybe ExactNumericType_Numeric_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Numeric.Sequence.Option")

_ExactNumericType_Numeric_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Numeric_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Numeric_Sequence_Option_Sequence_Option = 
  ExactNumericType_Numeric_Sequence_Option_Sequence_Option {
    exactNumericType_Numeric_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Numeric_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Numeric.Sequence.Option.Sequence.Option")

_ExactNumericType_Numeric_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data ExactNumericType_Decimal = 
  ExactNumericType_Decimal {
    exactNumericType_DecimalSequence :: (Maybe ExactNumericType_Decimal_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Decimal")

_ExactNumericType_Decimal_sequence = (Core.FieldName "sequence")

data ExactNumericType_Decimal_Sequence_Option = 
  ExactNumericType_Decimal_Sequence_Option {
    exactNumericType_Decimal_Sequence_OptionPrecision :: Precision,
    exactNumericType_Decimal_Sequence_OptionSequence :: (Maybe ExactNumericType_Decimal_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Decimal.Sequence.Option")

_ExactNumericType_Decimal_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Decimal_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Decimal_Sequence_Option_Sequence_Option = 
  ExactNumericType_Decimal_Sequence_Option_Sequence_Option {
    exactNumericType_Decimal_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Decimal_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Decimal.Sequence.Option.Sequence.Option")

_ExactNumericType_Decimal_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data ExactNumericType_Dec = 
  ExactNumericType_Dec {
    exactNumericType_DecSequence :: (Maybe ExactNumericType_Dec_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Dec")

_ExactNumericType_Dec_sequence = (Core.FieldName "sequence")

data ExactNumericType_Dec_Sequence_Option = 
  ExactNumericType_Dec_Sequence_Option {
    exactNumericType_Dec_Sequence_OptionPrecision :: Precision,
    exactNumericType_Dec_Sequence_OptionSequence :: (Maybe ExactNumericType_Dec_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Dec.Sequence.Option")

_ExactNumericType_Dec_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Dec_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Dec_Sequence_Option_Sequence_Option = 
  ExactNumericType_Dec_Sequence_Option_Sequence_Option {
    exactNumericType_Dec_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Dec_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Dec.Sequence.Option.Sequence.Option")

_ExactNumericType_Dec_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data GenerationClause = 
  
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
  
  deriving (Eq, Ord, Read, Show)

_IdentityColumnSpecification = (Core.Name "hydra/ext/sql/ansi.IdentityColumnSpecification")

data IntervalType = 
  
  deriving (Eq, Ord, Read, Show)

_IntervalType = (Core.Name "hydra/ext/sql/ansi.IntervalType")

data LargeObjectLength = 
  
  deriving (Eq, Ord, Read, Show)

_LargeObjectLength = (Core.Name "hydra/ext/sql/ansi.LargeObjectLength")

newtype Length = 
  Length {
    unLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Length = (Core.Name "hydra/ext/sql/ansi.Length")

data LikeClause = 
  
  deriving (Eq, Ord, Read, Show)

_LikeClause = (Core.Name "hydra/ext/sql/ansi.LikeClause")

data MultisetType = 
  MultisetType {
    multisetTypeDataType :: DataType}
  deriving (Eq, Ord, Read, Show)

_MultisetType = (Core.Name "hydra/ext/sql/ansi.MultisetType")

_MultisetType_dataType = (Core.FieldName "dataType")

data NationalCharacterStringType = 
  
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
    predefinedType_CharacterStringCharacters :: (Maybe PredefinedType_CharacterString_Characters_Option),
    predefinedType_CharacterStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_CharacterString = (Core.Name "hydra/ext/sql/ansi.PredefinedType.CharacterString")

_PredefinedType_CharacterString_type = (Core.FieldName "type")

_PredefinedType_CharacterString_characters = (Core.FieldName "characters")

_PredefinedType_CharacterString_collate = (Core.FieldName "collate")

data PredefinedType_CharacterString_Characters_Option = 
  PredefinedType_CharacterString_Characters_Option {
    predefinedType_CharacterString_Characters_OptionCharacterSetSpecification :: CharacterSetSpecification}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_CharacterString_Characters_Option = (Core.Name "hydra/ext/sql/ansi.PredefinedType.CharacterString.Characters.Option")

_PredefinedType_CharacterString_Characters_Option_characterSetSpecification = (Core.FieldName "characterSetSpecification")

data PredefinedType_NationalCharacterString = 
  PredefinedType_NationalCharacterString {
    predefinedType_NationalCharacterStringType :: NationalCharacterStringType,
    predefinedType_NationalCharacterStringCollate :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_NationalCharacterString = (Core.Name "hydra/ext/sql/ansi.PredefinedType.NationalCharacterString")

_PredefinedType_NationalCharacterString_type = (Core.FieldName "type")

_PredefinedType_NationalCharacterString_collate = (Core.FieldName "collate")

data ReferenceScopeCheck = 
  
  deriving (Eq, Ord, Read, Show)

_ReferenceScopeCheck = (Core.Name "hydra/ext/sql/ansi.ReferenceScopeCheck")

data ReferenceType = 
  
  deriving (Eq, Ord, Read, Show)

_ReferenceType = (Core.Name "hydra/ext/sql/ansi.ReferenceType")

data RowType = 
  
  deriving (Eq, Ord, Read, Show)

_RowType = (Core.Name "hydra/ext/sql/ansi.RowType")

newtype Scale = 
  Scale {
    unScale :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Scale = (Core.Name "hydra/ext/sql/ansi.Scale")

data SelfReferencingColumnSpecification = 
  
  deriving (Eq, Ord, Read, Show)

_SelfReferencingColumnSpecification = (Core.Name "hydra/ext/sql/ansi.SelfReferencingColumnSpecification")

data SubtableClause = 
  
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
    tableDefinitionCommitActions :: (Maybe TableDefinition_CommitActions_Option)}
  deriving (Eq, Ord, Read, Show)

_TableDefinition = (Core.Name "hydra/ext/sql/ansi.TableDefinition")

_TableDefinition_scope = (Core.FieldName "scope")

_TableDefinition_name = (Core.FieldName "name")

_TableDefinition_source = (Core.FieldName "source")

_TableDefinition_commitActions = (Core.FieldName "commitActions")

data TableDefinition_CommitActions_Option = 
  TableDefinition_CommitActions_Option {
    tableDefinition_CommitActions_OptionTableCommitAction :: TableCommitAction}
  deriving (Eq, Ord, Read, Show)

_TableDefinition_CommitActions_Option = (Core.Name "hydra/ext/sql/ansi.TableDefinition.CommitActions.Option")

_TableDefinition_CommitActions_Option_tableCommitAction = (Core.FieldName "tableCommitAction")

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
    tableElementListRest :: [TableElementList_Rest_Elmt]}
  deriving (Eq, Ord, Read, Show)

_TableElementList = (Core.Name "hydra/ext/sql/ansi.TableElementList")

_TableElementList_first = (Core.FieldName "first")

_TableElementList_rest = (Core.FieldName "rest")

data TableElementList_Rest_Elmt = 
  TableElementList_Rest_Elmt {
    tableElementList_Rest_ElmtTableElement :: TableElement}
  deriving (Eq, Ord, Read, Show)

_TableElementList_Rest_Elmt = (Core.Name "hydra/ext/sql/ansi.TableElementList.Rest.Elmt")

_TableElementList_Rest_Elmt_tableElement = (Core.FieldName "tableElement")

data TableScope = 
  TableScope {
    tableScopeGlobalOrLocal :: GlobalOrLocal}
  deriving (Eq, Ord, Read, Show)

_TableScope = (Core.Name "hydra/ext/sql/ansi.TableScope")

_TableScope_globalOrLocal = (Core.FieldName "globalOrLocal")