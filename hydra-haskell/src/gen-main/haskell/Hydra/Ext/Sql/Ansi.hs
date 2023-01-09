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
  ApproximateNumericTypeSequence ApproximateNumericType_Sequence |
  ApproximateNumericTypeREAL  |
  ApproximateNumericTypeDOUBLESpPRECISION 
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType")

_ApproximateNumericType_sequence = (Core.FieldName "sequence")

_ApproximateNumericType_rEAL = (Core.FieldName "rEAL")

_ApproximateNumericType_dOUBLESpPRECISION = (Core.FieldName "dOUBLESpPRECISION")

data ApproximateNumericType_Sequence = 
  ApproximateNumericType_Sequence {
    approximateNumericType_SequenceSequence :: (Maybe ApproximateNumericType_Sequence_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType_Sequence = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType.Sequence")

_ApproximateNumericType_Sequence_sequence = (Core.FieldName "sequence")

data ApproximateNumericType_Sequence_Sequence_Option = 
  ApproximateNumericType_Sequence_Sequence_Option {
    approximateNumericType_Sequence_Sequence_OptionPrecision :: Precision}
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType_Sequence_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ApproximateNumericType.Sequence.Sequence.Option")

_ApproximateNumericType_Sequence_Sequence_Option_precision = (Core.FieldName "precision")

data ArrayType = 
  
  deriving (Eq, Ord, Read, Show)

_ArrayType = (Core.Name "hydra/ext/sql/ansi.ArrayType")

data AsSubqueryClause = 
  
  deriving (Eq, Ord, Read, Show)

_AsSubqueryClause = (Core.Name "hydra/ext/sql/ansi.AsSubqueryClause")

data BinaryLargeObjectStringType = 
  BinaryLargeObjectStringTypeSequence BinaryLargeObjectStringType_Sequence |
  BinaryLargeObjectStringTypeSequence2 BinaryLargeObjectStringType_Sequence2
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType")

_BinaryLargeObjectStringType_sequence = (Core.FieldName "sequence")

_BinaryLargeObjectStringType_sequence2 = (Core.FieldName "sequence2")

data BinaryLargeObjectStringType_Sequence = 
  BinaryLargeObjectStringType_Sequence {
    binaryLargeObjectStringType_SequenceSequence :: (Maybe BinaryLargeObjectStringType_Sequence_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Sequence = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Sequence")

_BinaryLargeObjectStringType_Sequence_sequence = (Core.FieldName "sequence")

data BinaryLargeObjectStringType_Sequence_Sequence_Option = 
  BinaryLargeObjectStringType_Sequence_Sequence_Option {
    binaryLargeObjectStringType_Sequence_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Sequence_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Sequence.Sequence.Option")

_BinaryLargeObjectStringType_Sequence_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data BinaryLargeObjectStringType_Sequence2 = 
  BinaryLargeObjectStringType_Sequence2 {
    binaryLargeObjectStringType_Sequence2Sequence :: (Maybe BinaryLargeObjectStringType_Sequence2_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Sequence2 = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Sequence2")

_BinaryLargeObjectStringType_Sequence2_sequence = (Core.FieldName "sequence")

data BinaryLargeObjectStringType_Sequence2_Sequence_Option = 
  BinaryLargeObjectStringType_Sequence2_Sequence_Option {
    binaryLargeObjectStringType_Sequence2_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_BinaryLargeObjectStringType_Sequence2_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.BinaryLargeObjectStringType.Sequence2.Sequence.Option")

_BinaryLargeObjectStringType_Sequence2_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data BooleanType = 
  BooleanType {}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra/ext/sql/ansi.BooleanType")

data CharacterSetSpecification = 
  
  deriving (Eq, Ord, Read, Show)

_CharacterSetSpecification = (Core.Name "hydra/ext/sql/ansi.CharacterSetSpecification")

data CharacterStringType = 
  CharacterStringTypeSequence CharacterStringType_Sequence |
  CharacterStringTypeSequence2 CharacterStringType_Sequence2 |
  CharacterStringTypeSequence3 CharacterStringType_Sequence3 |
  CharacterStringTypeSequence4 CharacterStringType_Sequence4 |
  CharacterStringTypeSequence5 CharacterStringType_Sequence5 |
  CharacterStringTypeSequence6 CharacterStringType_Sequence6 |
  CharacterStringTypeSequence7 CharacterStringType_Sequence7 |
  CharacterStringTypeSequence8 CharacterStringType_Sequence8
  deriving (Eq, Ord, Read, Show)

_CharacterStringType = (Core.Name "hydra/ext/sql/ansi.CharacterStringType")

_CharacterStringType_sequence = (Core.FieldName "sequence")

_CharacterStringType_sequence2 = (Core.FieldName "sequence2")

_CharacterStringType_sequence3 = (Core.FieldName "sequence3")

_CharacterStringType_sequence4 = (Core.FieldName "sequence4")

_CharacterStringType_sequence5 = (Core.FieldName "sequence5")

_CharacterStringType_sequence6 = (Core.FieldName "sequence6")

_CharacterStringType_sequence7 = (Core.FieldName "sequence7")

_CharacterStringType_sequence8 = (Core.FieldName "sequence8")

data CharacterStringType_Sequence = 
  CharacterStringType_Sequence {
    characterStringType_SequenceSequence :: (Maybe CharacterStringType_Sequence_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence")

_CharacterStringType_Sequence_sequence = (Core.FieldName "sequence")

data CharacterStringType_Sequence_Sequence_Option = 
  CharacterStringType_Sequence_Sequence_Option {
    characterStringType_Sequence_Sequence_OptionLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence.Sequence.Option")

_CharacterStringType_Sequence_Sequence_Option_length = (Core.FieldName "length")

data CharacterStringType_Sequence2 = 
  CharacterStringType_Sequence2 {
    characterStringType_Sequence2Sequence :: (Maybe CharacterStringType_Sequence2_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence2 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence2")

_CharacterStringType_Sequence2_sequence = (Core.FieldName "sequence")

data CharacterStringType_Sequence2_Sequence_Option = 
  CharacterStringType_Sequence2_Sequence_Option {
    characterStringType_Sequence2_Sequence_OptionLength :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence2_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence2.Sequence.Option")

_CharacterStringType_Sequence2_Sequence_Option_length = (Core.FieldName "length")

data CharacterStringType_Sequence3 = 
  CharacterStringType_Sequence3 {
    characterStringType_Sequence3Length :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence3 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence3")

_CharacterStringType_Sequence3_length = (Core.FieldName "length")

data CharacterStringType_Sequence4 = 
  CharacterStringType_Sequence4 {
    characterStringType_Sequence4Length :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence4 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence4")

_CharacterStringType_Sequence4_length = (Core.FieldName "length")

data CharacterStringType_Sequence5 = 
  CharacterStringType_Sequence5 {
    characterStringType_Sequence5Length :: Length}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence5 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence5")

_CharacterStringType_Sequence5_length = (Core.FieldName "length")

data CharacterStringType_Sequence6 = 
  CharacterStringType_Sequence6 {
    characterStringType_Sequence6Sequence :: (Maybe CharacterStringType_Sequence6_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence6 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence6")

_CharacterStringType_Sequence6_sequence = (Core.FieldName "sequence")

data CharacterStringType_Sequence6_Sequence_Option = 
  CharacterStringType_Sequence6_Sequence_Option {
    characterStringType_Sequence6_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence6_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence6.Sequence.Option")

_CharacterStringType_Sequence6_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CharacterStringType_Sequence7 = 
  CharacterStringType_Sequence7 {
    characterStringType_Sequence7Sequence :: (Maybe CharacterStringType_Sequence7_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence7 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence7")

_CharacterStringType_Sequence7_sequence = (Core.FieldName "sequence")

data CharacterStringType_Sequence7_Sequence_Option = 
  CharacterStringType_Sequence7_Sequence_Option {
    characterStringType_Sequence7_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence7_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence7.Sequence.Option")

_CharacterStringType_Sequence7_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CharacterStringType_Sequence8 = 
  CharacterStringType_Sequence8 {
    characterStringType_Sequence8Sequence :: (Maybe CharacterStringType_Sequence8_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence8 = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence8")

_CharacterStringType_Sequence8_sequence = (Core.FieldName "sequence")

data CharacterStringType_Sequence8_Sequence_Option = 
  CharacterStringType_Sequence8_Sequence_Option {
    characterStringType_Sequence8_Sequence_OptionLargeObjectLength :: LargeObjectLength}
  deriving (Eq, Ord, Read, Show)

_CharacterStringType_Sequence8_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.CharacterStringType.Sequence8.Sequence.Option")

_CharacterStringType_Sequence8_Sequence_Option_largeObjectLength = (Core.FieldName "largeObjectLength")

data CollateClause = 
  
  deriving (Eq, Ord, Read, Show)

_CollateClause = (Core.Name "hydra/ext/sql/ansi.CollateClause")

data CollectionType = 
  CollectionTypeArrayType ArrayType |
  CollectionTypeMultisetType MultisetType
  deriving (Eq, Ord, Read, Show)

_CollectionType = (Core.Name "hydra/ext/sql/ansi.CollectionType")

_CollectionType_arrayType = (Core.FieldName "arrayType")

_CollectionType_multisetType = (Core.FieldName "multisetType")

data ColumnConstraintDefinition = 
  
  deriving (Eq, Ord, Read, Show)

_ColumnConstraintDefinition = (Core.Name "hydra/ext/sql/ansi.ColumnConstraintDefinition")

data ColumnDefinition = 
  ColumnDefinition {
    columnDefinitionColumnName :: ColumnName,
    columnDefinitionAlts :: (Maybe ColumnDefinition_Alts_Option),
    columnDefinitionReferenceScopeCheck :: (Maybe ReferenceScopeCheck),
    columnDefinitionAlts2 :: (Maybe ColumnDefinition_Alts2_Option),
    columnDefinitionListOfColumnConstraintDefinition :: [ColumnConstraintDefinition],
    columnDefinitionCollateClause :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition")

_ColumnDefinition_columnName = (Core.FieldName "columnName")

_ColumnDefinition_alts = (Core.FieldName "alts")

_ColumnDefinition_referenceScopeCheck = (Core.FieldName "referenceScopeCheck")

_ColumnDefinition_alts2 = (Core.FieldName "alts2")

_ColumnDefinition_listOfColumnConstraintDefinition = (Core.FieldName "listOfColumnConstraintDefinition")

_ColumnDefinition_collateClause = (Core.FieldName "collateClause")

data ColumnDefinition_Alts_Option = 
  ColumnDefinition_Alts_OptionDataType DataType |
  ColumnDefinition_Alts_OptionDomainName DomainName
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_Alts_Option = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition.Alts.Option")

_ColumnDefinition_Alts_Option_dataType = (Core.FieldName "dataType")

_ColumnDefinition_Alts_Option_domainName = (Core.FieldName "domainName")

data ColumnDefinition_Alts2_Option = 
  ColumnDefinition_Alts2_OptionDefaultClause DefaultClause |
  ColumnDefinition_Alts2_OptionIdentityColumnSpecification IdentityColumnSpecification |
  ColumnDefinition_Alts2_OptionGenerationClause GenerationClause
  deriving (Eq, Ord, Read, Show)

_ColumnDefinition_Alts2_Option = (Core.Name "hydra/ext/sql/ansi.ColumnDefinition.Alts2.Option")

_ColumnDefinition_Alts2_Option_defaultClause = (Core.FieldName "defaultClause")

_ColumnDefinition_Alts2_Option_identityColumnSpecification = (Core.FieldName "identityColumnSpecification")

_ColumnDefinition_Alts2_Option_generationClause = (Core.FieldName "generationClause")

data ColumnOptions = 
  
  deriving (Eq, Ord, Read, Show)

_ColumnOptions = (Core.Name "hydra/ext/sql/ansi.ColumnOptions")

data DataType = 
  DataTypePredefinedType PredefinedType |
  DataTypeRowType RowType |
  DataTypePathResolvedUserDefinedTypeName PathResolvedUserDefinedTypeName |
  DataTypeReferenceType ReferenceType |
  DataTypeCollectionType CollectionType
  deriving (Eq, Ord, Read, Show)

_DataType = (Core.Name "hydra/ext/sql/ansi.DataType")

_DataType_predefinedType = (Core.FieldName "predefinedType")

_DataType_rowType = (Core.FieldName "rowType")

_DataType_pathResolvedUserDefinedTypeName = (Core.FieldName "pathResolvedUserDefinedTypeName")

_DataType_referenceType = (Core.FieldName "referenceType")

_DataType_collectionType = (Core.FieldName "collectionType")

data DatetimeType = 
  
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra/ext/sql/ansi.DatetimeType")

data DefaultClause = 
  
  deriving (Eq, Ord, Read, Show)

_DefaultClause = (Core.Name "hydra/ext/sql/ansi.DefaultClause")

data ExactNumericType = 
  ExactNumericTypeSequence ExactNumericType_Sequence |
  ExactNumericTypeSequence2 ExactNumericType_Sequence2 |
  ExactNumericTypeSequence3 ExactNumericType_Sequence3 |
  ExactNumericTypeSMALLINT  |
  ExactNumericTypeINTEGER  |
  ExactNumericTypeINT  |
  ExactNumericTypeBIGINT 
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = (Core.Name "hydra/ext/sql/ansi.ExactNumericType")

_ExactNumericType_sequence = (Core.FieldName "sequence")

_ExactNumericType_sequence2 = (Core.FieldName "sequence2")

_ExactNumericType_sequence3 = (Core.FieldName "sequence3")

_ExactNumericType_sMALLINT = (Core.FieldName "sMALLINT")

_ExactNumericType_iNTEGER = (Core.FieldName "iNTEGER")

_ExactNumericType_iNT = (Core.FieldName "iNT")

_ExactNumericType_bIGINT = (Core.FieldName "bIGINT")

data ExactNumericType_Sequence = 
  ExactNumericType_Sequence {
    exactNumericType_SequenceSequence :: (Maybe ExactNumericType_Sequence_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence")

_ExactNumericType_Sequence_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence_Sequence_Option = 
  ExactNumericType_Sequence_Sequence_Option {
    exactNumericType_Sequence_Sequence_OptionPrecision :: Precision,
    exactNumericType_Sequence_Sequence_OptionSequence :: (Maybe ExactNumericType_Sequence_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence.Sequence.Option")

_ExactNumericType_Sequence_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Sequence_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence_Sequence_Option_Sequence_Option = 
  ExactNumericType_Sequence_Sequence_Option_Sequence_Option {
    exactNumericType_Sequence_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence.Sequence.Option.Sequence.Option")

_ExactNumericType_Sequence_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data ExactNumericType_Sequence2 = 
  ExactNumericType_Sequence2 {
    exactNumericType_Sequence2Sequence :: (Maybe ExactNumericType_Sequence2_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence2 = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence2")

_ExactNumericType_Sequence2_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence2_Sequence_Option = 
  ExactNumericType_Sequence2_Sequence_Option {
    exactNumericType_Sequence2_Sequence_OptionPrecision :: Precision,
    exactNumericType_Sequence2_Sequence_OptionSequence :: (Maybe ExactNumericType_Sequence2_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence2_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence2.Sequence.Option")

_ExactNumericType_Sequence2_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Sequence2_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence2_Sequence_Option_Sequence_Option = 
  ExactNumericType_Sequence2_Sequence_Option_Sequence_Option {
    exactNumericType_Sequence2_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence2_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence2.Sequence.Option.Sequence.Option")

_ExactNumericType_Sequence2_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data ExactNumericType_Sequence3 = 
  ExactNumericType_Sequence3 {
    exactNumericType_Sequence3Sequence :: (Maybe ExactNumericType_Sequence3_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence3 = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence3")

_ExactNumericType_Sequence3_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence3_Sequence_Option = 
  ExactNumericType_Sequence3_Sequence_Option {
    exactNumericType_Sequence3_Sequence_OptionPrecision :: Precision,
    exactNumericType_Sequence3_Sequence_OptionSequence :: (Maybe ExactNumericType_Sequence3_Sequence_Option_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence3_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence3.Sequence.Option")

_ExactNumericType_Sequence3_Sequence_Option_precision = (Core.FieldName "precision")

_ExactNumericType_Sequence3_Sequence_Option_sequence = (Core.FieldName "sequence")

data ExactNumericType_Sequence3_Sequence_Option_Sequence_Option = 
  ExactNumericType_Sequence3_Sequence_Option_Sequence_Option {
    exactNumericType_Sequence3_Sequence_Option_Sequence_OptionScale :: Scale}
  deriving (Eq, Ord, Read, Show)

_ExactNumericType_Sequence3_Sequence_Option_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.ExactNumericType.Sequence3.Sequence.Option.Sequence.Option")

_ExactNumericType_Sequence3_Sequence_Option_Sequence_Option_scale = (Core.FieldName "scale")

data GenerationClause = 
  
  deriving (Eq, Ord, Read, Show)

_GenerationClause = (Core.Name "hydra/ext/sql/ansi.GenerationClause")

data GlobalOrLocal = 
  GlobalOrLocalGLOBAL  |
  GlobalOrLocalLOCAL 
  deriving (Eq, Ord, Read, Show)

_GlobalOrLocal = (Core.Name "hydra/ext/sql/ansi.GlobalOrLocal")

_GlobalOrLocal_gLOBAL = (Core.FieldName "gLOBAL")

_GlobalOrLocal_lOCAL = (Core.FieldName "lOCAL")

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
  NumericTypeExactNumericType ExactNumericType |
  NumericTypeApproximateNumericType ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/ext/sql/ansi.NumericType")

_NumericType_exactNumericType = (Core.FieldName "exactNumericType")

_NumericType_approximateNumericType = (Core.FieldName "approximateNumericType")

newtype Precision = 
  Precision {
    unPrecision :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra/ext/sql/ansi.Precision")

data PredefinedType = 
  PredefinedTypeSequence PredefinedType_Sequence |
  PredefinedTypeSequence2 PredefinedType_Sequence2 |
  PredefinedTypeBinaryLargeObjectStringType BinaryLargeObjectStringType |
  PredefinedTypeNumericType NumericType |
  PredefinedTypeBooleanType BooleanType |
  PredefinedTypeDatetimeType DatetimeType |
  PredefinedTypeIntervalType IntervalType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = (Core.Name "hydra/ext/sql/ansi.PredefinedType")

_PredefinedType_sequence = (Core.FieldName "sequence")

_PredefinedType_sequence2 = (Core.FieldName "sequence2")

_PredefinedType_binaryLargeObjectStringType = (Core.FieldName "binaryLargeObjectStringType")

_PredefinedType_numericType = (Core.FieldName "numericType")

_PredefinedType_booleanType = (Core.FieldName "booleanType")

_PredefinedType_datetimeType = (Core.FieldName "datetimeType")

_PredefinedType_intervalType = (Core.FieldName "intervalType")

data PredefinedType_Sequence = 
  PredefinedType_Sequence {
    predefinedType_SequenceCharacterStringType :: CharacterStringType,
    predefinedType_SequenceSequence :: (Maybe PredefinedType_Sequence_Sequence_Option),
    predefinedType_SequenceCollateClause :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_Sequence = (Core.Name "hydra/ext/sql/ansi.PredefinedType.Sequence")

_PredefinedType_Sequence_characterStringType = (Core.FieldName "characterStringType")

_PredefinedType_Sequence_sequence = (Core.FieldName "sequence")

_PredefinedType_Sequence_collateClause = (Core.FieldName "collateClause")

data PredefinedType_Sequence_Sequence_Option = 
  PredefinedType_Sequence_Sequence_Option {
    predefinedType_Sequence_Sequence_OptionCharacterSetSpecification :: CharacterSetSpecification}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_Sequence_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.PredefinedType.Sequence.Sequence.Option")

_PredefinedType_Sequence_Sequence_Option_characterSetSpecification = (Core.FieldName "characterSetSpecification")

data PredefinedType_Sequence2 = 
  PredefinedType_Sequence2 {
    predefinedType_Sequence2NationalCharacterStringType :: NationalCharacterStringType,
    predefinedType_Sequence2CollateClause :: (Maybe CollateClause)}
  deriving (Eq, Ord, Read, Show)

_PredefinedType_Sequence2 = (Core.Name "hydra/ext/sql/ansi.PredefinedType.Sequence2")

_PredefinedType_Sequence2_nationalCharacterStringType = (Core.FieldName "nationalCharacterStringType")

_PredefinedType_Sequence2_collateClause = (Core.FieldName "collateClause")

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
  TableCommitActionPRESERVE  |
  TableCommitActionDELETE 
  deriving (Eq, Ord, Read, Show)

_TableCommitAction = (Core.Name "hydra/ext/sql/ansi.TableCommitAction")

_TableCommitAction_pRESERVE = (Core.FieldName "pRESERVE")

_TableCommitAction_dELETE = (Core.FieldName "dELETE")

data TableConstraintDefinition = 
  
  deriving (Eq, Ord, Read, Show)

_TableConstraintDefinition = (Core.Name "hydra/ext/sql/ansi.TableConstraintDefinition")

data TableContentsSource = 
  TableContentsSourceTableElementList TableElementList |
  TableContentsSourceSequence TableContentsSource_Sequence |
  TableContentsSourceAsSubqueryClause AsSubqueryClause
  deriving (Eq, Ord, Read, Show)

_TableContentsSource = (Core.Name "hydra/ext/sql/ansi.TableContentsSource")

_TableContentsSource_tableElementList = (Core.FieldName "tableElementList")

_TableContentsSource_sequence = (Core.FieldName "sequence")

_TableContentsSource_asSubqueryClause = (Core.FieldName "asSubqueryClause")

data TableContentsSource_Sequence = 
  TableContentsSource_Sequence {
    tableContentsSource_SequencePathResolvedUserDefinedTypeName :: PathResolvedUserDefinedTypeName,
    tableContentsSource_SequenceSubtableClause :: (Maybe SubtableClause),
    tableContentsSource_SequenceTableElementList :: (Maybe TableElementList)}
  deriving (Eq, Ord, Read, Show)

_TableContentsSource_Sequence = (Core.Name "hydra/ext/sql/ansi.TableContentsSource.Sequence")

_TableContentsSource_Sequence_pathResolvedUserDefinedTypeName = (Core.FieldName "pathResolvedUserDefinedTypeName")

_TableContentsSource_Sequence_subtableClause = (Core.FieldName "subtableClause")

_TableContentsSource_Sequence_tableElementList = (Core.FieldName "tableElementList")

data TableDefinition = 
  TableDefinition {
    tableDefinitionTableScope :: (Maybe TableScope),
    tableDefinitionTableName :: TableName,
    tableDefinitionTableContentsSource :: TableContentsSource,
    tableDefinitionSequence :: (Maybe TableDefinition_Sequence_Option)}
  deriving (Eq, Ord, Read, Show)

_TableDefinition = (Core.Name "hydra/ext/sql/ansi.TableDefinition")

_TableDefinition_tableScope = (Core.FieldName "tableScope")

_TableDefinition_tableName = (Core.FieldName "tableName")

_TableDefinition_tableContentsSource = (Core.FieldName "tableContentsSource")

_TableDefinition_sequence = (Core.FieldName "sequence")

data TableDefinition_Sequence_Option = 
  TableDefinition_Sequence_Option {
    tableDefinition_Sequence_OptionTableCommitAction :: TableCommitAction}
  deriving (Eq, Ord, Read, Show)

_TableDefinition_Sequence_Option = (Core.Name "hydra/ext/sql/ansi.TableDefinition.Sequence.Option")

_TableDefinition_Sequence_Option_tableCommitAction = (Core.FieldName "tableCommitAction")

data TableElement = 
  TableElementColumnDefinition ColumnDefinition |
  TableElementTableConstraintDefinition TableConstraintDefinition |
  TableElementLikeClause LikeClause |
  TableElementSelfReferencingColumnSpecification SelfReferencingColumnSpecification |
  TableElementColumnOptions ColumnOptions
  deriving (Eq, Ord, Read, Show)

_TableElement = (Core.Name "hydra/ext/sql/ansi.TableElement")

_TableElement_columnDefinition = (Core.FieldName "columnDefinition")

_TableElement_tableConstraintDefinition = (Core.FieldName "tableConstraintDefinition")

_TableElement_likeClause = (Core.FieldName "likeClause")

_TableElement_selfReferencingColumnSpecification = (Core.FieldName "selfReferencingColumnSpecification")

_TableElement_columnOptions = (Core.FieldName "columnOptions")

data TableElementList = 
  TableElementList {
    tableElementListTableElement :: TableElement,
    tableElementListListOfSequence :: [TableElementList_ListOfSequence_Elmt]}
  deriving (Eq, Ord, Read, Show)

_TableElementList = (Core.Name "hydra/ext/sql/ansi.TableElementList")

_TableElementList_tableElement = (Core.FieldName "tableElement")

_TableElementList_listOfSequence = (Core.FieldName "listOfSequence")

data TableElementList_ListOfSequence_Elmt = 
  TableElementList_ListOfSequence_Elmt {
    tableElementList_ListOfSequence_ElmtTableElement :: TableElement}
  deriving (Eq, Ord, Read, Show)

_TableElementList_ListOfSequence_Elmt = (Core.Name "hydra/ext/sql/ansi.TableElementList.ListOfSequence.Elmt")

_TableElementList_ListOfSequence_Elmt_tableElement = (Core.FieldName "tableElement")

data TableScope = 
  TableScope {
    tableScopeGlobalOrLocal :: GlobalOrLocal}
  deriving (Eq, Ord, Read, Show)

_TableScope = (Core.Name "hydra/ext/sql/ansi.TableScope")

_TableScope_globalOrLocal = (Core.FieldName "globalOrLocal")