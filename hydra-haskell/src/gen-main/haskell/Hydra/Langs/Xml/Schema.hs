-- | A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.
-- | See: https://www.w3.org/TR/xmlschema-2
-- | Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply
-- |       the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly
-- |       corresponding Hydra literal type.

module Hydra.Langs.Xml.Schema where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype AnySimpleType = 
  AnySimpleType {
    unAnySimpleType :: String}
  deriving (Eq, Ord, Read, Show)

_AnySimpleType = (Core.Name "hydra/langs/xml/schema.AnySimpleType")

_AnySimpleType_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype AnyType = 
  AnyType {
    unAnyType :: String}
  deriving (Eq, Ord, Read, Show)

_AnyType = (Core.Name "hydra/langs/xml/schema.AnyType")

_AnyType_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype AnyURI = 
  AnyURI {
    unAnyURI :: String}
  deriving (Eq, Ord, Read, Show)

_AnyURI = (Core.Name "hydra/langs/xml/schema.AnyURI")

_AnyURI_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Base64Binary = 
  Base64Binary {
    unBase64Binary :: String}
  deriving (Eq, Ord, Read, Show)

_Base64Binary = (Core.Name "hydra/langs/xml/schema.Base64Binary")

_Base64Binary_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Boolean = 
  Boolean {
    unBoolean :: Bool}
  deriving (Eq, Ord, Read, Show)

_Boolean = (Core.Name "hydra/langs/xml/schema.Boolean")

_Boolean_type_ = (Core.TypeLiteral Core.LiteralTypeBoolean)

newtype Byte = 
  Byte {
    unByte :: Int8}
  deriving (Eq, Ord, Read, Show)

_Byte = (Core.Name "hydra/langs/xml/schema.Byte")

_Byte_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt8))

newtype Date = 
  Date {
    unDate :: String}
  deriving (Eq, Ord, Read, Show)

_Date = (Core.Name "hydra/langs/xml/schema.Date")

_Date_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype DateTime = 
  DateTime {
    unDateTime :: String}
  deriving (Eq, Ord, Read, Show)

_DateTime = (Core.Name "hydra/langs/xml/schema.DateTime")

_DateTime_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Decimal = 
  Decimal {
    unDecimal :: String}
  deriving (Eq, Ord, Read, Show)

_Decimal = (Core.Name "hydra/langs/xml/schema.Decimal")

_Decimal_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Double_ = 
  Double_ {
    unDouble :: Double}
  deriving (Eq, Ord, Read, Show)

_Double = (Core.Name "hydra/langs/xml/schema.Double")

_Double_type_ = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))

newtype Duration = 
  Duration {
    unDuration :: String}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra/langs/xml/schema.Duration")

_Duration_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ENTITIES = 
  ENTITIES {
    unENTITIES :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITIES = (Core.Name "hydra/langs/xml/schema.ENTITIES")

_ENTITIES_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ENTITY = 
  ENTITY {
    unENTITY :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITY = (Core.Name "hydra/langs/xml/schema.ENTITY")

_ENTITY_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Float_ = 
  Float_ {
    unFloat :: Float}
  deriving (Eq, Ord, Read, Show)

_Float = (Core.Name "hydra/langs/xml/schema.Float")

_Float_type_ = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat32))

newtype GDay = 
  GDay {
    unGDay :: String}
  deriving (Eq, Ord, Read, Show)

_GDay = (Core.Name "hydra/langs/xml/schema.GDay")

_GDay_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype GMonth = 
  GMonth {
    unGMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GMonth = (Core.Name "hydra/langs/xml/schema.GMonth")

_GMonth_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype GMonthDay = 
  GMonthDay {
    unGMonthDay :: String}
  deriving (Eq, Ord, Read, Show)

_GMonthDay = (Core.Name "hydra/langs/xml/schema.GMonthDay")

_GMonthDay_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype GYear = 
  GYear {
    unGYear :: String}
  deriving (Eq, Ord, Read, Show)

_GYear = (Core.Name "hydra/langs/xml/schema.GYear")

_GYear_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype GYearMonth = 
  GYearMonth {
    unGYearMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GYearMonth = (Core.Name "hydra/langs/xml/schema.GYearMonth")

_GYearMonth_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype HexBinary = 
  HexBinary {
    unHexBinary :: String}
  deriving (Eq, Ord, Read, Show)

_HexBinary = (Core.Name "hydra/langs/xml/schema.HexBinary")

_HexBinary_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ID = 
  ID {
    unID :: String}
  deriving (Eq, Ord, Read, Show)

_ID = (Core.Name "hydra/langs/xml/schema.ID")

_ID_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype IDREF = 
  IDREF {
    unIDREF :: String}
  deriving (Eq, Ord, Read, Show)

_IDREF = (Core.Name "hydra/langs/xml/schema.IDREF")

_IDREF_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype IDREFS = 
  IDREFS {
    unIDREFS :: String}
  deriving (Eq, Ord, Read, Show)

_IDREFS = (Core.Name "hydra/langs/xml/schema.IDREFS")

_IDREFS_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Int_ = 
  Int_ {
    unInt :: Int}
  deriving (Eq, Ord, Read, Show)

_Int = (Core.Name "hydra/langs/xml/schema.Int")

_Int_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))

newtype Integer_ = 
  Integer_ {
    unInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_Integer = (Core.Name "hydra/langs/xml/schema.Integer")

_Integer_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype Language = 
  Language {
    unLanguage :: String}
  deriving (Eq, Ord, Read, Show)

_Language = (Core.Name "hydra/langs/xml/schema.Language")

_Language_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Long = 
  Long {
    unLong :: Int64}
  deriving (Eq, Ord, Read, Show)

_Long = (Core.Name "hydra/langs/xml/schema.Long")

_Long_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))

newtype NMTOKEN = 
  NMTOKEN {
    unNMTOKEN :: String}
  deriving (Eq, Ord, Read, Show)

_NMTOKEN = (Core.Name "hydra/langs/xml/schema.NMTOKEN")

_NMTOKEN_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype NOTATION = 
  NOTATION {
    unNOTATION :: String}
  deriving (Eq, Ord, Read, Show)

_NOTATION = (Core.Name "hydra/langs/xml/schema.NOTATION")

_NOTATION_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/langs/xml/schema.Name")

_Name_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype NegativeInteger = 
  NegativeInteger {
    unNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NegativeInteger = (Core.Name "hydra/langs/xml/schema.NegativeInteger")

_NegativeInteger_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype NonNegativeInteger = 
  NonNegativeInteger {
    unNonNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonNegativeInteger = (Core.Name "hydra/langs/xml/schema.NonNegativeInteger")

_NonNegativeInteger_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype NonPositiveInteger = 
  NonPositiveInteger {
    unNonPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonPositiveInteger = (Core.Name "hydra/langs/xml/schema.NonPositiveInteger")

_NonPositiveInteger_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype NormalizedString = 
  NormalizedString {
    unNormalizedString :: String}
  deriving (Eq, Ord, Read, Show)

_NormalizedString = (Core.Name "hydra/langs/xml/schema.NormalizedString")

_NormalizedString_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype PositiveInteger = 
  PositiveInteger {
    unPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_PositiveInteger = (Core.Name "hydra/langs/xml/schema.PositiveInteger")

_PositiveInteger_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))

newtype QName = 
  QName {
    unQName :: String}
  deriving (Eq, Ord, Read, Show)

_QName = (Core.Name "hydra/langs/xml/schema.QName")

_QName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Short = 
  Short {
    unShort :: Int16}
  deriving (Eq, Ord, Read, Show)

_Short = (Core.Name "hydra/langs/xml/schema.Short")

_Short_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt16))

newtype String_ = 
  String_ {
    unString :: String}
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra/langs/xml/schema.String")

_String_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Time = 
  Time {
    unTime :: String}
  deriving (Eq, Ord, Read, Show)

_Time = (Core.Name "hydra/langs/xml/schema.Time")

_Time_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Token = 
  Token {
    unToken :: String}
  deriving (Eq, Ord, Read, Show)

_Token = (Core.Name "hydra/langs/xml/schema.Token")

_Token_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype UnsignedByte = 
  UnsignedByte {
    unUnsignedByte :: Int16}
  deriving (Eq, Ord, Read, Show)

_UnsignedByte = (Core.Name "hydra/langs/xml/schema.UnsignedByte")

_UnsignedByte_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint8))

newtype UnsignedInt = 
  UnsignedInt {
    unUnsignedInt :: Int64}
  deriving (Eq, Ord, Read, Show)

_UnsignedInt = (Core.Name "hydra/langs/xml/schema.UnsignedInt")

_UnsignedInt_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint32))

newtype UnsignedLong = 
  UnsignedLong {
    unUnsignedLong :: Integer}
  deriving (Eq, Ord, Read, Show)

_UnsignedLong = (Core.Name "hydra/langs/xml/schema.UnsignedLong")

_UnsignedLong_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint64))

newtype UnsignedShort = 
  UnsignedShort {
    unUnsignedShort :: Int}
  deriving (Eq, Ord, Read, Show)

_UnsignedShort = (Core.Name "hydra/langs/xml/schema.UnsignedShort")

_UnsignedShort_type_ = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeUint16))

-- | See https://www.w3.org/TR/xmlschema-2/#non-fundamental
data ConstrainingFacet = 
  ConstrainingFacet {}
  deriving (Eq, Ord, Read, Show)

_ConstrainingFacet = (Core.Name "hydra/langs/xml/schema.ConstrainingFacet")

_ConstrainingFacet_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data Datatype = 
  DatatypeAnySimpleType  |
  DatatypeAnyType  |
  DatatypeAnyURI  |
  DatatypeBase64Binary  |
  DatatypeBoolean  |
  DatatypeByte  |
  DatatypeDate  |
  DatatypeDateTime  |
  DatatypeDecimal  |
  DatatypeDouble  |
  DatatypeDuration  |
  DatatypeENTITIES  |
  DatatypeENTITY  |
  DatatypeFloat  |
  DatatypeGDay  |
  DatatypeGMonth  |
  DatatypeGMonthDay  |
  DatatypeGYear  |
  DatatypeGYearMonth  |
  DatatypeHexBinary  |
  DatatypeID  |
  DatatypeIDREF  |
  DatatypeIDREFS  |
  DatatypeInt  |
  DatatypeInteger  |
  DatatypeLanguage  |
  DatatypeLong  |
  DatatypeNMTOKEN  |
  DatatypeNOTATION  |
  DatatypeName  |
  DatatypeNegativeInteger  |
  DatatypeNonNegativeInteger  |
  DatatypeNonPositiveInteger  |
  DatatypeNormalizedString  |
  DatatypePositiveInteger  |
  DatatypeQName  |
  DatatypeShort  |
  DatatypeString  |
  DatatypeTime  |
  DatatypeToken  |
  DatatypeUnsignedByte  |
  DatatypeUnsignedInt  |
  DatatypeUnsignedLong  |
  DatatypeUnsignedShort 
  deriving (Eq, Ord, Read, Show)

_Datatype = (Core.Name "hydra/langs/xml/schema.Datatype")

_Datatype_anySimpleType = (Core.Name "anySimpleType")

_Datatype_anyType = (Core.Name "anyType")

_Datatype_anyURI = (Core.Name "anyURI")

_Datatype_base64Binary = (Core.Name "base64Binary")

_Datatype_boolean = (Core.Name "boolean")

_Datatype_byte = (Core.Name "byte")

_Datatype_date = (Core.Name "date")

_Datatype_dateTime = (Core.Name "dateTime")

_Datatype_decimal = (Core.Name "decimal")

_Datatype_double = (Core.Name "double")

_Datatype_duration = (Core.Name "duration")

_Datatype_ENTITIES = (Core.Name "ENTITIES")

_Datatype_ENTITY = (Core.Name "ENTITY")

_Datatype_float = (Core.Name "float")

_Datatype_gDay = (Core.Name "gDay")

_Datatype_gMonth = (Core.Name "gMonth")

_Datatype_gMonthDay = (Core.Name "gMonthDay")

_Datatype_gYear = (Core.Name "gYear")

_Datatype_gYearMonth = (Core.Name "gYearMonth")

_Datatype_hexBinary = (Core.Name "hexBinary")

_Datatype_ID = (Core.Name "ID")

_Datatype_IDREF = (Core.Name "IDREF")

_Datatype_IDREFS = (Core.Name "IDREFS")

_Datatype_int = (Core.Name "int")

_Datatype_integer = (Core.Name "integer")

_Datatype_language = (Core.Name "language")

_Datatype_long = (Core.Name "long")

_Datatype_NMTOKEN = (Core.Name "NMTOKEN")

_Datatype_NOTATION = (Core.Name "NOTATION")

_Datatype_name = (Core.Name "name")

_Datatype_negativeInteger = (Core.Name "negativeInteger")

_Datatype_nonNegativeInteger = (Core.Name "nonNegativeInteger")

_Datatype_nonPositiveInteger = (Core.Name "nonPositiveInteger")

_Datatype_normalizedString = (Core.Name "normalizedString")

_Datatype_positiveInteger = (Core.Name "positiveInteger")

_Datatype_qName = (Core.Name "qName")

_Datatype_short = (Core.Name "short")

_Datatype_string = (Core.Name "string")

_Datatype_time = (Core.Name "time")

_Datatype_token = (Core.Name "token")

_Datatype_unsignedByte = (Core.Name "unsignedByte")

_Datatype_unsignedInt = (Core.Name "unsignedInt")

_Datatype_unsignedLong = (Core.Name "unsignedLong")

_Datatype_unsignedShort = (Core.Name "unsignedShort")

_Datatype_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/xml/schema.Datatype"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anySimpleType"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anyType"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anyURI"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "base64Binary"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "byte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "date"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dateTime"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "decimal"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "duration"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ENTITIES"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ENTITY"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "float"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gDay"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gMonth"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gMonthDay"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gYear"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gYearMonth"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hexBinary"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ID"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "IDREF"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "IDREFS"),
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
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "language"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "NMTOKEN"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "NOTATION"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "negativeInteger"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nonNegativeInteger"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nonPositiveInteger"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "normalizedString"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "positiveInteger"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qName"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "short"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "time"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "token"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unsignedByte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unsignedInt"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unsignedLong"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unsignedShort"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))