-- | A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.
-- | See: https://www.w3.org/TR/xmlschema-2
-- | Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply
-- |       the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly
-- |       corresponding Hydra literal type.

module Hydra.Ext.Xml.Schema where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype AnySimpleType = 
  AnySimpleType {
    unAnySimpleType :: String}
  deriving (Eq, Ord, Read, Show)

_AnySimpleType = (Core.Name "hydra/ext/xml/schema.AnySimpleType")

newtype AnyType = 
  AnyType {
    unAnyType :: String}
  deriving (Eq, Ord, Read, Show)

_AnyType = (Core.Name "hydra/ext/xml/schema.AnyType")

newtype AnyURI = 
  AnyURI {
    unAnyURI :: String}
  deriving (Eq, Ord, Read, Show)

_AnyURI = (Core.Name "hydra/ext/xml/schema.AnyURI")

newtype Base64Binary = 
  Base64Binary {
    unBase64Binary :: String}
  deriving (Eq, Ord, Read, Show)

_Base64Binary = (Core.Name "hydra/ext/xml/schema.Base64Binary")

newtype Boolean = 
  Boolean {
    unBoolean :: Bool}
  deriving (Eq, Ord, Read, Show)

_Boolean = (Core.Name "hydra/ext/xml/schema.Boolean")

newtype Byte = 
  Byte {
    unByte :: Int8}
  deriving (Eq, Ord, Read, Show)

_Byte = (Core.Name "hydra/ext/xml/schema.Byte")

newtype Date = 
  Date {
    unDate :: String}
  deriving (Eq, Ord, Read, Show)

_Date = (Core.Name "hydra/ext/xml/schema.Date")

newtype DateTime = 
  DateTime {
    unDateTime :: String}
  deriving (Eq, Ord, Read, Show)

_DateTime = (Core.Name "hydra/ext/xml/schema.DateTime")

newtype Decimal = 
  Decimal {
    unDecimal :: String}
  deriving (Eq, Ord, Read, Show)

_Decimal = (Core.Name "hydra/ext/xml/schema.Decimal")

newtype Double_ = 
  Double_ {
    unDouble :: Double}
  deriving (Eq, Ord, Read, Show)

_Double = (Core.Name "hydra/ext/xml/schema.Double")

newtype Duration = 
  Duration {
    unDuration :: String}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra/ext/xml/schema.Duration")

newtype ENTITIES = 
  ENTITIES {
    unENTITIES :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITIES = (Core.Name "hydra/ext/xml/schema.ENTITIES")

newtype ENTITY = 
  ENTITY {
    unENTITY :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITY = (Core.Name "hydra/ext/xml/schema.ENTITY")

newtype Float_ = 
  Float_ {
    unFloat :: Float}
  deriving (Eq, Ord, Read, Show)

_Float = (Core.Name "hydra/ext/xml/schema.Float")

newtype GDay = 
  GDay {
    unGDay :: String}
  deriving (Eq, Ord, Read, Show)

_GDay = (Core.Name "hydra/ext/xml/schema.GDay")

newtype GMonth = 
  GMonth {
    unGMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GMonth = (Core.Name "hydra/ext/xml/schema.GMonth")

newtype GMonthDay = 
  GMonthDay {
    unGMonthDay :: String}
  deriving (Eq, Ord, Read, Show)

_GMonthDay = (Core.Name "hydra/ext/xml/schema.GMonthDay")

newtype GYear = 
  GYear {
    unGYear :: String}
  deriving (Eq, Ord, Read, Show)

_GYear = (Core.Name "hydra/ext/xml/schema.GYear")

newtype GYearMonth = 
  GYearMonth {
    unGYearMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GYearMonth = (Core.Name "hydra/ext/xml/schema.GYearMonth")

newtype HexBinary = 
  HexBinary {
    unHexBinary :: String}
  deriving (Eq, Ord, Read, Show)

_HexBinary = (Core.Name "hydra/ext/xml/schema.HexBinary")

newtype ID = 
  ID {
    unID :: String}
  deriving (Eq, Ord, Read, Show)

_ID = (Core.Name "hydra/ext/xml/schema.ID")

newtype IDREF = 
  IDREF {
    unIDREF :: String}
  deriving (Eq, Ord, Read, Show)

_IDREF = (Core.Name "hydra/ext/xml/schema.IDREF")

newtype IDREFS = 
  IDREFS {
    unIDREFS :: String}
  deriving (Eq, Ord, Read, Show)

_IDREFS = (Core.Name "hydra/ext/xml/schema.IDREFS")

newtype Int_ = 
  Int_ {
    unInt :: Int}
  deriving (Eq, Ord, Read, Show)

_Int = (Core.Name "hydra/ext/xml/schema.Int")

newtype Integer_ = 
  Integer_ {
    unInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_Integer = (Core.Name "hydra/ext/xml/schema.Integer")

newtype Language = 
  Language {
    unLanguage :: String}
  deriving (Eq, Ord, Read, Show)

_Language = (Core.Name "hydra/ext/xml/schema.Language")

newtype Long = 
  Long {
    unLong :: Int64}
  deriving (Eq, Ord, Read, Show)

_Long = (Core.Name "hydra/ext/xml/schema.Long")

newtype NMTOKEN = 
  NMTOKEN {
    unNMTOKEN :: String}
  deriving (Eq, Ord, Read, Show)

_NMTOKEN = (Core.Name "hydra/ext/xml/schema.NMTOKEN")

newtype NOTATION = 
  NOTATION {
    unNOTATION :: String}
  deriving (Eq, Ord, Read, Show)

_NOTATION = (Core.Name "hydra/ext/xml/schema.NOTATION")

newtype Name = 
  Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/ext/xml/schema.Name")

newtype NegativeInteger = 
  NegativeInteger {
    unNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NegativeInteger = (Core.Name "hydra/ext/xml/schema.NegativeInteger")

newtype NonNegativeInteger = 
  NonNegativeInteger {
    unNonNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonNegativeInteger = (Core.Name "hydra/ext/xml/schema.NonNegativeInteger")

newtype NonPositiveInteger = 
  NonPositiveInteger {
    unNonPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonPositiveInteger = (Core.Name "hydra/ext/xml/schema.NonPositiveInteger")

newtype NormalizedString = 
  NormalizedString {
    unNormalizedString :: String}
  deriving (Eq, Ord, Read, Show)

_NormalizedString = (Core.Name "hydra/ext/xml/schema.NormalizedString")

newtype PositiveInteger = 
  PositiveInteger {
    unPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_PositiveInteger = (Core.Name "hydra/ext/xml/schema.PositiveInteger")

newtype QName = 
  QName {
    unQName :: String}
  deriving (Eq, Ord, Read, Show)

_QName = (Core.Name "hydra/ext/xml/schema.QName")

newtype Short = 
  Short {
    unShort :: Int16}
  deriving (Eq, Ord, Read, Show)

_Short = (Core.Name "hydra/ext/xml/schema.Short")

newtype String_ = 
  String_ {
    unString :: String}
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra/ext/xml/schema.String")

newtype Time = 
  Time {
    unTime :: String}
  deriving (Eq, Ord, Read, Show)

_Time = (Core.Name "hydra/ext/xml/schema.Time")

newtype Token = 
  Token {
    unToken :: String}
  deriving (Eq, Ord, Read, Show)

_Token = (Core.Name "hydra/ext/xml/schema.Token")

newtype UnsignedByte = 
  UnsignedByte {
    unUnsignedByte :: Int16}
  deriving (Eq, Ord, Read, Show)

_UnsignedByte = (Core.Name "hydra/ext/xml/schema.UnsignedByte")

newtype UnsignedInt = 
  UnsignedInt {
    unUnsignedInt :: Int64}
  deriving (Eq, Ord, Read, Show)

_UnsignedInt = (Core.Name "hydra/ext/xml/schema.UnsignedInt")

newtype UnsignedLong = 
  UnsignedLong {
    unUnsignedLong :: Integer}
  deriving (Eq, Ord, Read, Show)

_UnsignedLong = (Core.Name "hydra/ext/xml/schema.UnsignedLong")

newtype UnsignedShort = 
  UnsignedShort {
    unUnsignedShort :: Int}
  deriving (Eq, Ord, Read, Show)

_UnsignedShort = (Core.Name "hydra/ext/xml/schema.UnsignedShort")

-- | See https://www.w3.org/TR/xmlschema-2/#non-fundamental
data ConstrainingFacet = 
  ConstrainingFacet {}
  deriving (Eq, Ord, Read, Show)

_ConstrainingFacet = (Core.Name "hydra/ext/xml/schema.ConstrainingFacet")

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

_Datatype = (Core.Name "hydra/ext/xml/schema.Datatype")

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