module Hydra.Ext.Xml.Schema where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype AnySimpleType 
  = AnySimpleType {
    unAnySimpleType :: String}
  deriving (Eq, Ord, Read, Show)

_AnySimpleType = (Core.Name "hydra/ext/xml/schema.AnySimpleType")

newtype AnyType 
  = AnyType {
    unAnyType :: String}
  deriving (Eq, Ord, Read, Show)

_AnyType = (Core.Name "hydra/ext/xml/schema.AnyType")

newtype AnyURI 
  = AnyURI {
    unAnyURI :: String}
  deriving (Eq, Ord, Read, Show)

_AnyURI = (Core.Name "hydra/ext/xml/schema.AnyURI")

newtype Base64Binary 
  = Base64Binary {
    unBase64Binary :: String}
  deriving (Eq, Ord, Read, Show)

_Base64Binary = (Core.Name "hydra/ext/xml/schema.Base64Binary")

newtype Boolean 
  = Boolean {
    unBoolean :: Bool}
  deriving (Eq, Ord, Read, Show)

_Boolean = (Core.Name "hydra/ext/xml/schema.Boolean")

newtype Byte 
  = Byte {
    unByte :: Int}
  deriving (Eq, Ord, Read, Show)

_Byte = (Core.Name "hydra/ext/xml/schema.Byte")

newtype Date 
  = Date {
    unDate :: String}
  deriving (Eq, Ord, Read, Show)

_Date = (Core.Name "hydra/ext/xml/schema.Date")

newtype DateTime 
  = DateTime {
    unDateTime :: String}
  deriving (Eq, Ord, Read, Show)

_DateTime = (Core.Name "hydra/ext/xml/schema.DateTime")

newtype Decimal 
  = Decimal {
    unDecimal :: String}
  deriving (Eq, Ord, Read, Show)

_Decimal = (Core.Name "hydra/ext/xml/schema.Decimal")

newtype Double_ 
  = Double {
    unDouble :: Double}
  deriving (Eq, Ord, Read, Show)

_Double = (Core.Name "hydra/ext/xml/schema.Double")

newtype Duration 
  = Duration {
    unDuration :: String}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra/ext/xml/schema.Duration")

newtype ENTITIES 
  = ENTITIES {
    unENTITIES :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITIES = (Core.Name "hydra/ext/xml/schema.ENTITIES")

newtype ENTITY 
  = ENTITY {
    unENTITY :: String}
  deriving (Eq, Ord, Read, Show)

_ENTITY = (Core.Name "hydra/ext/xml/schema.ENTITY")

newtype Float_ 
  = Float {
    unFloat :: Float}
  deriving (Eq, Ord, Read, Show)

_Float = (Core.Name "hydra/ext/xml/schema.Float")

newtype GDay 
  = GDay {
    unGDay :: String}
  deriving (Eq, Ord, Read, Show)

_GDay = (Core.Name "hydra/ext/xml/schema.GDay")

newtype GMonth 
  = GMonth {
    unGMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GMonth = (Core.Name "hydra/ext/xml/schema.GMonth")

newtype GMonthDay 
  = GMonthDay {
    unGMonthDay :: String}
  deriving (Eq, Ord, Read, Show)

_GMonthDay = (Core.Name "hydra/ext/xml/schema.GMonthDay")

newtype GYear 
  = GYear {
    unGYear :: String}
  deriving (Eq, Ord, Read, Show)

_GYear = (Core.Name "hydra/ext/xml/schema.GYear")

newtype GYearMonth 
  = GYearMonth {
    unGYearMonth :: String}
  deriving (Eq, Ord, Read, Show)

_GYearMonth = (Core.Name "hydra/ext/xml/schema.GYearMonth")

newtype HexBinary 
  = HexBinary {
    unHexBinary :: String}
  deriving (Eq, Ord, Read, Show)

_HexBinary = (Core.Name "hydra/ext/xml/schema.HexBinary")

newtype ID 
  = ID {
    unID :: String}
  deriving (Eq, Ord, Read, Show)

_ID = (Core.Name "hydra/ext/xml/schema.ID")

newtype IDREF 
  = IDREF {
    unIDREF :: String}
  deriving (Eq, Ord, Read, Show)

_IDREF = (Core.Name "hydra/ext/xml/schema.IDREF")

newtype IDREFS 
  = IDREFS {
    unIDREFS :: String}
  deriving (Eq, Ord, Read, Show)

_IDREFS = (Core.Name "hydra/ext/xml/schema.IDREFS")

newtype Int_ 
  = Int {
    unInt :: Int}
  deriving (Eq, Ord, Read, Show)

_Int = (Core.Name "hydra/ext/xml/schema.Int")

newtype Integer_ 
  = Integer {
    unInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_Integer = (Core.Name "hydra/ext/xml/schema.Integer")

newtype Language 
  = Language {
    unLanguage :: String}
  deriving (Eq, Ord, Read, Show)

_Language = (Core.Name "hydra/ext/xml/schema.Language")

newtype Long 
  = Long {
    unLong :: Integer}
  deriving (Eq, Ord, Read, Show)

_Long = (Core.Name "hydra/ext/xml/schema.Long")

newtype NMTOKEN 
  = NMTOKEN {
    unNMTOKEN :: String}
  deriving (Eq, Ord, Read, Show)

_NMTOKEN = (Core.Name "hydra/ext/xml/schema.NMTOKEN")

newtype NOTATION 
  = NOTATION {
    unNOTATION :: String}
  deriving (Eq, Ord, Read, Show)

_NOTATION = (Core.Name "hydra/ext/xml/schema.NOTATION")

newtype Name 
  = Name {
    unName :: String}
  deriving (Eq, Ord, Read, Show)

_Name = (Core.Name "hydra/ext/xml/schema.Name")

newtype NegativeInteger 
  = NegativeInteger {
    unNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NegativeInteger = (Core.Name "hydra/ext/xml/schema.NegativeInteger")

newtype NonNegativeInteger 
  = NonNegativeInteger {
    unNonNegativeInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonNegativeInteger = (Core.Name "hydra/ext/xml/schema.NonNegativeInteger")

newtype NonPositiveInteger 
  = NonPositiveInteger {
    unNonPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_NonPositiveInteger = (Core.Name "hydra/ext/xml/schema.NonPositiveInteger")

newtype NormalizedString 
  = NormalizedString {
    unNormalizedString :: String}
  deriving (Eq, Ord, Read, Show)

_NormalizedString = (Core.Name "hydra/ext/xml/schema.NormalizedString")

newtype PositiveInteger 
  = PositiveInteger {
    unPositiveInteger :: Integer}
  deriving (Eq, Ord, Read, Show)

_PositiveInteger = (Core.Name "hydra/ext/xml/schema.PositiveInteger")

newtype QName 
  = QName {
    unQName :: String}
  deriving (Eq, Ord, Read, Show)

_QName = (Core.Name "hydra/ext/xml/schema.QName")

newtype Short 
  = Short {
    unShort :: Int}
  deriving (Eq, Ord, Read, Show)

_Short = (Core.Name "hydra/ext/xml/schema.Short")

newtype String_ 
  = String {
    unString :: String}
  deriving (Eq, Ord, Read, Show)

_String = (Core.Name "hydra/ext/xml/schema.String")

newtype Time 
  = Time {
    unTime :: String}
  deriving (Eq, Ord, Read, Show)

_Time = (Core.Name "hydra/ext/xml/schema.Time")

newtype Token 
  = Token {
    unToken :: String}
  deriving (Eq, Ord, Read, Show)

_Token = (Core.Name "hydra/ext/xml/schema.Token")

newtype UnsignedByte 
  = UnsignedByte {
    unUnsignedByte :: Int}
  deriving (Eq, Ord, Read, Show)

_UnsignedByte = (Core.Name "hydra/ext/xml/schema.UnsignedByte")

newtype UnsignedInt 
  = UnsignedInt {
    unUnsignedInt :: Integer}
  deriving (Eq, Ord, Read, Show)

_UnsignedInt = (Core.Name "hydra/ext/xml/schema.UnsignedInt")

newtype UnsignedLong 
  = UnsignedLong {
    unUnsignedLong :: Integer}
  deriving (Eq, Ord, Read, Show)

_UnsignedLong = (Core.Name "hydra/ext/xml/schema.UnsignedLong")

newtype UnsignedShort 
  = UnsignedShort {
    unUnsignedShort :: Int}
  deriving (Eq, Ord, Read, Show)

_UnsignedShort = (Core.Name "hydra/ext/xml/schema.UnsignedShort")