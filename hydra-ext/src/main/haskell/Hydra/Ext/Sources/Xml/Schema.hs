module Hydra.Ext.Sources.Xml.Schema where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.org.w3.xml.schema"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.\n" ++
      "See: https://www.w3.org/TR/xmlschema-2\n" ++
      "Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply\n" ++
      "      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly\n" ++
      "      corresponding Hydra literal type.")
  where
    elements = datatypes ++ others
    datatypes = [
      anySimpleType,
      anyType,
      anyURI,
      base64Binary,
      boolean_,
      byte_,
      date,
      dateTime,
      decimal,
      double_,
      duration,
      entities,
      entity,
      float_,
      gDay,
      gMonth,
      gMonthDay,
      gYear,
      gYearMonth,
      hexBinary,
      id_,
      idref,
      idrefs,
      int_,
      integer_,
      language_,
      long_,
      nmtoken,
      notation,
      name_,
      negativeInteger,
      nonNegativeInteger,
      nonPositiveInteger,
      normalizedString,
      positiveInteger,
      qName,
      short_,
      string_,
      time,
      token,
      unsignedByte,
      unsignedInt,
      unsignedLong,
      unsignedShort]
    others = [
      constrainingFacet,
      datatype_]

anySimpleType :: Binding
anySimpleType = define "AnySimpleType" $ T.wrap T.string

anyType :: Binding
anyType = define "AnyType" $ T.wrap T.string

anyURI :: Binding
anyURI = define "AnyURI" $ T.wrap T.string

base64Binary :: Binding
base64Binary = define "Base64Binary" $ T.wrap T.string

boolean_ :: Binding
boolean_ = define "Boolean" $ T.wrap T.boolean

byte_ :: Binding
byte_ = define "Byte" $ T.wrap T.int8

date :: Binding
date = define "Date" $ T.wrap T.string

dateTime :: Binding
dateTime = define "DateTime" $ T.wrap T.string

decimal :: Binding
decimal = define "Decimal" $ T.wrap T.string

double_ :: Binding
double_ = define "Double" $ T.wrap T.float64

duration :: Binding
duration = define "Duration" $ T.wrap T.string

entities :: Binding
entities = define "ENTITIES" $ T.wrap T.string

entity :: Binding
entity = define "ENTITY" $ T.wrap T.string

float_ :: Binding
float_ = define "Float" $ T.wrap T.float32

gDay :: Binding
gDay = define "GDay" $ T.wrap T.string

gMonth :: Binding
gMonth = define "GMonth" $ T.wrap T.string

gMonthDay :: Binding
gMonthDay = define "GMonthDay" $ T.wrap T.string

gYear :: Binding
gYear = define "GYear" $ T.wrap T.string

gYearMonth :: Binding
gYearMonth = define "GYearMonth" $ T.wrap T.string

hexBinary :: Binding
hexBinary = define "HexBinary" $ T.wrap T.string

id_ :: Binding
id_ = define "ID" $ T.wrap T.string

idref :: Binding
idref = define "IDREF" $ T.wrap T.string

idrefs :: Binding
idrefs = define "IDREFS" $ T.wrap T.string

int_ :: Binding
int_ = define "Int" $ T.wrap T.int32

integer_ :: Binding
integer_ = define "Integer" $ T.wrap T.bigint

language_ :: Binding
language_ = define "Language" $ T.wrap T.string

long_ :: Binding
long_ = define "Long" $ T.wrap T.int64

nmtoken :: Binding
nmtoken = define "NMTOKEN" $ T.wrap T.string

notation :: Binding
notation = define "NOTATION" $ T.wrap T.string

name_ :: Binding
name_ = define "Name" $ T.wrap T.string

negativeInteger :: Binding
negativeInteger = define "NegativeInteger" $ T.wrap T.bigint

nonNegativeInteger :: Binding
nonNegativeInteger = define "NonNegativeInteger" $ T.wrap T.bigint

nonPositiveInteger :: Binding
nonPositiveInteger = define "NonPositiveInteger" $ T.wrap T.bigint

normalizedString :: Binding
normalizedString = define "NormalizedString" $ T.wrap T.string

positiveInteger :: Binding
positiveInteger = define "PositiveInteger" $ T.wrap T.bigint

qName :: Binding
qName = define "QName" $ T.wrap T.string

short_ :: Binding
short_ = define "Short" $ T.wrap T.int16

string_ :: Binding
string_ = define "String" $ T.wrap T.string

time :: Binding
time = define "Time" $ T.wrap T.string

token :: Binding
token = define "Token" $ T.wrap T.string

unsignedByte :: Binding
unsignedByte = define "UnsignedByte" $ T.wrap T.uint8

unsignedInt :: Binding
unsignedInt = define "UnsignedInt" $ T.wrap T.uint32

unsignedLong :: Binding
unsignedLong = define "UnsignedLong" $ T.wrap T.uint64

unsignedShort :: Binding
unsignedShort = define "UnsignedShort" $ T.wrap T.uint16

constrainingFacet :: Binding
constrainingFacet = define "ConstrainingFacet" $
  see "https://www.w3.org/TR/xmlschema-2/#non-fundamental" $
  T.wrap T.unit -- TODO: concrete facets

datatype_ :: Binding
datatype_ = define "Datatype" $ T.enum [
  "anySimpleType",
  "anyType",
  "anyURI",
  "base64Binary",
  "boolean",
  "byte",
  "date",
  "dateTime",
  "decimal",
  "double",
  "duration",
  "ENTITIES",
  "ENTITY",
  "float",
  "gDay",
  "gMonth",
  "gMonthDay",
  "gYear",
  "gYearMonth",
  "hexBinary",
  "ID",
  "IDREF",
  "IDREFS",
  "int",
  "integer",
  "language",
  "long",
  "NMTOKEN",
  "NOTATION",
  "name",
  "negativeInteger",
  "nonNegativeInteger",
  "nonPositiveInteger",
  "normalizedString",
  "positiveInteger",
  "qName",
  "short",
  "string",
  "time",
  "token",
  "unsignedByte",
  "unsignedInt",
  "unsignedLong",
  "unsignedShort"]
