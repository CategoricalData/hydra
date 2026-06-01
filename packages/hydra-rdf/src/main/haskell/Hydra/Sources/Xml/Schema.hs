module Hydra.Sources.Xml.Schema where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.xml.schema"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.\n" ++
      "See: https://www.w3.org/TR/xmlschema-2\n" ++
      "Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply\n" ++
      "      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly\n" ++
      "      corresponding Hydra literal type."))}
  where
    definitions = datatypes ++ others
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

anySimpleType :: TypeDefinition
anySimpleType = define "AnySimpleType" $ T.wrap T.string

anyType :: TypeDefinition
anyType = define "AnyType" $ T.wrap T.string

anyURI :: TypeDefinition
anyURI = define "AnyURI" $ T.wrap T.string

base64Binary :: TypeDefinition
base64Binary = define "Base64Binary" $ T.wrap T.string

boolean_ :: TypeDefinition
boolean_ = define "Boolean" $ T.wrap T.boolean

byte_ :: TypeDefinition
byte_ = define "Byte" $ T.wrap T.int8

constrainingFacet :: TypeDefinition
constrainingFacet = define "ConstrainingFacet" $
  see "https://www.w3.org/TR/xmlschema-2/#non-fundamental" $
  T.wrap T.unit -- TODO: concrete facets

datatype_ :: TypeDefinition
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

date :: TypeDefinition
date = define "Date" $ T.wrap T.string

dateTime :: TypeDefinition
dateTime = define "DateTime" $ T.wrap T.string

decimal :: TypeDefinition
decimal = define "Decimal" $ T.wrap T.string

double_ :: TypeDefinition
double_ = define "Double" $ T.wrap T.float64

duration :: TypeDefinition
duration = define "Duration" $ T.wrap T.string

entities :: TypeDefinition
entities = define "ENTITIES" $ T.wrap T.string

entity :: TypeDefinition
entity = define "ENTITY" $ T.wrap T.string

float_ :: TypeDefinition
float_ = define "Float" $ T.wrap T.float32

gDay :: TypeDefinition
gDay = define "GDay" $ T.wrap T.string

gMonth :: TypeDefinition
gMonth = define "GMonth" $ T.wrap T.string

gMonthDay :: TypeDefinition
gMonthDay = define "GMonthDay" $ T.wrap T.string

gYear :: TypeDefinition
gYear = define "GYear" $ T.wrap T.string

gYearMonth :: TypeDefinition
gYearMonth = define "GYearMonth" $ T.wrap T.string

hexBinary :: TypeDefinition
hexBinary = define "HexBinary" $ T.wrap T.string

id_ :: TypeDefinition
id_ = define "ID" $ T.wrap T.string

idref :: TypeDefinition
idref = define "IDREF" $ T.wrap T.string

idrefs :: TypeDefinition
idrefs = define "IDREFS" $ T.wrap T.string

int_ :: TypeDefinition
int_ = define "Int" $ T.wrap T.int32

integer_ :: TypeDefinition
integer_ = define "Integer" $ T.wrap T.bigint

language_ :: TypeDefinition
language_ = define "Language" $ T.wrap T.string

long_ :: TypeDefinition
long_ = define "Long" $ T.wrap T.int64

name_ :: TypeDefinition
name_ = define "Name" $ T.wrap T.string

negativeInteger :: TypeDefinition
negativeInteger = define "NegativeInteger" $ T.wrap T.bigint

nmtoken :: TypeDefinition
nmtoken = define "NMTOKEN" $ T.wrap T.string

nonNegativeInteger :: TypeDefinition
nonNegativeInteger = define "NonNegativeInteger" $ T.wrap T.bigint

nonPositiveInteger :: TypeDefinition
nonPositiveInteger = define "NonPositiveInteger" $ T.wrap T.bigint

normalizedString :: TypeDefinition
normalizedString = define "NormalizedString" $ T.wrap T.string

notation :: TypeDefinition
notation = define "NOTATION" $ T.wrap T.string

positiveInteger :: TypeDefinition
positiveInteger = define "PositiveInteger" $ T.wrap T.bigint

qName :: TypeDefinition
qName = define "QName" $ T.wrap T.string

short_ :: TypeDefinition
short_ = define "Short" $ T.wrap T.int16

string_ :: TypeDefinition
string_ = define "String" $ T.wrap T.string

time :: TypeDefinition
time = define "Time" $ T.wrap T.string

token :: TypeDefinition
token = define "Token" $ T.wrap T.string

unsignedByte :: TypeDefinition
unsignedByte = define "UnsignedByte" $ T.wrap T.uint8

unsignedInt :: TypeDefinition
unsignedInt = define "UnsignedInt" $ T.wrap T.uint32

unsignedLong :: TypeDefinition
unsignedLong = define "UnsignedLong" $ T.wrap T.uint64

unsignedShort :: TypeDefinition
unsignedShort = define "UnsignedShort" $ T.wrap T.uint16
