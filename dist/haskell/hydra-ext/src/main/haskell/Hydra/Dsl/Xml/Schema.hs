-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.xml.schema

module Hydra.Dsl.Xml.Schema where

import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Xml.Schema as Schema
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Int as I

anySimpleType :: Phantoms.TTerm String -> Phantoms.TTerm Schema.AnySimpleType
anySimpleType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.AnySimpleType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

anyType :: Phantoms.TTerm String -> Phantoms.TTerm Schema.AnyType
anyType x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.AnyType"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

anyURI :: Phantoms.TTerm String -> Phantoms.TTerm Schema.AnyURI
anyURI x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.AnyURI"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

base64Binary :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Base64Binary
base64Binary x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Base64Binary"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

boolean :: Phantoms.TTerm Bool -> Phantoms.TTerm Schema.Boolean
boolean x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Boolean"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

byte :: Phantoms.TTerm I.Int8 -> Phantoms.TTerm Schema.Byte
byte x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Byte"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

constrainingFacet :: Phantoms.TTerm () -> Phantoms.TTerm Schema.ConstrainingFacet
constrainingFacet x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.ConstrainingFacet"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

datatypeAnySimpleType :: Phantoms.TTerm Schema.Datatype
datatypeAnySimpleType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anySimpleType"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeAnyType :: Phantoms.TTerm Schema.Datatype
datatypeAnyType =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anyType"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeAnyURI :: Phantoms.TTerm Schema.Datatype
datatypeAnyURI =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "anyURI"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeBase64Binary :: Phantoms.TTerm Schema.Datatype
datatypeBase64Binary =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "base64Binary"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeBoolean :: Phantoms.TTerm Schema.Datatype
datatypeBoolean =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "boolean"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeByte :: Phantoms.TTerm Schema.Datatype
datatypeByte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "byte"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeDate :: Phantoms.TTerm Schema.Datatype
datatypeDate =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "date"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeDateTime :: Phantoms.TTerm Schema.Datatype
datatypeDateTime =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "dateTime"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeDecimal :: Phantoms.TTerm Schema.Datatype
datatypeDecimal =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decimal"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeDouble :: Phantoms.TTerm Schema.Datatype
datatypeDouble =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "double"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeDuration :: Phantoms.TTerm Schema.Datatype
datatypeDuration =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "duration"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeENTITIES :: Phantoms.TTerm Schema.Datatype
datatypeENTITIES =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ENTITIES"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeENTITY :: Phantoms.TTerm Schema.Datatype
datatypeENTITY =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ENTITY"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeFloat :: Phantoms.TTerm Schema.Datatype
datatypeFloat =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "float"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeGDay :: Phantoms.TTerm Schema.Datatype
datatypeGDay =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gDay"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeGMonth :: Phantoms.TTerm Schema.Datatype
datatypeGMonth =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gMonth"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeGMonthDay :: Phantoms.TTerm Schema.Datatype
datatypeGMonthDay =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gMonthDay"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeGYear :: Phantoms.TTerm Schema.Datatype
datatypeGYear =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gYear"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeGYearMonth :: Phantoms.TTerm Schema.Datatype
datatypeGYearMonth =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "gYearMonth"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeHexBinary :: Phantoms.TTerm Schema.Datatype
datatypeHexBinary =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "hexBinary"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeID :: Phantoms.TTerm Schema.Datatype
datatypeID =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ID"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeIDREF :: Phantoms.TTerm Schema.Datatype
datatypeIDREF =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IDREF"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeIDREFS :: Phantoms.TTerm Schema.Datatype
datatypeIDREFS =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "IDREFS"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeInt :: Phantoms.TTerm Schema.Datatype
datatypeInt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "int"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeInteger :: Phantoms.TTerm Schema.Datatype
datatypeInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "integer"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeLanguage :: Phantoms.TTerm Schema.Datatype
datatypeLanguage =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "language"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeLong :: Phantoms.TTerm Schema.Datatype
datatypeLong =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "long"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNMTOKEN :: Phantoms.TTerm Schema.Datatype
datatypeNMTOKEN =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NMTOKEN"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNOTATION :: Phantoms.TTerm Schema.Datatype
datatypeNOTATION =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "NOTATION"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeName :: Phantoms.TTerm Schema.Datatype
datatypeName =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "name"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNegativeInteger :: Phantoms.TTerm Schema.Datatype
datatypeNegativeInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "negativeInteger"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNonNegativeInteger :: Phantoms.TTerm Schema.Datatype
datatypeNonNegativeInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonNegativeInteger"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNonPositiveInteger :: Phantoms.TTerm Schema.Datatype
datatypeNonPositiveInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nonPositiveInteger"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeNormalizedString :: Phantoms.TTerm Schema.Datatype
datatypeNormalizedString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "normalizedString"),
        Core.fieldTerm = Core.TermUnit}}))

datatypePositiveInteger :: Phantoms.TTerm Schema.Datatype
datatypePositiveInteger =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "positiveInteger"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeQName :: Phantoms.TTerm Schema.Datatype
datatypeQName =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "qName"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeShort :: Phantoms.TTerm Schema.Datatype
datatypeShort =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "short"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeString :: Phantoms.TTerm Schema.Datatype
datatypeString =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "string"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeTime :: Phantoms.TTerm Schema.Datatype
datatypeTime =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "time"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeToken :: Phantoms.TTerm Schema.Datatype
datatypeToken =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "token"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeUnsignedByte :: Phantoms.TTerm Schema.Datatype
datatypeUnsignedByte =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedByte"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeUnsignedInt :: Phantoms.TTerm Schema.Datatype
datatypeUnsignedInt =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedInt"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeUnsignedLong :: Phantoms.TTerm Schema.Datatype
datatypeUnsignedLong =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedLong"),
        Core.fieldTerm = Core.TermUnit}}))

datatypeUnsignedShort :: Phantoms.TTerm Schema.Datatype
datatypeUnsignedShort =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.xml.schema.Datatype"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "unsignedShort"),
        Core.fieldTerm = Core.TermUnit}}))

date :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Date
date x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Date"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

dateTime :: Phantoms.TTerm String -> Phantoms.TTerm Schema.DateTime
dateTime x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.DateTime"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

decimal :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Decimal
decimal x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Decimal"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

double :: Phantoms.TTerm Double -> Phantoms.TTerm Schema.Double_
double x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Double"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

duration :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Duration
duration x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Duration"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

eNTITIES :: Phantoms.TTerm String -> Phantoms.TTerm Schema.ENTITIES
eNTITIES x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.ENTITIES"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

eNTITY :: Phantoms.TTerm String -> Phantoms.TTerm Schema.ENTITY
eNTITY x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.ENTITY"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

float :: Phantoms.TTerm Float -> Phantoms.TTerm Schema.Float_
float x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Float"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

gDay :: Phantoms.TTerm String -> Phantoms.TTerm Schema.GDay
gDay x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.GDay"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

gMonth :: Phantoms.TTerm String -> Phantoms.TTerm Schema.GMonth
gMonth x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.GMonth"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

gMonthDay :: Phantoms.TTerm String -> Phantoms.TTerm Schema.GMonthDay
gMonthDay x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.GMonthDay"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

gYear :: Phantoms.TTerm String -> Phantoms.TTerm Schema.GYear
gYear x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.GYear"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

gYearMonth :: Phantoms.TTerm String -> Phantoms.TTerm Schema.GYearMonth
gYearMonth x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.GYearMonth"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

hexBinary :: Phantoms.TTerm String -> Phantoms.TTerm Schema.HexBinary
hexBinary x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.HexBinary"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

iD :: Phantoms.TTerm String -> Phantoms.TTerm Schema.ID
iD x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.ID"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

iDREF :: Phantoms.TTerm String -> Phantoms.TTerm Schema.IDREF
iDREF x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.IDREF"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

iDREFS :: Phantoms.TTerm String -> Phantoms.TTerm Schema.IDREFS
iDREFS x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.IDREFS"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

int :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.Int_
int x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Int"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

integer :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.Integer_
integer x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Integer"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

language :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Language
language x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Language"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

long :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Schema.Long
long x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Long"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nMTOKEN :: Phantoms.TTerm String -> Phantoms.TTerm Schema.NMTOKEN
nMTOKEN x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NMTOKEN"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nOTATION :: Phantoms.TTerm String -> Phantoms.TTerm Schema.NOTATION
nOTATION x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NOTATION"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

name :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Name
name x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Name"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

negativeInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.NegativeInteger
negativeInteger x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NegativeInteger"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nonNegativeInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.NonNegativeInteger
nonNegativeInteger x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NonNegativeInteger"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

nonPositiveInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.NonPositiveInteger
nonPositiveInteger x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NonPositiveInteger"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

normalizedString :: Phantoms.TTerm String -> Phantoms.TTerm Schema.NormalizedString
normalizedString x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.NormalizedString"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

positiveInteger :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.PositiveInteger
positiveInteger x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.PositiveInteger"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

qName :: Phantoms.TTerm String -> Phantoms.TTerm Schema.QName
qName x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.QName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

short :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Schema.Short
short x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Short"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

string :: Phantoms.TTerm String -> Phantoms.TTerm Schema.String_
string x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.String"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

time :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Time
time x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Time"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

token :: Phantoms.TTerm String -> Phantoms.TTerm Schema.Token
token x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.Token"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unAnySimpleType :: Phantoms.TTerm Schema.AnySimpleType -> Phantoms.TTerm String
unAnySimpleType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.AnySimpleType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnyType :: Phantoms.TTerm Schema.AnyType -> Phantoms.TTerm String
unAnyType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.AnyType")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unAnyURI :: Phantoms.TTerm Schema.AnyURI -> Phantoms.TTerm String
unAnyURI x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.AnyURI")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unBase64Binary :: Phantoms.TTerm Schema.Base64Binary -> Phantoms.TTerm String
unBase64Binary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Base64Binary")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unBoolean :: Phantoms.TTerm Schema.Boolean -> Phantoms.TTerm Bool
unBoolean x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Boolean")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unByte :: Phantoms.TTerm Schema.Byte -> Phantoms.TTerm I.Int8
unByte x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Byte")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unConstrainingFacet :: Phantoms.TTerm Schema.ConstrainingFacet -> Phantoms.TTerm ()
unConstrainingFacet x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.ConstrainingFacet")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDate :: Phantoms.TTerm Schema.Date -> Phantoms.TTerm String
unDate x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Date")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDateTime :: Phantoms.TTerm Schema.DateTime -> Phantoms.TTerm String
unDateTime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.DateTime")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDecimal :: Phantoms.TTerm Schema.Decimal -> Phantoms.TTerm String
unDecimal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Decimal")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDouble :: Phantoms.TTerm Schema.Double_ -> Phantoms.TTerm Double
unDouble x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Double")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unDuration :: Phantoms.TTerm Schema.Duration -> Phantoms.TTerm String
unDuration x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Duration")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unENTITIES :: Phantoms.TTerm Schema.ENTITIES -> Phantoms.TTerm String
unENTITIES x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.ENTITIES")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unENTITY :: Phantoms.TTerm Schema.ENTITY -> Phantoms.TTerm String
unENTITY x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.ENTITY")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unFloat :: Phantoms.TTerm Schema.Float_ -> Phantoms.TTerm Float
unFloat x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Float")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGDay :: Phantoms.TTerm Schema.GDay -> Phantoms.TTerm String
unGDay x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.GDay")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGMonth :: Phantoms.TTerm Schema.GMonth -> Phantoms.TTerm String
unGMonth x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.GMonth")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGMonthDay :: Phantoms.TTerm Schema.GMonthDay -> Phantoms.TTerm String
unGMonthDay x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.GMonthDay")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGYear :: Phantoms.TTerm Schema.GYear -> Phantoms.TTerm String
unGYear x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.GYear")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unGYearMonth :: Phantoms.TTerm Schema.GYearMonth -> Phantoms.TTerm String
unGYearMonth x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.GYearMonth")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unHexBinary :: Phantoms.TTerm Schema.HexBinary -> Phantoms.TTerm String
unHexBinary x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.HexBinary")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unID :: Phantoms.TTerm Schema.ID -> Phantoms.TTerm String
unID x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.ID")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIDREF :: Phantoms.TTerm Schema.IDREF -> Phantoms.TTerm String
unIDREF x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.IDREF")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unIDREFS :: Phantoms.TTerm Schema.IDREFS -> Phantoms.TTerm String
unIDREFS x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.IDREFS")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInt :: Phantoms.TTerm Schema.Int_ -> Phantoms.TTerm Int
unInt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Int")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unInteger :: Phantoms.TTerm Schema.Integer_ -> Phantoms.TTerm Integer
unInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Integer")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unLanguage :: Phantoms.TTerm Schema.Language -> Phantoms.TTerm String
unLanguage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Language")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unLong :: Phantoms.TTerm Schema.Long -> Phantoms.TTerm I.Int64
unLong x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Long")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNMTOKEN :: Phantoms.TTerm Schema.NMTOKEN -> Phantoms.TTerm String
unNMTOKEN x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NMTOKEN")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNOTATION :: Phantoms.TTerm Schema.NOTATION -> Phantoms.TTerm String
unNOTATION x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NOTATION")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unName :: Phantoms.TTerm Schema.Name -> Phantoms.TTerm String
unName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Name")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNegativeInteger :: Phantoms.TTerm Schema.NegativeInteger -> Phantoms.TTerm Integer
unNegativeInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NegativeInteger")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNonNegativeInteger :: Phantoms.TTerm Schema.NonNegativeInteger -> Phantoms.TTerm Integer
unNonNegativeInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NonNegativeInteger")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNonPositiveInteger :: Phantoms.TTerm Schema.NonPositiveInteger -> Phantoms.TTerm Integer
unNonPositiveInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NonPositiveInteger")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNormalizedString :: Phantoms.TTerm Schema.NormalizedString -> Phantoms.TTerm String
unNormalizedString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.NormalizedString")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unPositiveInteger :: Phantoms.TTerm Schema.PositiveInteger -> Phantoms.TTerm Integer
unPositiveInteger x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.PositiveInteger")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unQName :: Phantoms.TTerm Schema.QName -> Phantoms.TTerm String
unQName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.QName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unShort :: Phantoms.TTerm Schema.Short -> Phantoms.TTerm I.Int16
unShort x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Short")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unString :: Phantoms.TTerm Schema.String_ -> Phantoms.TTerm String
unString x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.String")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unTime :: Phantoms.TTerm Schema.Time -> Phantoms.TTerm String
unTime x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Time")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unToken :: Phantoms.TTerm Schema.Token -> Phantoms.TTerm String
unToken x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.Token")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUnsignedByte :: Phantoms.TTerm Schema.UnsignedByte -> Phantoms.TTerm I.Int16
unUnsignedByte x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.UnsignedByte")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUnsignedInt :: Phantoms.TTerm Schema.UnsignedInt -> Phantoms.TTerm I.Int64
unUnsignedInt x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.UnsignedInt")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUnsignedLong :: Phantoms.TTerm Schema.UnsignedLong -> Phantoms.TTerm Integer
unUnsignedLong x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.UnsignedLong")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unUnsignedShort :: Phantoms.TTerm Schema.UnsignedShort -> Phantoms.TTerm Int
unUnsignedShort x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.xml.schema.UnsignedShort")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unsignedByte :: Phantoms.TTerm I.Int16 -> Phantoms.TTerm Schema.UnsignedByte
unsignedByte x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.UnsignedByte"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unsignedInt :: Phantoms.TTerm I.Int64 -> Phantoms.TTerm Schema.UnsignedInt
unsignedInt x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.UnsignedInt"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unsignedLong :: Phantoms.TTerm Integer -> Phantoms.TTerm Schema.UnsignedLong
unsignedLong x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.UnsignedLong"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

unsignedShort :: Phantoms.TTerm Int -> Phantoms.TTerm Schema.UnsignedShort
unsignedShort x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.xml.schema.UnsignedShort"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
