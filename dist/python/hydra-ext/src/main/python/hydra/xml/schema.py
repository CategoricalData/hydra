# Note: this is an automatically generated file. Do not edit.

r"""A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.
See: https://www.w3.org/TR/xmlschema-2
Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply
      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly
      corresponding Hydra literal type."""

from __future__ import annotations
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Node
from typing import TypeAlias, cast
import hydra.core

class AnySimpleType(Node[str]):
    ...

AnySimpleType.TYPE_ = hydra.core.Name("hydra.xml.schema.AnySimpleType")

class AnyType(Node[str]):
    ...

AnyType.TYPE_ = hydra.core.Name("hydra.xml.schema.AnyType")

class AnyURI(Node[str]):
    ...

AnyURI.TYPE_ = hydra.core.Name("hydra.xml.schema.AnyURI")

class Base64Binary(Node[str]):
    ...

Base64Binary.TYPE_ = hydra.core.Name("hydra.xml.schema.Base64Binary")

class Boolean(Node[bool]):
    ...

Boolean.TYPE_ = hydra.core.Name("hydra.xml.schema.Boolean")

class Byte(Node[int]):
    ...

Byte.TYPE_ = hydra.core.Name("hydra.xml.schema.Byte")

class Date(Node[str]):
    ...

Date.TYPE_ = hydra.core.Name("hydra.xml.schema.Date")

class DateTime(Node[str]):
    ...

DateTime.TYPE_ = hydra.core.Name("hydra.xml.schema.DateTime")

class Decimal(Node[str]):
    ...

Decimal.TYPE_ = hydra.core.Name("hydra.xml.schema.Decimal")

class Double(Node[float]):
    ...

Double.TYPE_ = hydra.core.Name("hydra.xml.schema.Double")

class Duration(Node[str]):
    ...

Duration.TYPE_ = hydra.core.Name("hydra.xml.schema.Duration")

class ENTITIES(Node[str]):
    ...

ENTITIES.TYPE_ = hydra.core.Name("hydra.xml.schema.ENTITIES")

class ENTITY(Node[str]):
    ...

ENTITY.TYPE_ = hydra.core.Name("hydra.xml.schema.ENTITY")

class Float(Node[float]):
    ...

Float.TYPE_ = hydra.core.Name("hydra.xml.schema.Float")

class GDay(Node[str]):
    ...

GDay.TYPE_ = hydra.core.Name("hydra.xml.schema.GDay")

class GMonth(Node[str]):
    ...

GMonth.TYPE_ = hydra.core.Name("hydra.xml.schema.GMonth")

class GMonthDay(Node[str]):
    ...

GMonthDay.TYPE_ = hydra.core.Name("hydra.xml.schema.GMonthDay")

class GYear(Node[str]):
    ...

GYear.TYPE_ = hydra.core.Name("hydra.xml.schema.GYear")

class GYearMonth(Node[str]):
    ...

GYearMonth.TYPE_ = hydra.core.Name("hydra.xml.schema.GYearMonth")

class HexBinary(Node[str]):
    ...

HexBinary.TYPE_ = hydra.core.Name("hydra.xml.schema.HexBinary")

class ID(Node[str]):
    ...

ID.TYPE_ = hydra.core.Name("hydra.xml.schema.ID")

class IDREF(Node[str]):
    ...

IDREF.TYPE_ = hydra.core.Name("hydra.xml.schema.IDREF")

class IDREFS(Node[str]):
    ...

IDREFS.TYPE_ = hydra.core.Name("hydra.xml.schema.IDREFS")

class Int(Node[int]):
    ...

Int.TYPE_ = hydra.core.Name("hydra.xml.schema.Int")

class Integer(Node[int]):
    ...

Integer.TYPE_ = hydra.core.Name("hydra.xml.schema.Integer")

class Language(Node[str]):
    ...

Language.TYPE_ = hydra.core.Name("hydra.xml.schema.Language")

class Long(Node[int]):
    ...

Long.TYPE_ = hydra.core.Name("hydra.xml.schema.Long")

class NMTOKEN(Node[str]):
    ...

NMTOKEN.TYPE_ = hydra.core.Name("hydra.xml.schema.NMTOKEN")

class NOTATION(Node[str]):
    ...

NOTATION.TYPE_ = hydra.core.Name("hydra.xml.schema.NOTATION")

class Name(Node[str]):
    ...

Name.TYPE_ = hydra.core.Name("hydra.xml.schema.Name")

class NegativeInteger(Node[int]):
    ...

NegativeInteger.TYPE_ = hydra.core.Name("hydra.xml.schema.NegativeInteger")

class NonNegativeInteger(Node[int]):
    ...

NonNegativeInteger.TYPE_ = hydra.core.Name("hydra.xml.schema.NonNegativeInteger")

class NonPositiveInteger(Node[int]):
    ...

NonPositiveInteger.TYPE_ = hydra.core.Name("hydra.xml.schema.NonPositiveInteger")

class NormalizedString(Node[str]):
    ...

NormalizedString.TYPE_ = hydra.core.Name("hydra.xml.schema.NormalizedString")

class PositiveInteger(Node[int]):
    ...

PositiveInteger.TYPE_ = hydra.core.Name("hydra.xml.schema.PositiveInteger")

class QName(Node[str]):
    ...

QName.TYPE_ = hydra.core.Name("hydra.xml.schema.QName")

class Short(Node[int]):
    ...

Short.TYPE_ = hydra.core.Name("hydra.xml.schema.Short")

class String(Node[str]):
    ...

String.TYPE_ = hydra.core.Name("hydra.xml.schema.String")

class Time(Node[str]):
    ...

Time.TYPE_ = hydra.core.Name("hydra.xml.schema.Time")

class Token(Node[str]):
    ...

Token.TYPE_ = hydra.core.Name("hydra.xml.schema.Token")

class UnsignedByte(Node[int]):
    ...

UnsignedByte.TYPE_ = hydra.core.Name("hydra.xml.schema.UnsignedByte")

class UnsignedInt(Node[int]):
    ...

UnsignedInt.TYPE_ = hydra.core.Name("hydra.xml.schema.UnsignedInt")

class UnsignedLong(Node[int]):
    ...

UnsignedLong.TYPE_ = hydra.core.Name("hydra.xml.schema.UnsignedLong")

class UnsignedShort(Node[int]):
    ...

UnsignedShort.TYPE_ = hydra.core.Name("hydra.xml.schema.UnsignedShort")

class ConstrainingFacet(Node[None]):
    r"""See https://www.w3.org/TR/xmlschema-2/#non-fundamental."""

ConstrainingFacet.TYPE_ = hydra.core.Name("hydra.xml.schema.ConstrainingFacet")

class Datatype(Enum):
    ANY_SIMPLE_TYPE = hydra.core.Name("anySimpleType")

    ANY_TYPE = hydra.core.Name("anyType")

    ANY_U_R_I = hydra.core.Name("anyURI")

    BASE64_BINARY = hydra.core.Name("base64Binary")

    BOOLEAN = hydra.core.Name("boolean")

    BYTE = hydra.core.Name("byte")

    DATE = hydra.core.Name("date")

    DATE_TIME = hydra.core.Name("dateTime")

    DECIMAL = hydra.core.Name("decimal")

    DOUBLE = hydra.core.Name("double")

    DURATION = hydra.core.Name("duration")

    E_N_T_I_T_I_E_S = hydra.core.Name("ENTITIES")

    E_N_T_I_T_Y = hydra.core.Name("ENTITY")

    FLOAT = hydra.core.Name("float")

    G_DAY = hydra.core.Name("gDay")

    G_MONTH = hydra.core.Name("gMonth")

    G_MONTH_DAY = hydra.core.Name("gMonthDay")

    G_YEAR = hydra.core.Name("gYear")

    G_YEAR_MONTH = hydra.core.Name("gYearMonth")

    HEX_BINARY = hydra.core.Name("hexBinary")

    I_D = hydra.core.Name("ID")

    I_D_R_E_F = hydra.core.Name("IDREF")

    I_D_R_E_F_S = hydra.core.Name("IDREFS")

    INT = hydra.core.Name("int")

    INTEGER = hydra.core.Name("integer")

    LANGUAGE = hydra.core.Name("language")

    LONG = hydra.core.Name("long")

    N_M_T_O_K_E_N = hydra.core.Name("NMTOKEN")

    N_O_T_A_T_I_O_N = hydra.core.Name("NOTATION")

    NAME = hydra.core.Name("name")

    NEGATIVE_INTEGER = hydra.core.Name("negativeInteger")

    NON_NEGATIVE_INTEGER = hydra.core.Name("nonNegativeInteger")

    NON_POSITIVE_INTEGER = hydra.core.Name("nonPositiveInteger")

    NORMALIZED_STRING = hydra.core.Name("normalizedString")

    POSITIVE_INTEGER = hydra.core.Name("positiveInteger")

    Q_NAME = hydra.core.Name("qName")

    SHORT = hydra.core.Name("short")

    STRING = hydra.core.Name("string")

    TIME = hydra.core.Name("time")

    TOKEN = hydra.core.Name("token")

    UNSIGNED_BYTE = hydra.core.Name("unsignedByte")

    UNSIGNED_INT = hydra.core.Name("unsignedInt")

    UNSIGNED_LONG = hydra.core.Name("unsignedLong")

    UNSIGNED_SHORT = hydra.core.Name("unsignedShort")

Datatype.TYPE_ = hydra.core.Name("hydra.xml.schema.Datatype")
