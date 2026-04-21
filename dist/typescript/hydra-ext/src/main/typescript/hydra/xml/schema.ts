// Note: this is an automatically generated file. Do not edit.

/**
 * A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.
 * See: https://www.w3.org/TR/xmlschema-2
 * Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply
 *       the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly
 *       corresponding Hydra literal type.
 */



import * as Core from "../core.js";

export type AnySimpleType = string & { readonly __brand: "AnySimpleType" };

export type AnyType = string & { readonly __brand: "AnyType" };

export type AnyURI = string & { readonly __brand: "AnyURI" };

export type Base64Binary = string & { readonly __brand: "Base64Binary" };

export type Boolean = boolean & { readonly __brand: "Boolean" };

export type Byte = number & { readonly __brand: "Byte" };

export type Date = string & { readonly __brand: "Date" };

export type DateTime = string & { readonly __brand: "DateTime" };

export type Decimal = string & { readonly __brand: "Decimal" };

export type Double = number & { readonly __brand: "Double" };

export type Duration = string & { readonly __brand: "Duration" };

export type ENTITIES = string & { readonly __brand: "ENTITIES" };

export type ENTITY = string & { readonly __brand: "ENTITY" };

export type Float = number & { readonly __brand: "Float" };

export type GDay = string & { readonly __brand: "GDay" };

export type GMonth = string & { readonly __brand: "GMonth" };

export type GMonthDay = string & { readonly __brand: "GMonthDay" };

export type GYear = string & { readonly __brand: "GYear" };

export type GYearMonth = string & { readonly __brand: "GYearMonth" };

export type HexBinary = string & { readonly __brand: "HexBinary" };

export type ID = string & { readonly __brand: "ID" };

export type IDREF = string & { readonly __brand: "IDREF" };

export type IDREFS = string & { readonly __brand: "IDREFS" };

export type Int = number & { readonly __brand: "Int" };

export type Integer = bigint & { readonly __brand: "Integer" };

export type Language = string & { readonly __brand: "Language" };

export type Long = bigint & { readonly __brand: "Long" };

export type NMTOKEN = string & { readonly __brand: "NMTOKEN" };

export type NOTATION = string & { readonly __brand: "NOTATION" };

export type Name = string & { readonly __brand: "Name" };

export type NegativeInteger = bigint & { readonly __brand: "NegativeInteger" };

export type NonNegativeInteger = bigint & { readonly __brand: "NonNegativeInteger" };

export type NonPositiveInteger = bigint & { readonly __brand: "NonPositiveInteger" };

export type NormalizedString = string & { readonly __brand: "NormalizedString" };

export type PositiveInteger = bigint & { readonly __brand: "PositiveInteger" };

export type QName = string & { readonly __brand: "QName" };

export type Short = bigint & { readonly __brand: "Short" };

export type String = string & { readonly __brand: "String" };

export type Time = string & { readonly __brand: "Time" };

export type Token = string & { readonly __brand: "Token" };

export type UnsignedByte = bigint & { readonly __brand: "UnsignedByte" };

export type UnsignedInt = bigint & { readonly __brand: "UnsignedInt" };

export type UnsignedLong = bigint & { readonly __brand: "UnsignedLong" };

export type UnsignedShort = number & { readonly __brand: "UnsignedShort" };

export type ConstrainingFacet = void & { readonly __brand: "ConstrainingFacet" };

export type Datatype =
  | { readonly tag: "anySimpleType" }
  | { readonly tag: "anyType" }
  | { readonly tag: "anyURI" }
  | { readonly tag: "base64Binary" }
  | { readonly tag: "boolean" }
  | { readonly tag: "byte" }
  | { readonly tag: "date" }
  | { readonly tag: "dateTime" }
  | { readonly tag: "decimal" }
  | { readonly tag: "double" }
  | { readonly tag: "duration" }
  | { readonly tag: "eNTITIES" }
  | { readonly tag: "eNTITY" }
  | { readonly tag: "float" }
  | { readonly tag: "gDay" }
  | { readonly tag: "gMonth" }
  | { readonly tag: "gMonthDay" }
  | { readonly tag: "gYear" }
  | { readonly tag: "gYearMonth" }
  | { readonly tag: "hexBinary" }
  | { readonly tag: "iD" }
  | { readonly tag: "iDREF" }
  | { readonly tag: "iDREFS" }
  | { readonly tag: "int" }
  | { readonly tag: "integer" }
  | { readonly tag: "language" }
  | { readonly tag: "long" }
  | { readonly tag: "nMTOKEN" }
  | { readonly tag: "nOTATION" }
  | { readonly tag: "name" }
  | { readonly tag: "negativeInteger" }
  | { readonly tag: "nonNegativeInteger" }
  | { readonly tag: "nonPositiveInteger" }
  | { readonly tag: "normalizedString" }
  | { readonly tag: "positiveInteger" }
  | { readonly tag: "qName" }
  | { readonly tag: "short" }
  | { readonly tag: "string" }
  | { readonly tag: "time" }
  | { readonly tag: "token" }
  | { readonly tag: "unsignedByte" }
  | { readonly tag: "unsignedInt" }
  | { readonly tag: "unsignedLong" }
  | { readonly tag: "unsignedShort" };
