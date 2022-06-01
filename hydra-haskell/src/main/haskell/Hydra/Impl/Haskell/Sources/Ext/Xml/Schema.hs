{-|
Partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.
See: https://www.w3.org/TR/xmlschema-2

Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply
      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly
      corresponding Hydra literal type. 
-}

module Hydra.Impl.Haskell.Sources.Ext.Xml.Schema where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


xmlSchemaModule :: Module Meta
xmlSchemaModule = Module xmlSchema []

xmlSchemaName :: GraphName
xmlSchemaName = GraphName "hydra/ext/xml/schema"

xmlSchema :: Graph Meta
xmlSchema = Graph xmlSchemaName elements (const True) hydraCoreName
  where
    def = datatype xmlSchemaName

    elements = [

      def "AnySimpleType" string,
      def "AnyType" string,
      def "AnyURI" string,
      def "Base64Binary" string,
      def "Boolean" boolean,
      def "Byte" int8,
      def "Date" string,
      def "DateTime" string,
      def "Decimal" string,
      def "Double" float64,
      def "Duration" string,
      def "ENTITIES" string,
      def "ENTITY" string,
      def "Float" float32,
      def "GDay" string,
      def "GMonth" string,
      def "GMonthDay" string,
      def "GYear" string,
      def "GYearMonth" string,
      def "HexBinary" string,
      def "ID" string,
      def "IDREF" string,
      def "IDREFS" string,
      def "Int" int32,
      def "Integer" bigint,
      def "Language" string,
      def "Long" int64,
      def "NMTOKEN" string,
      def "NOTATION" string,
      def "Name" string,
      def "NegativeInteger" bigint,
      def "NonNegativeInteger" bigint,
      def "NonPositiveInteger" bigint,
      def "NormalizedString" string,
      def "PositiveInteger" bigint,
      def "QName" string,
      def "Short" int16,
      def "String" string,
      def "Time" string,
      def "Token" string,
      def "UnsignedByte" uint8,
      def "UnsignedInt" uint32,
      def "UnsignedLong" uint64,
      def "UnsignedShort" uint16]
