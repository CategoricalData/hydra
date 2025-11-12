module Hydra.Ext.Sources.Xml.Schema where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Meta        as Meta
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


xmlSchemaModule :: Module
xmlSchemaModule = Module ns elements [Core.module_] [Core.module_] $
    Just ("A partial XML Schema model, focusing on datatypes. All simple datatypes (i.e. xsd:anySimpleType and below) are included.\n" ++
      "See: https://www.w3.org/TR/xmlschema-2\n" ++
      "Note: for most of the XML Schema datatype definitions included here, the associated Hydra type is simply\n" ++
      "      the string type. Exceptions are made for xsd:boolean and most of the numeric types, where there is a clearly\n" ++
      "      corresponding Hydra literal type.")
  where
    ns = Namespace "hydra.ext.org.w3.xml.schema"
    def = datatype ns

    elements = datatypes ++ others

    datatypes = [
      def "AnySimpleType" $ wrap string,
      def "AnyType" $ wrap string,
      def "AnyURI" $ wrap string,
      def "Base64Binary" $ wrap string,
      def "Boolean" $ wrap boolean,
      def "Byte" $ wrap int8,
      def "Date" $ wrap string,
      def "DateTime" $ wrap string,
      def "Decimal" $ wrap string,
      def "Double" $ wrap float64,
      def "Duration" $ wrap string,
      def "ENTITIES" $ wrap string,
      def "ENTITY" $ wrap string,
      def "Float" $ wrap float32,
      def "GDay" $ wrap string,
      def "GMonth" $ wrap string,
      def "GMonthDay" $ wrap string,
      def "GYear" $ wrap string,
      def "GYearMonth" $ wrap string,
      def "HexBinary" $ wrap string,
      def "ID" $ wrap string,
      def "IDREF" $ wrap string,
      def "IDREFS" $ wrap string,
      def "Int" $ wrap int32,
      def "Integer" $ wrap bigint,
      def "Language" $ wrap string,
      def "Long" $ wrap int64,
      def "NMTOKEN" $ wrap string,
      def "NOTATION" $ wrap string,
      def "Name" $ wrap string,
      def "NegativeInteger" $ wrap bigint,
      def "NonNegativeInteger" $ wrap bigint,
      def "NonPositiveInteger" $ wrap bigint,
      def "NormalizedString" $ wrap string,
      def "PositiveInteger" $ wrap bigint,
      def "QName" $ wrap string,
      def "Short" $ wrap int16,
      def "String" $ wrap string,
      def "Time" $ wrap string,
      def "Token" $ wrap string,
      def "UnsignedByte" $ wrap uint8,
      def "UnsignedInt" $ wrap uint32,
      def "UnsignedLong" $ wrap uint64,
      def "UnsignedShort" $ wrap uint16]

    others = [
      def "ConstrainingFacet" $
        see "https://www.w3.org/TR/xmlschema-2/#non-fundamental" $
        wrap unit, -- TODO: concrete facets

      def "Datatype" $ enum [
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
        "unsignedShort"]]
