module Hydra.Impl.Haskell.Sources.Core where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraCoreModule = Module hydraCore []

-- Note: here, the element namespace doubles as a graph name
hydraCoreName = "hydra/core"

hydraCore :: Graph Meta
hydraCore = Graph hydraCoreName elements (const True) hydraCoreName
  where
    core = nominal . qualify hydraCoreName
    def = datatype hydraCoreName
    elements = [

      def "Application"
        "A term which applies a function to an argument" $
        universal "m" $ record [
          field "function" $ universal "m" $ core "Data",
          field "argument" $ universal "m" $ core "Data"],

      def "BooleanValue"
        "A boolean literal value" $
        enum [
          "false",
          "true"],

      def "Comparison"
        "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      def "Data"
        "A data term" $
        universal "m" $ record [
          field "term" $ universal "m" $ core "DataTerm",
          field "meta" $ variable "m"],

      def "DataTerm"
        "A term expression" $
        universal "m" $ union [
          field "application" $ universal "m" $ core "Application",
          field "literal" $ core "Literal",
          field "element" $ core "Name",
          field "function" $ universal "m" $ core "Function",
          field "let" $ universal "m" $ core "Let",
          field "list" $ list $ universal "m" $ core "Data",
          field "map" $ Types.map (universal "m" $ core "Data") (universal "m" $ core "Data"),
          field "nominal" $ universal "m" $ core "Named",
          field "optional" $ optional $ universal "m" $ core "Data",
          field "record" $ list $ universal "m" $ core "Field",
          field "set" $ set $ universal "m" $ core "Data",
          field "typeAbstraction" $ universal "m" $ core "TypeAbstraction",
          field "typeApplication" $ universal "m" $ core "TypeApplication",
          field "union" $ universal "m" $ core "Field",
          field "variable" $ core "Variable"],

      def "DataVariant"
        "The identifier of a term expression constructor" $
        enum [
          "application",
          "element",
          "function",
          "let",
          "list",
          "literal",
          "map",
          "nominal",
          "optional",
          "record",
          "set",
          "typeAbstraction",
          "typeApplication",
          "union",
          "variable"],

      def "Field"
        "A labeled term" $
        universal "m" $ record [
          field "name" $ core "FieldName",
          field "data" $ universal "m" $ core "Data"],

      def "FieldName"
        "The name of a field"
        string,

      def "FieldType"
        "The name and type of a field" $
        universal "m" $ record [
          field "name" $ core "FieldName",
          field "type" $ universal "m" $ core "Type"],

      def "FloatType"
        "A floating-point type" $
        enum [
          "bigfloat",
          "float32",
          "float64"],

      def "FloatValue"
        "A floating-point literal value" $
        union [
          field "bigfloat" bigfloat,
          field "float32" float32,
          field "float64" float64],

      def "Function"
        "A function" $
        universal "m" $ union [
          field "cases" $ list $ universal "m" $ core "Field",
          field "compareTo" $ universal "m" $ core "Data",
          field "data" unit,
          field "lambda" $ universal "m" $ core "Lambda",
          field "optionalCases" $ universal "m" $ core "OptionalCases",
          field "primitive" $ core "Name",
          field "projection" $ core "FieldName"],

      def "FunctionType"
        "A function type, also known as an arrow type" $
        universal "m" $ record [
          field "domain" $ universal "m" $ core "Type",
          field "codomain" $ universal "m" $ core "Type"],

      def "FunctionVariant"
        "The identifier of a function constructor" $
        enum [
          "cases",
          "compareTo",
          "data",
          "lambda",
          "optionalCases",
          "primitive",
          "projection"],

      def "IntegerType"
        "An integer type" $
        enum [
          "bigint",
          "int8",
          "int16",
          "int32",
          "int64",
          "uint8",
          "uint16",
          "uint32",
          "uint64"],

      def "IntegerValue"
        "An integer literal value" $
        union [
          field "bigint" bigint,
          field "int8" int8,
          field "int16" int16,
          field "int32" int32,
          field "int64" int64,
          field "uint8" uint8,
          field "uint16" uint16,
          field "uint32" uint32,
          field "uint64" uint64],

      def "Lambda"
        "A function abstraction (lambda)" $
        universal "m" $ record [
          field "parameter" $ core "Variable",
          field "body" $ universal "m" $ core "Data"],

      def "Let"
        "A 'let' binding" $
        universal "m" $ record [
          field "key" $ core "Variable",
          field "value" $ universal "m" $ core "Data",
          field "environment" $ universal "m" $ core "Data"],

      def "Literal"
        "A term constant; an instance of a literal type" $
        union [
          field "binary" binary,
          field "boolean" $ core "BooleanValue",
          field "float" $ core "FloatValue",
          field "integer" $ core "IntegerValue",
          field "string" string],

      def "LiteralType"
        "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
        union [
          field "binary" unit,
          field "boolean" unit,
          field "float" $ core "FloatType",
          field "integer" $ core "IntegerType",
          field "string" unit],

      def "LiteralVariant"
        "The identifier of a literal constructor" $
        enum [
          "binary",
          "boolean",
          "float",
          "integer",
          "string"],

      def "MapType"
        "A map type" $
        universal "m" $ record [
          field "keys" $ universal "m" $ core "Type",
          field "values" $ universal "m" $ core "Type"],

      def "Meta"
        "A built-in metadata container for terms" $
        record [
          field "description" $ optional string,
          field "type" $ optional $ universal "m" $ core "Type"],

      def "Name"
        "A unique element name"
        string,

      def "Named"
        "A term annotated with a fixed, named type; an instance of a newtype" $
        universal "m" $ record [
          field "typeName" (core "Name"),
          field "term" (universal "m" $ core "Data")],

      def "OptionalCases"
        "A case statement for matching optional terms" $
        universal "m" $ record [
          field "nothing" (universal "m" $ core "Data"),
          field "just" (universal "m" $ core "Data")],

      def "Precision"
        "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        union [
          field "arbitrary" unit,
          field "bits" int32],

      def "Type"
        "A data type" $
        universal "m" $ record [
          field "term" $ universal "m" $ core "TypeTerm",
          field "meta" $ variable "m"],

      def "TypeTerm"
        "A data type" $
        universal "m" $ union [
          field "element" $ universal "m" $ core "Type",
          field "function" $ universal "m" $ core "FunctionType",
          field "list" $ universal "m" $ core "Type",
          field "literal" $ core "LiteralType",
          field "map" $ universal "m" $ core "MapType",
          field "nominal" $ core "Name",
          field "optional" $ universal "m" $ core "Type",
          field "record" $ list $ universal "m" $ core "FieldType",
          field "set" $ universal "m" $ core "Type",
          field "union" $ list $ universal "m" $ core "FieldType",
          field "universal" $ universal "m" $ core "UniversalType",
          field "variable" $ core "TypeVariable"],

      def "TypeAbstraction"
        "A type abstraction (generalization), which binds a type variable to a term" $
        universal "m" $ record [
          field "parameter" (core "TypeVariable"),
          field "body" (universal "m" $ core "Data")],

      def "TypeApplication"
        "A type application (instantiation), which applies a term to a type" $
        universal "m" $ record [
          field "function" $ universal "m" $ core "Data",
          field "argument" $ universal "m" $ core "Type"],

      def "TypeScheme"
        "A type expression together with free type variables occurring in the expression" $
        universal "m" $ record [
          field "variables" $ list $ core "TypeVariable",
          field "type" $ universal "m" $ core "Type"],

      def "TypeVariable"
        "A symbol which stands in for a type"
        string,

      def "TypeVariant"
        "The identifier of a type constructor" $
        enum [
          "element",
          "function",
          "list",
          "literal",
          "map",
          "nominal",
          "optional",
          "record",
          "set",
          "union",
          "universal",
          "variable"],

      def "TypedData"
        "A type together with an instance of the type" $
        universal "m" $ record [
          field "type" $ universal "m" $ core "Type",
          field "term" $ universal "m" $ core "Data"],

      def "UniversalType"
        "A universally quantified ('forall') type, parameterized by a type variable" $
        universal "m" $ record [
          field "variable" string,
          field "body" $ universal "m" $ core "Type"],

      def "Variable"
        "A symbol which stands in for a term"
        string]
