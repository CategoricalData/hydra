module Hydra.Impl.Haskell.Sources.CoreGraph where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph gname elements (const True) "hydra/core"
  where
    -- Note: here, the element namespace "hydra/core" doubles as a graph name
    gname = "hydra/core"

    qualify lname = gname ++ "." ++ lname

    core = nominal . qualify

    datatype lname doc typ = typeElement standardContext (qualify lname) doc typ

    elements = [

      datatype "Application"
        "A term which applies a function to an argument" $
        universal "m" $ record [
          field "function" $ universal "m" $ core "Term",
          field "argument" $ universal "m" $ core "Term"],

      datatype "BooleanValue"
        "A boolean literal value" $
        enum [
          "false",
          "true"],

      datatype "Comparison"
        "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      datatype "Expression"
        "A term expression" $
        universal "m" $ union [
          field "application" $ universal "m" $ core "Application",
          field "literal" $ core "Literal",
          field "element" $ core "Name",
          field "function" $ universal "m" $ core "Function",
          field "let" $ universal "m" $ core "Let",
          field "list" $ list $ universal "m" $ core "Term",
          field "map" $ TypeMap $ MapType (universal "m" $ core "Term") (universal "m" $ core "Term"),
          field "nominal" $ universal "m" $ core "NominalTerm",
          field "optional" $ optional $ universal "m" $ core "Term",
          field "record" $ list $ universal "m" $ core "Field",
          field "set" $ TypeSet $ universal "m" $ core "Term",
          field "typeAbstraction" $ universal "m" $ core "TypeAbstraction",
          field "typeApplication" $ universal "m" $ core "TypeApplication",
          field "union" $ universal "m" $ core "Field",
          field "variable" $ core "Variable"],

      datatype "Field"
        "A labeled term" $
        universal "m" $ record [
          field "name" $ core "FieldName",
          field "term" $ universal "m" $ core "Term"],

      datatype "FieldName"
        "The name of a field"
        string,

      datatype "FieldType"
        "The name and type of a field" $
        record [
          field "name" $ core "FieldName",
          field "type" $ core "Type"],

      datatype "FloatType"
        "A floating-point type" $
        enum [
          "bigfloat",
          "float32",
          "float64"],

      datatype "FloatValue"
        "A floating-point literal value" $
        union [
          field "bigfloat" bigfloat,
          field "float32" float32,
          field "float64" float64],

      datatype "Function"
        "A function" $
        universal "m" $ union [
          field "cases" $ list $ universal "m" $ core "Field",
          field "compareTo" $ universal "m" $ core "Term",
          field "data" unit,
          field "lambda" $ universal "m" $ core "Lambda",
          field "optionalCases" $ universal "m" $ core "OptionalCases",
          field "primitive" $ core "Name",
          field "projection" $ core "FieldName"],

      datatype "FunctionType"
        "A function type, also known as an arrow type" $
        record [
          field "domain" $ core "Type",
          field "codomain" $ core "Type"],

      datatype "FunctionVariant"
        "The identifier of a function constructor" $
        enum [
          "cases",
          "compareTo",
          "data",
          "lambda",
          "optionalCases",
          "primitive",
          "projection"],

      datatype "IntegerType"
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

      datatype "IntegerValue"
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

      datatype "Lambda"
        "A function abstraction (lambda)" $
        universal "m" $ record [
          field "parameter" $ core "Variable",
          field "body" $ universal "m" $ core "Term"],

      datatype "Let"
        "A 'let' binding" $
        universal "m" $ record [
          field "key" $ core "Variable",
          field "value" $ universal "m" $ core "Term",
          field "environment" $ universal "m" $ core "Term"],

      datatype "Literal"
        "A term constant; an instance of a literal type" $
        union [
          field "binary" binary,
          field "boolean" $ core "BooleanValue",
          field "float" $ core "FloatValue",
          field "integer" $ core "IntegerValue",
          field "string" string],

      datatype "LiteralType"
        "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
        union [
          field "binary" unit,
          field "boolean" unit,
          field "float" $ core "FloatType",
          field "integer" $ core "IntegerType",
          field "string" unit],

      datatype "LiteralVariant"
        "The identifier of a literal constructor" $
        enum [
          "binary",
          "boolean",
          "float",
          "integer",
          "string"],

      datatype "MapType"
        "A map type" $
        record [
          field "keys" $ core "Type",
          field "values" $ core "Type"],

      datatype "Meta"
        "A built-in metadata container for terms" $
        record [
          field "description" (optional string),
          field "type" (optional $ core "Type")],

      datatype "Name"
        "A unique element name"
        string,

      datatype "NominalTerm"
        "A term annotated with a fixed, named type; an instance of a newtype" $
        universal "m" $ record [
          field "typeName" (core "Name"),
          field "term" (universal "m" $ core "Term")],

      datatype "OptionalCases"
        "A case statement for matching optional terms" $
        universal "m" $ record [
          field "nothing" (universal "m" $ core "Term"),
          field "just" (universal "m" $ core "Term")],

      datatype "OptionalExpression"
        "An encoded optional value, for languages which do not natively support optionals" $
        universal "m" $ union [
          field "just" (universal "m" $ core "Term"),
          field "nothing" unit],

      datatype "Precision"
        "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        union [
          field "arbitrary" unit,
          field "bits" int32],

      datatype "Term"
        "A data term" $
        universal "m" $ record [
          field "data" $ universal "m" $ core "Expression",
          field "meta" $ variable "m"],

      datatype "TermVariant"
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

      datatype "Type"
        "A data type" $
        union [
          field "literal" $ core "LiteralType",
          field "element" $ core "Type",
          field "function" $ core "FunctionType",
          field "list" $ core "Type",
          field "map" $ core "MapType",
          field "nominal" $ core "Name",
          field "optional" $ core "Type",
          field "record" $ list $ core "FieldType",
          field "set" $ core "Type",
          field "union" $ list $ core "FieldType",
          field "universal" $ core "UniversalType",
          field "variable" $ core "TypeVariable"],

      datatype "TypeAbstraction"
        "A type abstraction (generalization), which binds a type variable to a term" $
        universal "m" $ record [
          field "parameter" (core "TypeVariable"),
          field "body" (universal "m" $ core "Term")],

      datatype "TypeApplication"
        "A type application (instantiation), which applies a term to a type" $
        universal "m" $ record [
          field "function" (universal "m" $ core "Term"),
          field "argument" (core "Type")],

      datatype "TypeScheme"
        "A type expression together with free type variables occurring in the expression" $
        record [
          field "variables" (list $ core "TypeVariable"),
          field "type" (core "Type")],

      datatype "TypeVariable"
        "A symbol which stands in for a type"
        string,

      datatype "TypeVariant"
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

      datatype "TypedTerm"
        "A type together with an instance of the type" $
        universal "m" $ record [
          field "type" $ core "Type",
          field "term" $ universal "m" $ core "Term"],

      datatype "UniversalType"
        "A universally quantified ('forall') type, parameterized by a type variable" $
        record [
          field "variable" string,
          field "body" $ core "Type"],

      datatype "Variable"
        "A symbol which stands in for a term"
        string]
