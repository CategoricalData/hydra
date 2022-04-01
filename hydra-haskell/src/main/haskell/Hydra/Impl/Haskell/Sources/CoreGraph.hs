module Hydra.Impl.Haskell.Sources.CoreGraph where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Dsl.Standard


hydraCoreGraph :: Graph Meta
hydraCoreGraph = Graph modName elements (const True) "hydra/core"
  where
    -- Note: here, the element namespace "hydra/core" doubles as a graph name
    modName = "hydra/core"

    qualify lname = modName ++ "." ++ lname

    local = TypeNominal . qualify

    element lname doc typ = typeElement standardContext (qualify lname) doc typ

    elements = [

      element "Application"
        "A term which applies a function to an argument" $
        universal "m" $ TypeRecord [
          FieldType "function" $ universal "m" $ local "Term",
          FieldType "argument" $ universal "m" $ local "Term"],

      element "BooleanValue"
        "A boolean literal value" $
        enum [
          "false",
          "true"],

      element "Comparison"
        "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      element "Expression"
        "A term expression" $
        universal "m" $ TypeUnion [
          FieldType "application" $ universal "m" $ local "Application",
          FieldType "literal" $ local "Literal",
          FieldType "element" $ local "Name",
          FieldType "function" $ universal "m" $ local "Function",
          FieldType "let" $ universal "m" $ local "Let",
          FieldType "list" $ TypeList $ universal "m" $ local "Term",
          FieldType "map" $ TypeMap $ MapType (universal "m" $ local "Term") (universal "m" $ local "Term"),
          FieldType "nominal" $ universal "m" $ local "NominalTerm",
          FieldType "optional" $ TypeOptional $ universal "m" $ local "Term",
          FieldType "record" $ TypeList $ universal "m" $ local "Field",
          FieldType "set" $ TypeSet $ universal "m" $ local "Term",
          FieldType "typeAbstraction" $ universal "m" $ local "TypeAbstraction",
          FieldType "typeApplication" $ universal "m" $ local "TypeApplication",
          FieldType "union" $ universal "m" $ local "Field",
          FieldType "variable" $ local "Variable"],

      element "Field"
        "A labeled term" $
        universal "m" $ TypeRecord [
          FieldType "name" $ local "FieldName",
          FieldType "term" $ universal "m" $ local "Term"],

      element "FieldName"
        "The name of a field"
        stringType,

      element "FieldType"
        "The name and type of a field" $
        TypeRecord [
          FieldType "name" $ local "FieldName",
          FieldType "type" $ local "Type"],

      element "FloatType"
        "A floating-point type" $
        enum [
          "bigfloat",
          "float32",
          "float64"],

      element "FloatValue"
        "A floating-point literal value" $
        TypeUnion [
          FieldType "bigfloat" bigfloatType,
          FieldType "float32" float32Type,
          FieldType "float64" float64Type],

      element "Function"
        "A function" $
        universal "m" $ TypeUnion [
          FieldType "cases" $ TypeList $ universal "m" $ local "Field",
          FieldType "compareTo" $ universal "m" $ local "Term",
          FieldType "data" unitType,
          FieldType "lambda" $ universal "m" $ local "Lambda",
          FieldType "optionalCases" $ universal "m" $ local "OptionalCases",
          FieldType "primitive" $ local "Name",
          FieldType "projection" $ local "FieldName"],

      element "FunctionType"
        "A function type, also known as an arrow type" $
        TypeRecord [
          FieldType "domain" $ local "Type",
          FieldType "codomain" $ local "Type"],

      element "FunctionVariant"
        "The identifier of a function constructor" $
        enum [
          "cases",
          "compareTo",
          "data",
          "lambda",
          "optionalCases",
          "primitive",
          "projection"],

      element "IntegerType"
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

      element "IntegerValue"
        "An integer literal value" $
        TypeUnion [
          FieldType "bigint" bigintType,
          FieldType "int8" int8Type,
          FieldType "int16" int16Type,
          FieldType "int32" int32Type,
          FieldType "int64" int64Type,
          FieldType "uint8" uint8Type,
          FieldType "uint16" uint16Type,
          FieldType "uint32" uint32Type,
          FieldType "uint64" uint64Type],

      element "Lambda"
        "A function abstraction (lambda)" $
        universal "m" $ TypeRecord [
          FieldType "parameter" $ local "Variable",
          FieldType "body" $ universal "m" $ local "Term"],

      element "Let"
        "A 'let' binding" $
        universal "m" $ TypeRecord [
          FieldType "key" $ local "Variable",
          FieldType "value" $ universal "m" $ local "Term",
          FieldType "environment" $ universal "m" $ local "Term"],

      element "Literal"
        "A term constant; an instance of a literal type" $
        TypeUnion [
          FieldType "binary" binaryType,
          FieldType "boolean" $ local "BooleanValue",
          FieldType "float" $ local "FloatValue",
          FieldType "integer" $ local "IntegerValue",
          FieldType "string" stringType],

      element "LiteralType"
        "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
        TypeUnion [
          FieldType "binary" unitType,
          FieldType "boolean" unitType,
          FieldType "float" $ local "FloatType",
          FieldType "integer" $ local "IntegerType",
          FieldType "string" unitType],

      element "LiteralVariant"
        "The identifier of a literal constructor" $
        enum [
          "binary",
          "boolean",
          "float",
          "integer",
          "string"],

      element "MapType"
        "A map type" $
        TypeRecord [
          FieldType "keys" $ local "Type",
          FieldType "values" $ local "Type"],

      element "Meta"
        "A built-in metadata container for terms" $
        TypeRecord [
          FieldType "description" (TypeOptional stringType),
          FieldType "type" (TypeOptional $ local "Type")],

      element "Name"
        "A unique element name"
        stringType,

      element "NominalTerm"
        "A term annotated with a fixed, named type; an instance of a newtype" $
        universal "m" $ TypeRecord [
          FieldType "typeName" (local "Name"),
          FieldType "term" (universal "m" $ local "Term")],

      element "OptionalCases"
        "A case statement for matching optional terms" $
        universal "m" $ TypeRecord [
          FieldType "nothing" (universal "m" $ local "Term"),
          FieldType "just" (universal "m" $ local "Term")],

      element "OptionalExpression"
        "An encoded optional value, for languages which do not natively support optionals" $
        universal "m" $ TypeUnion [
          FieldType "just" (universal "m" $ local "Term"),
          FieldType "nothing" unitType],

      element "Precision"
        "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        TypeUnion [
          FieldType "arbitrary" unitType,
          FieldType "bits" int32Type],

      element "Term"
        "A data term" $
        universal "m" $ TypeRecord [
          FieldType "data" $ universal "m" $ local "Expression",
          FieldType "meta" $ typeVariable "m"],

      element "TermVariant"
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

      element "Type"
        "A data type" $
        TypeUnion [
          FieldType "literal" $ local "LiteralType",
          FieldType "element" $ local "Type",
          FieldType "function" $ local "FunctionType",
          FieldType "list" $ local "Type",
          FieldType "map" $ local "MapType",
          FieldType "nominal" $ local "Name",
          FieldType "optional" $ local "Type",
          FieldType "record" $ TypeList $ local "FieldType",
          FieldType "set" $ local "Type",
          FieldType "union" $ TypeList $ local "FieldType",
          FieldType "universal" $ local "UniversalType",
          FieldType "variable" $ local "TypeVariable"],

      element "TypeAbstraction"
        "A type abstraction (generalization), which binds a type variable to a term" $
        universal "m" $ TypeRecord [
          FieldType "parameter" (local "TypeVariable"),
          FieldType "body" (universal "m" $ local "Term")],

      element "TypeApplication"
        "A type application (instantiation), which applies a term to a type" $
        universal "m" $ TypeRecord [
          FieldType "function" (universal "m" $ local "Term"),
          FieldType "argument" (local "Type")],

      element "TypeScheme"
        "A type expression together with free type variables occurring in the expression" $
        TypeRecord [
          FieldType "variables" (TypeList $ local "TypeVariable"),
          FieldType "type" (local "Type")],

      element "TypeVariable"
        "A symbol which stands in for a type"
        stringType,

      element "TypeVariant"
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

      element "TypedTerm"
        "A type together with an instance of the type" $
        universal "m" $ TypeRecord [
          FieldType "type" $ local "Type",
          FieldType "term" $ universal "m" $ local "Term"],

      element "UniversalType"
        "A universally quantified ('forall') type, parameterized by a type variable" $
        TypeRecord [
          FieldType "variable" stringType,
          FieldType "body" $ local "Type"],

      element "Variable"
        "A symbol which stands in for a term"
        stringType]
