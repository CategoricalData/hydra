{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Core where

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraCoreModule :: Module Meta
hydraCoreModule = Module hydraCore []

-- Note: here, the element namespace doubles as a graph name
hydraCoreName :: GraphName
hydraCoreName = GraphName "hydra/core"

hydraCore :: Graph Meta
hydraCore = Graph hydraCoreName elements (const True) hydraCoreName
  where
    core = nsref hydraCoreName
    def = datatype hydraCoreName
    elements = [

      def "Annotated" $
        lambda "a" $
        lambda "m" $ record [
          "subject">: "a",
          "annotation">: "m"],

      def "Application" $
        doc "A term which applies a function to an argument" $
        lambda "m" $ record [
          "function">:
            doc "The left-hand side of the application" $
            core "Term" @@ "m",
          "argument">:
            doc "The right-hand side of the application" $
            core "Term" @@ "m"],

      def "ApplicationType" $
        doc "The type-level analog of an application term" $
        lambda "m" $ record [
          "function">:
            doc "The left-hand side of the application" $
            core "Type" @@ "m",
          "argument">:
            doc "The right-hand side of the application" $
            core "Type" @@ "m"],

      def "Comparison" $
        doc "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      def "Elimination" $
        doc "A corresponding elimination for an introduction term" $
        lambda "m" $ union [
          "element">:
            doc "Eliminates an element by mapping it to its data term. This is Hydra's delta function."
            unit,
          "nominal">:
            doc "Eliminates a nominal term by extracting the wrapped term" $
            core "Name",
          "optional">:
            doc "Eliminates an optional term by matching over the two possible cases" $
            core "OptionalCases" @@ "m",
          "record">:
            doc "Eliminates a record by projecting a given field" $
            core "FieldName",
          "union">:
            doc "Eliminates a union term by matching over the fields of the union. This is a case statement." $
            list $ core "Field" @@ "m"],

      def "EliminationVariant" $
        doc "The identifier of an elimination constructor" $
        enum [
          "element",
          "nominal",
          "optional",
          "record",
          "union"],

      def "Field" $
        doc "A labeled term" $
        lambda "m" $ record [
          "name">: core "FieldName",
          "term">: core "Term" @@ "m"],

      def "FieldName" $
        doc "The name of a field"
        string,

      def "FieldType" $
        doc "The name and type of a field" $
        lambda "m" $ record [
          "name">: core "FieldName",
          "type">: core "Type" @@ "m"],

      def "FloatType" $
        doc "A floating-point type" $
        enum [
          "bigfloat",
          "float32",
          "float64"],

      def "FloatValue" $
        doc "A floating-point literal value" $
        union [
          "bigfloat">:
            doc "An arbitrary-precision floating-point value" bigfloat,
          "float32">:
            doc "A 32-bit floating-point value" float32,
          "float64">:
            doc "A 64-bit floating-point value" float64],

      def "Function" $
        doc "A function" $
        lambda "m" $ union [
          "compareTo">:
            doc "Compares a term with a given term of the same type, producing a Comparison" $
            core "Term" @@ "m",
          "elimination">:
            doc "An elimination for any of a few term variants" $
            core "Elimination" @@ "m",
          "lambda">:
            doc "A function abstraction (lambda)" $
            core "Lambda" @@ "m",
          "primitive">:
            doc "A reference to a built-in (primitive) function" $
            core "Name"],

      def "FunctionType" $
        doc "A function type, also known as an arrow type" $
        lambda "m" $ record [
          "domain">: core "Type" @@ "m",
          "codomain">: core "Type" @@ "m"],

      def "FunctionVariant" $
        doc "The identifier of a function constructor" $
        enum [
          "compareTo",
          "elimination",
          "lambda",
          "primitive"],

      def "IntegerType" $
        doc "An integer type" $
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

      def "IntegerValue" $
        doc "An integer literal value" $
        union [
          "bigint">:
            doc "An arbitrary-precision integer value" bigint,
          "int8">:
            doc "An 8-bit signed integer value" int8,
          "int16">:
            doc "A 16-bit signed integer value (short value)" int16,
          "int32">:
            doc "A 32-bit signed integer value (int value)" int32,
          "int64">:
            doc "A 64-bit signed integer value (long value)" int64,
          "uint8">:
            doc "An 8-bit unsigned integer value (byte)" uint8,
          "uint16">:
            doc "A 16-bit unsigned integer value" uint16,
          "uint32">:
            doc "A 32-bit unsigned integer value (unsigned int)" uint32,
          "uint64">:
            doc "A 64-bit unsigned integer value (unsigned long)" uint64],

      def "Lambda" $
        doc "A function abstraction (lambda)" $
        lambda "m" $ record [
          "parameter">:
            doc "The parameter of the lambda" $
            core "Variable",
          "body">:
            doc "The body of the lambda" $
            core "Term" @@ "m"],

      def "LambdaType" $
        doc "A type abstraction; the type-level analog of a lambda term" $
        lambda "m" $ record [
          "parameter">:
            doc "The parameter of the lambda" $
            core "VariableType",
          "body">:
            doc "The body of the lambda" $
            core "Type" @@ "m"],

      def "Let" $
        doc "A 'let' binding" $
        lambda "m" $ record [
          "key">: core "Variable",
          "value">: core "Term" @@ "m",
          "environment">: core "Term" @@ "m"],

      def "Literal" $
        doc "A term constant; an instance of a literal type" $
        union [
          "binary">:
            doc "A binary literal" binary,
          "boolean">:
            doc "A boolean literal" boolean,
          "float">:
            doc "A floating-point literal" $ core "FloatValue",
          "integer">:
            doc "An integer literal" $
            core "IntegerValue",
          "string">:
            doc "A string literal" string],

      def "LiteralType" $
        doc "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
        union [
          "binary">: unit,
          "boolean">: unit,
          "float">: core "FloatType",
          "integer">: core "IntegerType",
          "string">: unit],

      def "LiteralVariant" $
        doc "The identifier of a literal constructor" $
        enum [
          "binary",
          "boolean",
          "float",
          "integer",
          "string"],

      def "MapType" $
        doc "A map type" $
        lambda "m" $ record [
          "keys">: core "Type" @@ "m",
          "values">: core "Type" @@ "m"],

      def "Meta" $
        doc "A built-in metadata container for terms" $
        record [
          "annotations">:
            doc "A map of annotation names to annotation values" $
            Types.map string (core "Term" @@ core "Meta")],

      def "Name" $
        doc "A unique element name"
        string,

      def "Named" $
        doc "A term annotated with a fixed, named type; an instance of a newtype" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "term">: core "Term" @@ "m"],

      def "OptionalCases" $
        doc "A case statement for matching optional terms" $
        lambda "m" $ record [
          "nothing">:
            doc "A term provided if the optional value is nothing" $
            core "Term" @@ "m",
          "just">:
            doc "A function which is applied of the optional value is non-nothing" $
            core "Term" @@ "m"],

      def "Precision" $
        doc "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        union [
          "arbitrary">: unit,
          "bits">: int32],

      def "Record" $
        doc "A record, or labeled tuple; a map of field names to terms" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "fields">: list $ core "Field" @@ "m"],

      def "RecordType" $
        doc "A record, or labeled tuple, type" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "fields">: list $ core "FieldType" @@ "m"],
        
      def "Term" $
        doc "A data term" $
        lambda "m" $ union [
          "annotated">:
            doc "A term annotated with metadata" $
            core "Annotated" @@ (core "Term" @@ "m") @@ "m",
          "application">:
            doc "A function application" $
            core "Application" @@ "m",
          "literal">:
            doc "A literal value" $
            core "Literal",
          "element">:
            doc "An element reference" $
            core "Name",
          "function">:
            doc "A function term" $
            core "Function" @@ "m",
          "let">:
            core "Let" @@ "m",
          "list">:
            doc "A list" $
            list $ core "Term" @@ "m",
          -- TODO: list elimination
          "map">:
            doc "A map of keys to values" $
            Types.map (core "Term" @@ "m") (core "Term" @@ "m"),
          "nominal">:
            core "Named" @@ "m",
          "optional">:
            doc "An optional value" $
            optional $ core "Term" @@ "m",
          "record">:
            doc "A record term" $
            core "Record" @@ "m",
          "set">:
            doc "A set of values" $
            set $ core "Term" @@ "m",
          "union">:
            doc "A union term" $
            core "Union" @@ "m",
          "variable">:
            doc "A variable reference" $
            core "Variable"],

      def "TermVariant" $
        doc "The identifier of a term expression constructor" $
        enum [
          "annotated",
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
          "union",
          "universal",
          "variable"],

      def "Type" $
        doc "A data type" $
        lambda "m" $ union [
          "annotated">:
            doc "A type annotated with metadata" $
            core "Annotated" @@ (core "Type" @@ "m") @@ "m",
          "application">: core "ApplicationType" @@ "m",
          "element">: core "Type" @@ "m",
          "function">: core "FunctionType" @@ "m",
          "lambda">: core "LambdaType" @@ "m",
          "list">: core "Type" @@ "m",
          "literal">: core "LiteralType",
          "map">: core "MapType" @@ "m",
          "nominal">: core "Name",
          "optional">: core "Type" @@ "m",
          "record">: core "RecordType" @@ "m",
          "set">: core "Type" @@ "m",
          "union">: core "UnionType" @@ "m",
          "variable">: core "VariableType"],

      def "TypeScheme" $
        doc "A type expression together with free type variables occurring in the expression" $
        lambda "m" $ record [
          "variables">: list $ core "VariableType",
          "type">: core "Type" @@ "m"],

      def "TypeVariant" $
        doc "The identifier of a type constructor" $
        enum [
          "annotated",
          "application",
          "element",
          "function",
          "lambda",
          "list",
          "literal",
          "map",
          "nominal",
          "optional",
          "record",
          "set",
          "union",
          "variable"],

      def "TypedTerm" $
        doc "A type together with an instance of the type" $
        lambda "m" $ record [
          "type">: core "Type" @@ "m",
          "term">: core "Term" @@ "m"],

      def "Variable" $
        doc "A symbol which stands in for a term"
        string,

      def "VariableType" $
        doc "A symbol which stands in for a type"
        string,
        
      def "Union" $
        doc "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "field">: core "Field" @@ "m"],
          
      def "UnionType" $
        doc "A union, or labeled sum, type" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "field">: core "FieldType" @@ "m"]]
