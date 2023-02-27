{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Core where

import Hydra.Kernel
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


hydraCore :: Graph Kv
hydraCore = elementsToGraph Nothing (moduleElements hydraCoreModule)

hydraCoreModule :: Module Kv
hydraCoreModule = Module ns elements [] $
    Just "Hydra's core data model, defining types, terms, and their dependencies"
  where
    ns = Namespace "hydra/core"
    core = nsref ns
    def = datatype ns
    doc s = setTypeDescription bootstrapContext (Just s)

    elements = [

      def "Annotated" $
        doc "An object, such as a type or term, together with an annotation" $
        lambda "a" $ lambda "m" $ record [
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

      def "CaseStatement" $
        doc "A union elimination; a case statement" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "cases">: list $ core "Field" @@ "m"],

      def "Elimination" $
        doc "A corresponding elimination for an introduction term" $
        lambda "m" $ union [
          "element">:
            doc "Eliminates an element by mapping it to its data term. This is Hydra's delta function."
            unit,
          "list">:
            doc "Eliminates a list using a fold function; this function has the signature b -> [a] -> b" $
            core "Term" @@ "m",
          "optional">:
            doc "Eliminates an optional term by matching over the two possible cases" $
            core "OptionalCases" @@ "m",
          "record">:
            doc "Eliminates a record by projecting a given field" $
            core "Projection",
          "union">:
            doc "Eliminates a union term by matching over the fields of the union. This is a case statement." $
            core "CaseStatement" @@ "m",
          "wrap">:
            doc "Unwrap a wrapped term" $
            core "Name"],
            
      def "Field" $
        doc "A labeled term" $
        lambda "m" $ record [
          "name">: core "FieldName",
          "term">: core "Term" @@ "m"],

      def "FieldName" $
        doc "The name of a field, unique within a record or union type"
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
            core "Name",
          "body">:
            doc "The body of the lambda" $
            core "Term" @@ "m"],

      def "LambdaType" $
        doc "A type abstraction; the type-level analog of a lambda term" $
        lambda "m" $ record [
          "parameter">:
            doc "The parameter of the lambda" $
            core "Name",
          "body">:
            doc "The body of the lambda" $
            core "Type" @@ "m"],

      def "Let" $
        doc "A set of (possibly recursive) 'let' bindings" $
        lambda "m" $ record [
          "bindings">: Types.map (core "Name") (core "Term" @@ "m"),
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

      def "MapType" $
        doc "A map type" $
        lambda "m" $ record [
          "keys">: core "Type" @@ "m",
          "values">: core "Type" @@ "m"],

      def "Name" $
        doc "A symbol which stands for a term, type, or element"
        string,

      def "Nominal" $
        doc "An object wrapped in a type name" $
        lambda "a" $ record [
          "typeName">: core "Name",
          "object">: "a"],

      def "OptionalCases" $
        doc "A case statement for matching optional terms" $
        lambda "m" $ record [
          "nothing">:
            doc "A term provided if the optional value is nothing" $
            core "Term" @@ "m",
          "just">:
            doc "A function which is applied if the optional value is non-nothing" $
            core "Term" @@ "m"],

      def "Projection" $
        doc "A record elimination; a projection" $
        record [
          "typeName">: core "Name",
          "field">: core "FieldName"],

      def "Record" $
        doc "A record, or labeled tuple; a map of field names to terms" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "fields">: list $ core "Field" @@ "m"],

      def "RowType" $
        doc "A labeled record or union type" $
        lambda "m" $ record [
          "typeName">:
            doc "The name of the row type, which must correspond to the name of a Type element" $
            core "Name",
          "extends">:
            doc ("Optionally, the name of another row type which this one extends. If/when field order " ++
                 "is preserved, the inherited fields of the extended type precede those of the extension.") $
            optional $ core "Name",
          "fields">:
            doc "The fields of this row type, excluding any inherited fields" $
            list $ core "FieldType" @@ "m"],

      def "Stream" $
        doc "An infinite stream of terms" $
        lambda "m" $ record [
          "first">: core "Term" @@ "m",
          "rest">: core "Stream" @@ "m"],

      def "Sum" $
        doc "The unlabeled equivalent of an Injection term" $
        lambda "m" $ record [
          "index">: int32,
          "size">: int32,
          "term">: core "Term" @@ "m"],

      def "Term" $
        doc "A data term" $
        lambda "m" $ union [
          "annotated">:
            doc "A term annotated with metadata" $
            core "Annotated" @@ (core "Term" @@ "m") @@ "m",
          "application">:
            doc "A function application" $
            core "Application" @@ "m",
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
          "literal">:
            doc "A literal value" $
            core "Literal",
          "map">:
            doc "A map of keys to values" $
            Types.map (core "Term" @@ "m") (core "Term" @@ "m"),
          "optional">:
            doc "An optional value" $
            optional $ core "Term" @@ "m",
          "product">:
            doc "A tuple" $
            list (core "Term" @@ "m"),
          "record">:
            doc "A record term" $
            core "Record" @@ "m",
          "set">:
            doc "A set of values" $
            set $ core "Term" @@ "m",
          "stream">:
            doc "An infinite stream of terms" $
            core "Stream" @@ "m",
          "sum">:
            doc "A variant tuple" $
            core "Sum" @@ "m",
          "union">:
            doc "An injection; an instance of a union type" $
            core "Injection" @@ "m",
          "variable">:
            doc "A variable reference" $
            core "Name",
          "wrap">:
            core "Nominal" @@ (core "Term" @@ "m")],

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
          "optional">: core "Type" @@ "m",
          "product">: list (core "Type" @@ "m"),
          "record">: core "RowType" @@ "m",
          "set">: core "Type" @@ "m",
          "stream">: core "Type" @@ "m",
          "sum">: list (core "Type" @@ "m"),
          "union">: core "RowType" @@ "m",
          "variable">: core "Name",
          "wrap">: core "Name"],

      def "Injection" $
        doc "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()" $
        lambda "m" $ record [
          "typeName">: core "Name",
          "field">: core "Field" @@ "m"],

      def "UnitType" $
        doc "An empty record type as a canonical unit type" $
        record []]
