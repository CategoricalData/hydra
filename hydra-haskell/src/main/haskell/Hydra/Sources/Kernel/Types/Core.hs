{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Core where

import Hydra.Kernel hiding (literalType)
import Hydra.Dsl.Annotations (doc)
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T


ns :: Namespace
ns = Namespace "hydra.core"

define :: String -> Type -> Binding
define = defineType ns

hydraCoreGraph :: Graph
hydraCoreGraph = elementsToGraph bootstrapGraph Nothing (moduleElements module_)

module_ :: Module
module_ = Module ns elements [] [module_] $ -- Note: hydra.core uniquely takes itself as a type-level dependency
    Just "Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies."
  where
    elements = [
      annotatedTerm,
      annotatedType,
      application,
      applicationType,
      binding,
      caseStatement,
      eitherType,
      pairType,
      elimination,
      field,
      fieldType,
      floatType,
      floatValue,
      forallType,
      function,
      functionType,
      injection,
      integerType,
      integerValue,
      lambda,
      let_,
      literal,
      literalType,
      mapType,
      name,
      projection,
      record,
      rowType,
      term,
      type_,
      typeApplicationTerm,
      typeLambda,
      typeScheme,
      wrappedTerm,
      wrappedType]

annotatedTerm :: Binding
annotatedTerm = define "AnnotatedTerm" $
  doc "A term together with an annotation" $
  T.record [
    "body">:
      doc "The term being annotated" $
      use term,
    "annotation">:
      doc "The annotation as a map from keys to values" $
      T.map (use name) (use term)]

annotatedType :: Binding
annotatedType = define "AnnotatedType" $
  doc "A type together with an annotation" $
  T.record [
    "body">:
      doc "The type being annotated" $
      use type_,
    "annotation">:
      doc "The annotation as a map from keys to values" $
      T.map (use name) (use term)]

application :: Binding
application = define "Application" $
  doc "A term which applies a function to an argument" $
  T.record [
    "function">:
      doc "The left-hand side of the application" $
      use term,
    "argument">:
      doc "The right-hand side of the application" $
      use term]

applicationType :: Binding
applicationType = define "ApplicationType" $
  doc "The type-level analog of an application term" $
  T.record [
    "function">:
      doc "The left-hand side of the application" $
      use type_,
    "argument">:
      doc "The right-hand side of the application" $
      use type_]

binding :: Binding
binding = define "Binding" $
  doc "A field with an optional type scheme, used to bind variables to terms in a 'let' expression" $
  T.record [
    "name">:
      doc "The name of the bound variable" $
      use name,
    "term">:
      doc "The term to which the variable is bound" $
      use term,
    "type">:
      doc "The optional type of the bound term" $
      T.optional (use typeScheme)]

caseStatement :: Binding
caseStatement = define "CaseStatement" $
  doc "A union elimination; a case statement" $
  T.record [
    "typeName">:
      doc "The name of the union type" $
      use name,
    "default">:
      doc "An optional default case, used if none of the explicit cases match" $
      T.optional (use term),
    "cases">:
      doc "A list of case alternatives, one per union field" $
      T.list (use field)]

eitherType :: Binding
eitherType = define "EitherType" $
  doc "A type which provides a choice between a 'left' type and a 'right' type" $
  T.record [
    "left">:
      doc "The 'left' alternative" $
      use type_,
    "right">:
      doc "The 'right' alternative" $
      use type_]

pairType :: Binding
pairType = define "PairType" $
  doc "A type which pairs a 'first' type and a 'second' type" $
  T.record [
    "first">:
      doc "The first component of the pair" $
      use type_,
    "second">:
      doc "The second component of the pair" $
      use type_]

elimination :: Binding
elimination = define "Elimination" $
  doc "A corresponding elimination for an introduction term" $
  T.union [
    "record">:
      doc "Eliminates a record by projecting a given field" $
      use projection,
    "union">:
      doc "Eliminates a union term by matching over the fields of the union. This is a case statement." $
      use caseStatement,
    "wrap">:
      doc "Unwrap a wrapped term" $
      use name]

field :: Binding
field = define "Field" $
  doc "A name/term pair" $
  T.record [
    "name">:
      doc "The name of the field" $
      use name,
    "term">:
      doc "The term value of the field" $
      use term]

fieldType :: Binding
fieldType = define "FieldType" $
  doc "A name/type pair" $
  T.record [
    "name">:
      doc "The name of the field" $
      use name,
    "type">:
      doc "The type of the field" $
      use type_]

floatType :: Binding
floatType = define "FloatType" $
  doc "A floating-point type" $
  T.union [
    "bigfloat">:
      doc "An arbitrary-precision floating-point type" $
      T.unit,
    "float32">:
      doc "A 32-bit floating-point type" $
      T.unit,
    "float64">:
      doc "A 64-bit floating-point type" $
      T.unit]

floatValue :: Binding
floatValue = define "FloatValue" $
  doc "A floating-point literal value" $
  T.union [
    "bigfloat">:
      doc "An arbitrary-precision floating-point value" T.bigfloat,
    "float32">:
      doc "A 32-bit floating-point value" T.float32,
    "float64">:
      doc "A 64-bit floating-point value" T.float64]

forallType :: Binding
forallType = define "ForallType" $
  doc "A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term." $
  T.record [
    "parameter">:
      doc "The variable which is bound by the lambda" $
      use name,
    "body">:
      doc "The body of the lambda" $
      use type_]

function :: Binding
function = define "Function" $
  doc "A function" $
  T.union [
    "elimination">:
      doc "An elimination for any of a few term variants" $
      use elimination,
    "lambda">:
      doc "A function abstraction (lambda)" $
      use lambda,
    "primitive">:
      doc "A reference to a built-in (primitive) function" $
      use name]

functionType :: Binding
functionType = define "FunctionType" $
  doc "A function type, also known as an arrow type" $
  T.record [
    "domain">:
      doc "The domain (input) type of the function" $
      use type_,
    "codomain">:
      doc "The codomain (output) type of the function" $
      use type_]

injection :: Binding
injection = define "Injection" $
  doc "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()" $
  T.record [
    "typeName">:
      doc "The name of the union type" $
      use name,
    "field">:
      doc "The field being injected, including its name and value" $
      use field]

integerType :: Binding
integerType = define "IntegerType" $
  doc "An integer type" $
  T.union [
    "bigint">:
      doc "An arbitrary-precision integer type" $
      T.unit,
    "int8">:
      doc "An 8-bit signed integer type" $
      T.unit,
    "int16">:
      doc "A 16-bit signed integer type" $
      T.unit,
    "int32">:
      doc "A 32-bit signed integer type" $
      T.unit,
    "int64">:
      doc "A 64-bit signed integer type" $
      T.unit,
    "uint8">:
      doc "An 8-bit unsigned integer type" $
      T.unit,
    "uint16">:
      doc "A 16-bit unsigned integer type" $
      T.unit,
    "uint32">:
      doc "A 32-bit unsigned integer type" $
      T.unit,
    "uint64">:
      doc "A 64-bit unsigned integer type" $
      T.unit]

integerValue :: Binding
integerValue = define "IntegerValue" $
  doc "An integer literal value" $
  T.union [
    "bigint">:
      doc "An arbitrary-precision integer value" T.bigint,
    "int8">:
      doc "An 8-bit signed integer value" T.int8,
    "int16">:
      doc "A 16-bit signed integer value (short value)" T.int16,
    "int32">:
      doc "A 32-bit signed integer value (int value)" T.int32,
    "int64">:
      doc "A 64-bit signed integer value (long value)" T.int64,
    "uint8">:
      doc "An 8-bit unsigned integer value (byte)" T.uint8,
    "uint16">:
      doc "A 16-bit unsigned integer value" T.uint16,
    "uint32">:
      doc "A 32-bit unsigned integer value (unsigned int)" T.uint32,
    "uint64">:
      doc "A 64-bit unsigned integer value (unsigned long)" T.uint64]

lambda :: Binding
lambda = define "Lambda" $
  doc "A function abstraction (lambda)" $
  T.record [
    "parameter">:
      doc "The parameter of the lambda" $
      use name,
    "domain">:
      doc "An optional domain type for the lambda" $
      T.optional (use type_),
    "body">:
      doc "The body of the lambda" $
      use term]

let_ :: Binding
let_ = define "Let" $
  doc "A set of (possibly recursive) 'let' bindings together with a body in which they are bound" $
  T.record [
    "bindings">:
      doc "The list of variable bindings" $
      T.list (use binding),
    "body">:
      doc "The body term in which the variables are bound" $
      use term]

literal :: Binding
literal = define "Literal" $
  doc "A term constant; an instance of a literal type" $
  T.union [
    "binary">:
      doc "A binary literal" T.binary,
    "boolean">:
      doc "A boolean literal" T.boolean,
    "float">:
      doc "A floating-point literal" $
      use floatValue,
    "integer">:
      doc "An integer literal" $
      use integerValue,
    "string">:
      doc "A string literal" T.string]

literalType :: Binding
literalType = define "LiteralType" $
  doc "Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants" $
  T.union [
    "binary">:
       doc "The type of a binary (byte string) value" T.unit,
    "boolean">:
      doc "The type of a boolean (true/false) value" T.unit,
    "float">:
      doc "The type of a floating-point value" $
      use floatType,
    "integer">:
      doc "The type of an integer value" $
      use integerType,
    "string">:
      doc "The type of a string value" T.unit]

mapType :: Binding
mapType = define "MapType" $
  doc "A map type" $
  T.record [
    "keys">:
      doc "The type of keys in the map" $
      use type_,
    "values">:
      doc "The type of values in the map" $
      use type_]

name :: Binding
name = define "Name" $
  doc "A unique identifier in some context; a string-valued key" $
  T.wrap T.string

projection :: Binding
projection = define "Projection" $
  doc "A record elimination; a projection" $
  T.record [
    "typeName">:
      doc "The name of the record type" $
      use name,
    "field">:
      doc "The name of the projected field" $
      use name]

record :: Binding
record = define "Record" $
  doc "A record, or labeled tuple; a map of field names to terms" $
  T.record [
    "typeName">:
      doc "The name of the record type" $
      use name,
    "fields">:
      doc "The fields of the record, as a list of name/term pairs" $
      T.list (use field)]

rowType :: Binding
rowType = define "RowType" $
  doc "A labeled record or union type" $
  T.record [
    "typeName">:
      doc "The name of the row type, which must correspond to the name of a Type element" $
      use name,
    "fields">:
      doc "The fields of this row type, excluding any inherited fields" $
      T.list (use fieldType)]

term :: Binding
term = define "Term" $
  doc "A data term" $
  T.union [
    "annotated">:
      doc "A term annotated with metadata" $
      use annotatedTerm,
    "application">:
      doc "A function application" $
      use application,
    "either">:
      doc "An either value" $
      T.either_ (use term) (use term),
    "function">:
      doc "A function term" $
      use function,
    "let">:
      doc "A 'let' term, which binds variables to terms" $
      use let_,
    "list">:
      doc "A list" $
      T.list (use term),
    "literal">:
      doc "A literal value" $
      use literal,
    "map">:
      doc "A map of keys to values" $
      T.map (use term) (use term),
    "maybe">:
      doc "An optional value" $
      T.optional (use term),
    "pair">:
      doc "A pair (2-tuple)" $
      T.pair (use term) (use term),
    "record">:
      doc "A record term" $
      use record,
    "set">:
      doc "A set of values" $
      T.set (use term),
    "typeApplication">:
      doc "A System F type application term" $
      use typeApplicationTerm,
    "typeLambda">:
      doc "A System F type abstraction term" $
      use typeLambda,
    "union">:
      doc "An injection; an instance of a union type" $
      use injection,
    "unit">:
      doc "A unit value; a term with no value" $
      T.unit,
    "variable">:
      doc "A variable reference" $
      use name,
    "wrap">:
      doc "A wrapped term; an instance of a wrapper type (newtype)" $
      use wrappedTerm]

type_ :: Binding
type_ = define "Type" $
  doc "A data type" $
  T.union [
    "annotated">:
      doc "An annotated type" $
      use annotatedType,
    "application">:
      doc "A type application" $
      use applicationType,
    "either">:
      doc "An either (sum) type" $
      use eitherType,
    "forall">:
      doc "A universally quantified (polymorphic) type" $
      use forallType,
    "function">:
      doc "A function type" $
      use functionType,
    "list">:
      doc "A list type" $
      use type_,
    "literal">:
      doc "A literal type" $
      use literalType,
    "map">:
      doc "A map type" $
      use mapType,
    "maybe">:
      doc "An optional type" $
      use type_,
    "pair">:
      doc "A pair (2-tuple) type" $
      use pairType,
    "record">:
      doc "A record type" $
      use rowType,
    "set">:
      doc "A set type" $
      use type_,
    "union">:
      doc "A union type with field names" $
      use rowType,
    "unit">:
      doc "The unit type" $
      T.unit,
    "variable">:
      doc "A type variable" $
      use name,
    "wrap">:
      doc "A wrapped type (newtype)" $
      use wrappedType]

typeApplicationTerm :: Binding
typeApplicationTerm = define "TypeApplicationTerm" $
  doc "A term applied to a type; a type application" $
  T.record [
    "body">:
      doc "The term being applied to a type" $
      use term,
    "type">:
      doc "The type argument" $
      use type_]

typeLambda :: Binding
typeLambda = define "TypeLambda" $
  doc "A System F type abstraction term" $
  T.record [
    "parameter">:
      doc "The type variable introduced by the abstraction" $
      use name,
    "body">:
      doc "The body of the abstraction" $
      use term]

typeScheme :: Binding
typeScheme = define "TypeScheme" $
  doc "A type expression together with free type variables occurring in the expression" $
  T.record [
    "variables">:
      doc "The free type variables" $
      T.list (use name),
    "type">:
      doc "The type expression" $
      use type_]

wrappedTerm :: Binding
wrappedTerm = define "WrappedTerm" $
  doc "A term wrapped in a type name" $
  T.record [
    "typeName">:
      doc "The name of the wrapper type" $
      use name,
    "body">:
      doc "The wrapped term" $
      use term]

wrappedType :: Binding
wrappedType = define "WrappedType" $
  doc "A type wrapped in a type name; a newtype" $
  T.record [
    "typeName">:
      doc "The name of the wrapper (newtype)" $
      use name,
    "body">:
      doc "The wrapped type" $
      use type_]
