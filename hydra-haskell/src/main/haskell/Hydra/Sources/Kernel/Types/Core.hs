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
module_ = Module ns elements [] [ns] $ -- Note: hydra.core uniquely takes itself as a type-level dependency
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
      doc "The term being annotated"
      term,
    "annotation">:
      doc "The annotation as a map from keys to values" $
      T.map name term]

annotatedType :: Binding
annotatedType = define "AnnotatedType" $
  doc "A type together with an annotation" $
  T.record [
    "body">:
      doc "The type being annotated"
      type_,
    "annotation">:
      doc "The annotation as a map from keys to values" $
      T.map name term]

application :: Binding
application = define "Application" $
  doc "A term which applies a function to an argument" $
  T.record [
    "function">:
      doc "The left-hand side of the application"
      term,
    "argument">:
      doc "The right-hand side of the application"
      term]

applicationType :: Binding
applicationType = define "ApplicationType" $
  doc "The type-level analog of an application term" $
  T.record [
    "function">:
      doc "The left-hand side of the application"
      type_,
    "argument">:
      doc "The right-hand side of the application"
      type_]

binding :: Binding
binding = define "Binding" $
  doc "A field with an optional type scheme, used to bind variables to terms in a 'let' expression" $
  T.record [
    "name">:
      doc "The name of the bound variable"
      name,
    "term">:
      doc "The term to which the variable is bound"
      term,
    "type">:
      doc "The optional type of the bound term" $
      T.maybe typeScheme]

caseStatement :: Binding
caseStatement = define "CaseStatement" $
  doc "A union elimination; a case statement" $
  T.record [
    "typeName">:
      doc "The name of the union type"
      name,
    "default">:
      doc "An optional default case, used if none of the explicit cases match" $
      T.maybe term,
    "cases">:
      doc "A list of case alternatives, one per union field" $
      T.list field]

eitherType :: Binding
eitherType = define "EitherType" $
  doc "A type which provides a choice between a 'left' type and a 'right' type" $
  T.record [
    "left">:
      doc "The 'left' alternative"
      type_,
    "right">:
      doc "The 'right' alternative"
      type_]

pairType :: Binding
pairType = define "PairType" $
  doc "A type which pairs a 'first' type and a 'second' type" $
  T.record [
    "first">:
      doc "The first component of the pair"
      type_,
    "second">:
      doc "The second component of the pair"
      type_]

elimination :: Binding
elimination = define "Elimination" $
  doc "A corresponding elimination for an introduction term" $
  T.union [
    "record">:
      doc "Eliminates a record by projecting a given field"
      projection,
    "union">:
      doc "Eliminates a union term by matching over the fields of the union. This is a case statement."
      caseStatement,
    "wrap">:
      doc "Unwrap a wrapped term"
      name]

field :: Binding
field = define "Field" $
  doc "A name/term pair" $
  T.record [
    "name">:
      doc "The name of the field"
      name,
    "term">:
      doc "The term value of the field"
      term]

fieldType :: Binding
fieldType = define "FieldType" $
  doc "A name/type pair" $
  T.record [
    "name">:
      doc "The name of the field"
      name,
    "type">:
      doc "The type of the field"
      type_]

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
      doc "The variable which is bound by the lambda"
      name,
    "body">:
      doc "The body of the lambda"
      type_]

function :: Binding
function = define "Function" $
  doc "A function" $
  T.union [
    "elimination">:
      doc "An elimination for any of a few term variants"
      elimination,
    "lambda">:
      doc "A function abstraction (lambda)"
      lambda,
    "primitive">:
      doc "A reference to a built-in (primitive) function"
      name]

functionType :: Binding
functionType = define "FunctionType" $
  doc "A function type, also known as an arrow type" $
  T.record [
    "domain">:
      doc "The domain (input) type of the function"
      type_,
    "codomain">:
      doc "The codomain (output) type of the function"
      type_]

injection :: Binding
injection = define "Injection" $
  doc "An instance of a union type; i.e. a string-indexed generalization of inl() or inr()" $
  T.record [
    "typeName">:
      doc "The name of the union type"
      name,
    "field">:
      doc "The field being injected, including its name and value"
      field]

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
      doc "The parameter of the lambda"
      name,
    "domain">:
      doc "An optional domain type for the lambda" $
      T.maybe type_,
    "body">:
      doc "The body of the lambda"
      term]

let_ :: Binding
let_ = define "Let" $
  doc "A set of (possibly recursive) 'let' bindings together with a body in which they are bound" $
  T.record [
    "bindings">:
      doc "The list of variable bindings" $
      T.list binding,
    "body">:
      doc "The body term in which the variables are bound"
      term]

literal :: Binding
literal = define "Literal" $
  doc "A term constant; an instance of a literal type" $
  T.union [
    "binary">:
      doc "A binary literal" T.binary,
    "boolean">:
      doc "A boolean literal" T.boolean,
    "float">:
      doc "A floating-point literal"
      floatValue,
    "integer">:
      doc "An integer literal"
      integerValue,
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
      doc "The type of a floating-point value"
      floatType,
    "integer">:
      doc "The type of an integer value"
      integerType,
    "string">:
      doc "The type of a string value" T.unit]

mapType :: Binding
mapType = define "MapType" $
  doc "A map type" $
  T.record [
    "keys">:
      doc "The type of keys in the map"
      type_,
    "values">:
      doc "The type of values in the map"
      type_]

name :: Binding
name = define "Name" $
  doc "A unique identifier in some context; a string-valued key" $
  T.wrap T.string

projection :: Binding
projection = define "Projection" $
  doc "A record elimination; a projection" $
  T.record [
    "typeName">:
      doc "The name of the record type"
      name,
    "field">:
      doc "The name of the projected field"
      name]

record :: Binding
record = define "Record" $
  doc "A record, or labeled tuple; a map of field names to terms" $
  T.record [
    "typeName">:
      doc "The name of the record type"
      name,
    "fields">:
      doc "The fields of the record, as a list of name/term pairs" $
      T.list field]

rowType :: Binding
rowType = define "RowType" $
  doc "A labeled record or union type" $
  T.record [
    "typeName">:
      doc "The name of the row type, which must correspond to the name of a Type element"
      name,
    "fields">:
      doc "The fields of this row type, excluding any inherited fields" $
      T.list fieldType]

term :: Binding
term = define "Term" $
  doc "A data term" $
  T.union [
    "annotated">:
      doc "A term annotated with metadata"
      annotatedTerm,
    "application">:
      doc "A function application"
      application,
    "either">:
      doc "An either value" $
      T.either_ term term,
    "function">:
      doc "A function term"
      function,
    "let">:
      doc "A 'let' term, which binds variables to terms"
      let_,
    "list">:
      doc "A list" $
      T.list term,
    "literal">:
      doc "A literal value"
      literal,
    "map">:
      doc "A map of keys to values" $
      T.map term term,
    "maybe">:
      doc "An optional value" $
      T.maybe term,
    "pair">:
      doc "A pair (2-tuple)" $
      T.pair term term,
    "record">:
      doc "A record term"
      record,
    "set">:
      doc "A set of values" $
      T.set term,
    "typeApplication">:
      doc "A System F type application term"
      typeApplicationTerm,
    "typeLambda">:
      doc "A System F type abstraction term"
      typeLambda,
    "union">:
      doc "An injection; an instance of a union type"
      injection,
    "unit">:
      doc "A unit value; a term with no value" $
      T.unit,
    "variable">:
      doc "A variable reference"
      name,
    "wrap">:
      doc "A wrapped term; an instance of a wrapper type (newtype)"
      wrappedTerm]

type_ :: Binding
type_ = define "Type" $
  doc "A data type" $
  T.union [
    "annotated">:
      doc "An annotated type"
      annotatedType,
    "application">:
      doc "A type application"
      applicationType,
    "either">:
      doc "An either (sum) type"
      eitherType,
    "forall">:
      doc "A universally quantified (polymorphic) type"
      forallType,
    "function">:
      doc "A function type"
      functionType,
    "list">:
      doc "A list type"
      type_,
    "literal">:
      doc "A literal type"
      literalType,
    "map">:
      doc "A map type"
      mapType,
    "maybe">:
      doc "An optional type"
      type_,
    "pair">:
      doc "A pair (2-tuple) type"
      pairType,
    "record">:
      doc "A record type"
      rowType,
    "set">:
      doc "A set type"
      type_,
    "union">:
      doc "A union type with field names"
      rowType,
    "unit">:
      doc "The unit type" $
      T.unit,
    "variable">:
      doc "A type variable"
      name,
    "wrap">:
      doc "A wrapped type (newtype)"
      wrappedType]

typeApplicationTerm :: Binding
typeApplicationTerm = define "TypeApplicationTerm" $
  doc "A term applied to a type; a type application" $
  T.record [
    "body">:
      doc "The term being applied to a type"
      term,
    "type">:
      doc "The type argument"
      type_]

typeLambda :: Binding
typeLambda = define "TypeLambda" $
  doc "A System F type abstraction term" $
  T.record [
    "parameter">:
      doc "The type variable introduced by the abstraction"
      name,
    "body">:
      doc "The body of the abstraction"
      term]

typeScheme :: Binding
typeScheme = define "TypeScheme" $
  doc "A type expression together with free type variables occurring in the expression" $
  T.record [
    "variables">:
      doc "The free type variables" $
      T.list name,
    "type">:
      doc "The type expression"
      type_]

wrappedTerm :: Binding
wrappedTerm = define "WrappedTerm" $
  doc "A term wrapped in a type name" $
  T.record [
    "typeName">:
      doc "The name of the wrapper type"
      name,
    "body">:
      doc "The wrapped term"
      term]

wrappedType :: Binding
wrappedType = define "WrappedType" $
  doc "A type wrapped in a type name; a newtype" $
  T.record [
    "typeName">:
      doc "The name of the wrapper (newtype)"
      name,
    "body">:
      doc "The wrapped type"
      type_]
