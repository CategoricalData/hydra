{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Kernel.Terms.Extract.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (lambdaBody, map, setType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Paths    as Paths
import qualified Hydra.Dsl.Annotations       as Annotations
import qualified Hydra.Dsl.Ast          as Ast
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Coders       as Coders
import qualified Hydra.Dsl.Util      as Util
import qualified Hydra.Dsl.Meta.Core         as Core
import qualified Hydra.Dsl.Meta.Graph        as Graph
import qualified Hydra.Dsl.Json.Model         as Json
import qualified Hydra.Dsl.Meta.Lib.Chars    as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers  as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Lists    as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps     as Maps
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs    as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets     as Sets
import           Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Literals          as Literals
import qualified Hydra.Dsl.LiteralTypes      as LiteralTypes
import qualified Hydra.Dsl.Meta.Base         as MetaBase
import qualified Hydra.Dsl.Meta.Terms        as MetaTerms
import qualified Hydra.Dsl.Meta.Types        as MetaTypes
import qualified Hydra.Dsl.Module       as Module
import qualified Hydra.Dsl.Parsing      as Parsing
import           Hydra.Dsl.Meta.Phantoms hiding (
  bigfloat, bigint, binary, boolean, cases, field, float32, float64, floatValue, injection, int8, int16, int32, int64,
  integerValue, lambda, list, literal, map, pair, set, record, string, unit, wrap, uint8, uint16, uint32, uint64)
import qualified Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Prims             as Prims
import qualified Hydra.Dsl.Meta.Tabular           as Tabular
import qualified Hydra.Dsl.Meta.Testing      as Testing
import qualified Hydra.Dsl.Terms             as Terms
import qualified Hydra.Dsl.Tests             as Tests
import qualified Hydra.Dsl.Topology     as Topology
import qualified Hydra.Dsl.Types             as Types
import qualified Hydra.Dsl.Typing       as Typing
import qualified Hydra.Dsl.Util         as Util
import qualified Hydra.Dsl.Meta.Variants     as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map)
import qualified Data.Int                    as I
import qualified Data.List                   as L
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.Maybe                  as Y

import qualified Hydra.Dsl.Meta.Context      as Ctx
import qualified Hydra.Dsl.Errors       as Error
import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical

import qualified Hydra.Dsl.Meta.DeepCore as DC
import           Hydra.Dsl.Meta.DeepCore ((@@@))

import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Sources.Kernel.Terms.Strip as Strip
import qualified Hydra.Sources.Kernel.Terms.Show.Errors as ShowError

formatError :: TTerm (InContext Error -> String)
formatError = "ic" ~> ShowError.error_ @@ Ctx.inContextObject (var "ic")


-- Helper for Either-based unexpected errors (replaces Monads.unexpected for migrated functions)
unexpected :: TTerm Context -> TTerm String -> TTerm String -> TTerm (Prelude.Either (InContext Error) a)
unexpected cx expected actual = Ctx.failInContext (Error.errorOther $ Error.otherError (Phantoms.string "expected " ++ expected ++ Phantoms.string " but found " ++ actual)) cx

ns :: Namespace
ns = Namespace "hydra.extract.core"

module_ :: Module
module_ = Module ns elements
    [Lexical.ns, Strip.ns, ShowCore.ns, ShowError.ns]
    kernelTypesNamespaces $
    Just ("Extraction and validation for hydra.core types")
  where
   elements = [
     toDefinition bigfloat,
     toDefinition bigfloatValue,
     toDefinition bigint,
     toDefinition bigintValue,
     toDefinition binary,
     toDefinition binaryLiteral,
     toDefinition boolean,
     toDefinition booleanLiteral,
     toDefinition caseField,
     toDefinition cases,
     toDefinition decodeEither,
     toDefinition decodeList,
     toDefinition decodeMap,
     toDefinition decodeMaybe,
     toDefinition decodePair,
     toDefinition decodeSet,
     toDefinition decodeUnit,
     toDefinition decodeWrapped,
     toDefinition field,
     toDefinition float32,
     toDefinition float32Value,
     toDefinition float64,
     toDefinition float64Value,
     toDefinition floatLiteral,
     toDefinition floatValue,
     toDefinition eitherTerm,
     toDefinition eitherType,
     toDefinition functionType,
     toDefinition injection,
     toDefinition int16,
     toDefinition int16Value,
     toDefinition int32,
     toDefinition int32Value,
     toDefinition int64,
     toDefinition int64Value,
     toDefinition int8,
     toDefinition int8Value,
     toDefinition integerLiteral,
     toDefinition integerValue,
     toDefinition lambdaBody,
     toDefinition lambda,
     toDefinition letBinding,
     toDefinition let_,
     toDefinition list,
     toDefinition listHead,
     toDefinition listOf,
     toDefinition listType,
     toDefinition literal,
     toDefinition map,
     toDefinition mapType,
     toDefinition nArgs,
     toDefinition maybeTerm,
     toDefinition maybeType,
     toDefinition pair,
     toDefinition record,
     toDefinition recordType,
     toDefinition requireField,
     toDefinition set,
     toDefinition setOf,
     toDefinition setType,
     toDefinition string,
     toDefinition stringLiteral,
     toDefinition termRecord,
     toDefinition toFieldMap,
     toDefinition uint16,
     toDefinition uint16Value,
     toDefinition uint32,
     toDefinition uint32Value,
     toDefinition uint64,
     toDefinition uint64Value,
     toDefinition uint8,
     toDefinition uint8Value,
     toDefinition unionType,
     toDefinition unit,
     toDefinition unitVariant,
     toDefinition wrap,
     toDefinition wrappedType]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

bigfloat :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Double)
bigfloat = define "bigfloat" $
  doc "Extract an arbitrary-precision floating-point value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "f" <<~ floatLiteral @@ var "cx" @@ var "l" $
  bigfloatValue @@ var "cx" @@ var "f"

bigfloatValue :: TTermDefinition (Context -> FloatValue -> Prelude.Either (InContext Error) Double)
bigfloatValue = define "bigfloatValue" $
  doc "Extract a bigfloat value from a FloatValue" $
  "cx" ~> "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "bigfloat") (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_bigfloat>>: "f" ~> right (var "f")]

bigint :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Integer)
bigint = define "bigint" $
  doc "Extract an arbitrary-precision integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  bigintValue @@ var "cx" @@ var "i"

bigintValue :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) Integer)
bigintValue = define "bigintValue" $
  doc "Extract a bigint value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "bigint") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_bigint>>: "i" ~> right (var "i")]

binary :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) String)
binary = define "binary" $
  doc "Extract a binary data value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  binaryLiteral @@ var "cx" @@ var "l"

binaryLiteral :: TTermDefinition (Context -> Literal -> Prelude.Either (InContext Error) String)
binaryLiteral = define "binaryLiteral" $
  doc "Extract a binary literal from a Literal value" $
  "cx" ~> "v" ~> Phantoms.cases _Literal (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "binary") (ShowCore.literal @@ var "v"))) [
    _Literal_binary>>: "b" ~> right (var "b")]

boolean :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Bool)
boolean = define "boolean" $
  doc "Extract a boolean value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  booleanLiteral @@ var "cx" @@ var "l"

booleanLiteral :: TTermDefinition (Context -> Literal -> Prelude.Either (InContext Error) Bool)
booleanLiteral = define "booleanLiteral" $
  doc "Extract a boolean literal from a Literal value" $
  "cx" ~> "v" ~> Phantoms.cases _Literal (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "boolean") (ShowCore.literal @@ var "v"))) [
    _Literal_boolean>>: "b" ~> right (var "b")]

-- TODO: nonstandard; move me
caseField :: TTermDefinition (Context -> Name -> String -> Graph -> Term -> Prelude.Either (InContext Error) Field)
caseField = define "caseField" $
  doc "Extract a specific case handler from a case statement term" $
  "cx" ~> "name" ~> "n" ~> "graph" ~> "term" ~>
  "fieldName" <~ Core.name (var "n") $
  "cs" <<~ cases @@ var "cx" @@ var "name" @@ var "graph" @@ var "term" $
  "matching" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fieldName"))
    (Core.caseStatementCases (var "cs")) $
  Logic.ifElse (Lists.null (var "matching"))
    (Ctx.failInContext (Error.errorOther $ Error.otherError (Phantoms.string "not enough cases")) (var "cx"))
    (right (Lists.head (var "matching")))

-- TODO: nonstandard; move me
cases :: TTermDefinition (Context -> Name -> Graph -> Term -> Prelude.Either (InContext Error) CaseStatement)
cases = define "cases" $
  doc "Extract case statement from a term" $
  "cx" ~> "name" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "case statement") (ShowCore.term @@ var "term"))) [
    _Term_function>>: "function" ~> Phantoms.cases _Function (var "function")
      (Just (unexpected (var "cx") (Phantoms.string "case statement") (ShowCore.term @@ var "term"))) [
      _Function_elimination>>: "elimination" ~> Phantoms.cases _Elimination (var "elimination")
        (Just (unexpected (var "cx") (Phantoms.string "case statement") (ShowCore.term @@ var "term"))) [
        _Elimination_union>>: "cs" ~>
          Logic.ifElse (Core.equalName_ (Core.caseStatementTypeName (var "cs")) (var "name"))
            (right (var "cs"))
            (unexpected (var "cx")
              (Phantoms.string "case statement for type " ++ (Core.unName (var "name")))
              (ShowCore.term @@ var "term"))]]]

-- TODO: nonstandard; move me
field :: TTermDefinition (Context -> Name -> (Term -> Prelude.Either (InContext Error) x) -> Graph -> [Field] -> Prelude.Either (InContext Error) x)
field = define "field" $
  doc "Extract a field value from a list of fields" $
  "cx" ~> "fname" ~> "mapping" ~> "graph" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fname"))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (unexpected (var "cx") (Phantoms.string "field " ++ (Core.unName (var "fname"))) (Phantoms.string "no matching field"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) $ Phantoms.int32 1)
      ("stripped" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ (Core.fieldTerm (Lists.head (var "matchingFields"))) $
       var "mapping" @@ var "stripped")
      (unexpected (var "cx") (Phantoms.string "single field") (Phantoms.string "multiple fields named " ++ (Core.unName (var "fname")))))

float32 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Float)
float32 = define "float32" $
  doc "Extract a 32-bit floating-point value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "f" <<~ floatLiteral @@ var "cx" @@ var "l" $
  float32Value @@ var "cx" @@ var "f"

float32Value :: TTermDefinition (Context -> FloatValue -> Prelude.Either (InContext Error) Float)
float32Value = define "float32Value" $
  doc "Extract a float32 value from a FloatValue" $
  "cx" ~> "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "float32") (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_float32>>: "f" ~> right (var "f")]

float64 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Double)
float64 = define "float64" $
  doc "Extract a 64-bit floating-point value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "f" <<~ floatLiteral @@ var "cx" @@ var "l" $
  float64Value @@ var "cx" @@ var "f"

float64Value :: TTermDefinition (Context -> FloatValue -> Prelude.Either (InContext Error) Double)
float64Value = define "float64Value" $
  doc "Extract a float64 value from a FloatValue" $
  "cx" ~> "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "float64") (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_float64>>: "f" ~> right (var "f")]

floatLiteral :: TTermDefinition (Context -> Literal -> Prelude.Either (InContext Error) FloatValue)
floatLiteral = define "floatLiteral" $
  doc "Extract a floating-point literal from a Literal value" $
  "cx" ~> "lit" ~> Phantoms.cases _Literal (var "lit")
    (Just (unexpected (var "cx") (Phantoms.string "floating-point value") (ShowCore.literal @@ var "lit"))) [
    _Literal_float>>: "v" ~> right (var "v")]

floatValue :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) FloatValue)
floatValue = define "floatValue" $
  doc "Extract a float value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  floatLiteral @@ var "cx" @@ var "l"

eitherTerm :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) x) -> (Term -> Prelude.Either (InContext Error) y) -> Graph -> Term -> Prelude.Either (InContext Error) (Either x y))
eitherTerm = define "eitherTerm" $
  doc "Extract an either value from a term, applying functions to the left and right values" $
  "cx" ~> "leftFun" ~> "rightFun" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx")
      (Phantoms.string "either value")
      (ShowCore.term @@ var "term"))) [
    _Term_either>>: "et" ~> Eithers.either_
      ("l" ~> Eithers.map (unaryFunction left) (var "leftFun" @@ var "l"))
      ("r" ~> Eithers.map (unaryFunction right) (var "rightFun" @@ var "r"))
      (var "et")]

eitherType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) EitherType)
eitherType = define "eitherType" $
  doc "Extract the left and right types from an either type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "either type") (ShowCore.type_ @@ var "typ"))) [
    _Type_either>>: "et" ~> right (var "et")]

functionType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) FunctionType)
functionType = define "functionType" $
  doc "Extract a function type from a type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "function type") (ShowCore.type_ @@ var "typ"))) [
    _Type_function>>: "ft" ~> right (var "ft")]

-- TODO: nonstandard; move me
injection :: TTermDefinition (Context -> Name -> Graph -> Term -> Prelude.Either (InContext Error) Field)
injection = define "injection" $
  doc "Extract a field from a union term" $
  "cx" ~> "expected" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "injection") (ShowCore.term @@ var "term"))) [
    _Term_union>>: "injection" ~>
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "expected"))
        (right (Core.injectionField (var "injection")))
        (unexpected (var "cx")
          (Phantoms.string "injection of type " ++ (Core.unName (var "expected")))
          (Core.unName (Core.injectionTypeName (var "injection"))))]

int16 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) I.Int16)
int16 = define "int16" $
  doc "Extract a 16-bit signed integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  int16Value @@ var "cx" @@ var "i"

int16Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) I.Int16)
int16Value = define "int16Value" $
  doc "Extract an int16 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "int16") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int16>>: "i" ~> right (var "i")]

int32 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Int)
int32 = define "int32" $
  doc "Extract a 32-bit signed integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  int32Value @@ var "cx" @@ var "i"

int32Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) Int)
int32Value = define "int32Value" $
  doc "Extract an int32 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "int32") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int32>>: "i" ~> right (var "i")]

int64 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) I.Int64)
int64 = define "int64" $
  doc "Extract a 64-bit signed integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  int64Value @@ var "cx" @@ var "i"

int64Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) I.Int64)
int64Value = define "int64Value" $
  doc "Extract an int64 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "int64") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int64>>: "i" ~> right (var "i")]

int8 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) I.Int8)
int8 = define "int8" $
  doc "Extract an 8-bit signed integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  int8Value @@ var "cx" @@ var "i"

int8Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) I.Int8)
int8Value = define "int8Value" $
  doc "Extract an int8 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "int8") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int8>>: "i" ~> right (var "i")]

integerLiteral :: TTermDefinition (Context -> Literal -> Prelude.Either (InContext Error) IntegerValue)
integerLiteral = define "integerLiteral" $
  doc "Extract an integer literal from a Literal value" $
  "cx" ~> "lit" ~> Phantoms.cases _Literal (var "lit")
    (Just (unexpected (var "cx") (Phantoms.string "integer value") (ShowCore.literal @@ var "lit"))) [
    _Literal_integer>>: "v" ~> right (var "v")]

integerValue :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) IntegerValue)
integerValue = define "integerValue" $
  doc "Extract an integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  integerLiteral @@ var "cx" @@ var "l"

lambdaBody :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Term)
lambdaBody = define "lambdaBody" $
  doc "Extract the body of a lambda term" $
  "cx" ~> "graph" ~> "term" ~> Eithers.map (unaryFunction Core.lambdaBody) (lambda @@ var "cx" @@ var "graph" @@ var "term")

lambda :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Lambda)
lambda = define "lambda" $
  doc "Extract a lambda from a term" $
  "cx" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "lambda") (ShowCore.term @@ var "term"))) [
    _Term_function>>: "function" ~> Phantoms.cases _Function (var "function")
      (Just (unexpected (var "cx") (Phantoms.string "lambda") (ShowCore.term @@ var "term"))) [
      _Function_lambda>>: "l" ~> right (var "l")]]

-- TODO: nonstandard; move me
letBinding :: TTermDefinition (Context -> String -> Graph -> Term -> Prelude.Either (InContext Error) Term)
letBinding = define "letBinding" $
  doc "Extract a binding with the given name from a let term" $
  "cx" ~> "n" ~> "graph" ~> "term" ~>
  "name" <~ Core.name (var "n") $
  "letExpr" <<~ let_ @@ var "cx" @@ var "graph" @@ var "term" $
  "matchingBindings" <~ Lists.filter
    ("b" ~> Core.equalName_ (Core.bindingName (var "b")) (var "name"))
    (Core.letBindings (var "letExpr")) $
  Logic.ifElse (Lists.null (var "matchingBindings"))
    (Ctx.failInContext (Error.errorOther $ Error.otherError (Phantoms.string "no such binding: " ++ var "n")) (var "cx"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingBindings")) $ Phantoms.int32 1)
      (right (Core.bindingTerm (Lists.head (var "matchingBindings"))))
      (Ctx.failInContext (Error.errorOther $ Error.otherError (Phantoms.string "multiple bindings named " ++ var "n")) (var "cx")))

let_ :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Let)
let_ = define "let" $
  doc "Extract a let expression from a term" $
  "cx" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "let term") (ShowCore.term @@ var "term"))) [
    _Term_let>>: "lt" ~> right (var "lt")]

list :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) [Term])
list = define "list" $
  doc "Extract a list of terms from a term" $
  "cx" ~> "graph" ~> "term" ~>
  "stripped" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term" $
  Phantoms.cases _Term (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "list") (ShowCore.term @@ var "stripped"))) [
    _Term_list>>: "l" ~> right (var "l")]

listHead :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Term)
listHead = define "listHead" $
  doc "Extract the first element of a list term" $
  "cx" ~> "graph" ~> "term" ~>
  "l" <<~ list @@ var "cx" @@ var "graph" @@ var "term" $
  Logic.ifElse (Lists.null (var "l"))
    (Ctx.failInContext (Error.errorOther $ Error.otherError (Phantoms.string "empty list")) (var "cx"))
    (right (Lists.head (var "l")))

listOf :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) x) -> Graph -> Term -> Prelude.Either (InContext Error) [x])
listOf = define "listOf" $
  doc "Extract a list of values from a term, mapping a function over each element" $
  "cx" ~> "f" ~> "graph" ~> "term" ~>
  "els" <<~ list @@ var "cx" @@ var "graph" @@ var "term" $
  Eithers.mapList (var "f") (var "els")

listType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) Type)
listType = define "listType" $
  doc "Extract the element type from a list type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "list type") (ShowCore.type_ @@ var "typ"))) [
    _Type_list>>: "t" ~> right (var "t")]

literal :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Literal)
literal = define "literal" $
  doc "Extract a literal value from a term" $
  "cx" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "literal") (ShowCore.term @@ var "term"))) [
    _Term_literal>>: "lit" ~> right (var "lit")]

map :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) k) -> (Term -> Prelude.Either (InContext Error) v) -> Graph -> Term -> Prelude.Either (InContext Error) (M.Map k v))
map = define "map" $
  doc "Extract a map of key-value pairs from a term, mapping functions over each key and value" $
  "cx" ~> "fk" ~> "fv" ~> "graph" ~> "term0" ~>
  "pair" <~ ("kvPair" ~>
    "kterm" <~ Pairs.first (var "kvPair") $
    "vterm" <~ Pairs.second (var "kvPair") $
    "kval" <<~ var "fk" @@ var "kterm" $
    "vval" <<~ var "fv" @@ var "vterm" $
    right (Phantoms.pair (var "kval") (var "vval"))) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx")
      (Phantoms.string "map")
      (ShowCore.term @@ var "term"))) [
    _Term_map>>: "m" ~> Eithers.map (unaryFunction Maps.fromList) (Eithers.mapList (var "pair") (Maps.toList (var "m")))]

mapType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) MapType)
mapType = define "mapType" $
  doc "Extract the key and value types from a map type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "map type") (ShowCore.type_ @@ var "typ"))) [
    _Type_map>>: "mt" ~> right (var "mt")]

-- TODO: nonstandard; move me
nArgs :: TTermDefinition (Context -> Name -> Int -> [a] -> Prelude.Either (InContext Error) ())
nArgs = define "nArgs" $
  doc "Ensure a function has the expected number of arguments" $
  "cx" ~> "name" ~> "n" ~> "args" ~>
  Logic.ifElse (Equality.equal (Lists.length (var "args")) (var "n"))
    (right Phantoms.unit)
    (unexpected (var "cx") (Strings.concat [
      Literals.showInt32 (var "n"),
      Phantoms.string " arguments to primitive ",
      Literals.showString (Core.unName (var "name"))]) (Literals.showInt32 (Lists.length (var "args"))))

maybeTerm :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) x) -> Graph -> Term -> Prelude.Either (InContext Error) (Maybe x))
maybeTerm = define "maybeTerm" $
  doc "Extract an optional value from a term, applying a function to the value if present" $
  "cx" ~> "f" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx")
      (Phantoms.string "maybe value")
      (ShowCore.term @@ var "term"))) [
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (right nothing)
      ("t" ~> Eithers.map (unaryFunction just) (var "f" @@ var "t"))
      (var "mt")]

maybeType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) Type)
maybeType = define "maybeType" $
  doc "Extract the base type from an optional type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "maybe type") (ShowCore.type_ @@ var "typ"))) [
    _Type_maybe>>: "t" ~> right (var "t")]

pair :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) k) -> (Term -> Prelude.Either (InContext Error) v) -> Graph -> Term -> Prelude.Either (InContext Error) (k, v))
pair = define "pair" $
  doc "Extract a pair of values from a term, applying functions to each component" $
  "cx" ~> "kf" ~> "vf" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx")
      (Phantoms.string "pair")
      (ShowCore.term @@ var "term"))) [
    _Term_pair>>: "p" ~>
      "kVal" <<~ var "kf" @@ (Pairs.first $ var "p") $
      "vVal" <<~ var "vf" @@ (Pairs.second $ var "p") $
      right (Phantoms.pair (var "kVal") (var "vVal"))]

-- TODO: nonstandard; move me
record :: TTermDefinition (Context -> Name -> Graph -> Term -> Prelude.Either (InContext Error) [Field])
record = define "record" $
  doc "Extract a record's fields from a term" $
  "cx" ~> "expected" ~> "graph" ~> "term0" ~>
  "record" <<~ termRecord @@ var "cx" @@ var "graph" @@ var "term0" $
  Logic.ifElse (Equality.equal (Core.recordTypeName (var "record")) (var "expected"))
    (right (Core.recordFields (var "record")))
    (unexpected (var "cx")
      (Phantoms.string "record of type " ++ (Core.unName (var "expected")))
      (Core.unName (Core.recordTypeName (var "record"))))

-- TODO: nonstandard; move me
recordType :: TTermDefinition (Context -> Name -> Type -> Prelude.Either (InContext Error) [FieldType])
recordType = define "recordType" $
  doc "Extract the field types from a record type" $
  "cx" ~> "ename" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "record type") (ShowCore.type_ @@ var "typ"))) [
    _Type_record>>: "fields" ~> right (var "fields")]

set :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) (S.Set Term))
set = define "set" $
  doc "Extract a set of terms from a term" $
  "cx" ~> "graph" ~> "term" ~>
  "stripped" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term" $
  Phantoms.cases _Term (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "set") (ShowCore.term @@ var "stripped"))) [
    _Term_set>>: "s" ~> right (var "s")]

setOf :: TTermDefinition (Context -> (Term -> Prelude.Either (InContext Error) x) -> Graph -> Term -> Prelude.Either (InContext Error) (S.Set x))
setOf = define "setOf" $
  doc "Extract a set of values from a term, mapping a function over each element" $
  "cx" ~> "f" ~> "graph" ~> "term" ~>
  "els" <<~ set @@ var "cx" @@ var "graph" @@ var "term" $
  Eithers.mapSet (var "f") (var "els")

setType :: TTermDefinition (Context -> Type -> Prelude.Either (InContext Error) Type)
setType = define "setType" $
  doc "Extract the element type from a set type" $
  "cx" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "set type") (ShowCore.type_ @@ var "typ"))) [
    _Type_set>>: "t" ~> right (var "t")]

string :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) String)
string = define "string" $
  doc "Extract a string value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  stringLiteral @@ var "cx" @@ var "l"

stringLiteral :: TTermDefinition (Context -> Literal -> Prelude.Either (InContext Error) String)
stringLiteral = define "stringLiteral" $
  doc "Extract a string literal from a Literal value" $
  "cx" ~> "v" ~> Phantoms.cases _Literal (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "string") (ShowCore.literal @@ var "v"))) [
    _Literal_string>>: "s" ~> right (var "s")]

termRecord :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Record)
termRecord = define "termRecord" $
  doc "Extract a record from a term" $
  "cx" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "record") (ShowCore.term @@ var "term"))) [
    _Term_record>>: "record" ~> right (var "record")]

uint16 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Int)
uint16 = define "uint16" $
  doc "Extract a 16-bit unsigned integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  uint16Value @@ var "cx" @@ var "i"

uint16Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) Int)
uint16Value = define "uint16Value" $
  doc "Extract a uint16 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "uint16") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint16>>: "i" ~> right (var "i")]

uint32 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) I.Int64)
uint32 = define "uint32" $
  doc "Extract a 32-bit unsigned integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  uint32Value @@ var "cx" @@ var "i"

uint32Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) I.Int64)
uint32Value = define "uint32Value" $
  doc "Extract a uint32 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "uint32") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint32>>: "i" ~> right (var "i")]

uint64 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) Integer)
uint64 = define "uint64" $
  doc "Extract a 64-bit unsigned integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  uint64Value @@ var "cx" @@ var "i"

uint64Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) Integer)
uint64Value = define "uint64Value" $
  doc "Extract a uint64 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "uint64") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint64>>: "i" ~> right (var "i")]

uint8 :: TTermDefinition (Context -> Graph -> Term -> Prelude.Either (InContext Error) I.Int16)
uint8 = define "uint8" $
  doc "Extract an 8-bit unsigned integer value from a term" $
  "cx" ~> "graph" ~> "t" ~>
  "l" <<~ literal @@ var "cx" @@ var "graph" @@ var "t" $
  "i" <<~ integerLiteral @@ var "cx" @@ var "l" $
  uint8Value @@ var "cx" @@ var "i"

uint8Value :: TTermDefinition (Context -> IntegerValue -> Prelude.Either (InContext Error) I.Int16)
uint8Value = define "uint8Value" $
  doc "Extract a uint8 value from an IntegerValue" $
  "cx" ~> "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (unexpected (var "cx") (Phantoms.string "uint8") (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint8>>: "i" ~> right (var "i")]

-- TODO: nonstandard; move me
unionType :: TTermDefinition (Context -> Name -> Type -> Prelude.Either (InContext Error) [FieldType])
unionType = define "unionType" $
  doc "Extract the field types from a union type" $
  "cx" ~> "ename" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "union type") (ShowCore.type_ @@ var "typ"))) [
    _Type_union>>: "fields" ~> right (var "fields")]

unit :: TTermDefinition (Context -> Term -> Prelude.Either (InContext Error) ())
unit = define "unit" $
  doc "Extract a unit value from a term" $
  "cx" ~> "term" ~> Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx") (Phantoms.string "unit") (ShowCore.term @@ var "term"))) [
    _Term_unit>>: constant (right Phantoms.unit)]

unitVariant :: TTermDefinition (Context -> Name -> Graph -> Term -> Prelude.Either (InContext Error) Name)
unitVariant = define "unitVariant" $
  doc "Extract a unit variant (a variant with an empty record value) from a union term" $
  "cx" ~> "tname" ~> "graph" ~> "term" ~>
  "field" <<~ injection @@ var "cx" @@ var "tname" @@ var "graph" @@ var "term" $
  "ignored" <<~ unit @@ var "cx" @@ (Core.fieldTerm (var "field")) $
  right (Core.fieldName (var "field"))

-- TODO: nonstandard; move me
wrap :: TTermDefinition (Context -> Name -> Graph -> Term -> Prelude.Either (InContext Error) Term)
wrap = define "wrap" $
  doc "Extract the wrapped value from a wrapped term" $
  "cx" ~> "expected" ~> "graph" ~> "term0" ~>
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "cx" @@ var "graph" @@ var "term0" $
  Phantoms.cases _Term (var "term")
    (Just (unexpected (var "cx")
      (Phantoms.string "wrap(" ++ (Core.unName (var "expected")) ++ Phantoms.string ")")
      (ShowCore.term @@ var "term"))) [
    _Term_wrap>>: "wrappedTerm" ~>
      Logic.ifElse (Core.equalName_ (Core.wrappedTermTypeName (var "wrappedTerm")) (var "expected"))
        (right (Core.wrappedTermBody (var "wrappedTerm")))
        (unexpected (var "cx")
          (Phantoms.string "wrapper of type " ++ (Core.unName (var "expected")))
          (Core.unName (Core.wrappedTermTypeName (var "wrappedTerm"))))]

-- TODO: nonstandard; move me
wrappedType :: TTermDefinition (Context -> Name -> Type -> Prelude.Either (InContext Error) Type)
wrappedType = define "wrappedType" $
  doc "Extract the wrapped type from a wrapper type" $
  "cx" ~> "ename" ~> "typ" ~>
  "stripped" <~ Strip.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (unexpected (var "cx") (Phantoms.string "wrapped type") (ShowCore.type_ @@ var "typ"))) [
    _Type_wrap>>: "innerType" ~> right (var "innerType")]

--------------------------------------------------------------------------------
-- Helper functions (merged from hydra.extract.helpers)
--------------------------------------------------------------------------------

-- | Helper to convert Either String Term to Either DecodingError Term
stripWithDecodingError :: TTerm Graph -> TTerm Term -> TTerm (Either DecodingError Term)
stripWithDecodingError g term = Eithers.bimap
  (unaryFunction Error.decodingError)
  ("x" ~> var "x")
  (Lexical.stripAndDereferenceTermEither @@ g @@ term)

-- | Decode an Either value using the provided left and right decoders
decodeEither :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> (Graph -> Term -> Either DecodingError b) -> Graph -> Term -> Either DecodingError (Either a b))
decodeEither = define "decodeEither" $
  doc "Decode an Either value using the provided left and right decoders" $
  "leftDecoder" ~> "rightDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected either value") [
      _Term_either>>: "e" ~>
        Eithers.either_
          -- Left case: decode the left value, then wrap result in Left
          ("lv" ~> Eithers.map ("x" ~> left (var "x")) (var "leftDecoder" @@ var "g" @@ var "lv"))
          -- Right case: decode the right value, then wrap result in Right
          ("rv" ~> Eithers.map ("x" ~> right (var "x")) (var "rightDecoder" @@ var "g" @@ var "rv"))
          (var "e")])

-- | Decode a list of elements using the provided element decoder
decodeList :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError [a])
decodeList = define "decodeList" $
  doc "Decode a list of elements using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected list") [
      _Term_list>>: "els" ~> Eithers.mapList (var "elemDecoder" @@ var "g") $ var "els"])

-- | Decode a Map using the provided key and value decoders
decodeMap :: TTermDefinition ((Graph -> Term -> Either DecodingError k) -> (Graph -> Term -> Either DecodingError v) -> Graph -> Term -> Either DecodingError (M.Map k v))
decodeMap = define "decodeMap" $
  doc "Decode a Map using the provided key and value decoders" $
  "keyDecoder" ~> "valDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected map") [
      _Term_map>>: "m" ~>
        Eithers.map (unaryFunction Maps.fromList)
          (Eithers.mapList
            ("kv" ~>
              Eithers.bind (var "keyDecoder" @@ var "g" @@ (Pairs.first $ var "kv"))
                ("k" ~> Eithers.map ("v" ~> Phantoms.pair (var "k") (var "v"))
                  (var "valDecoder" @@ var "g" @@ (Pairs.second $ var "kv"))))
            (Maps.toList $ var "m"))])

-- | Decode a Maybe value using the provided element decoder
decodeMaybe :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError (Maybe a))
decodeMaybe = define "decodeMaybe" $
  doc "Decode a Maybe value using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected optional value") [
      _Term_maybe>>: "opt" ~> Eithers.mapMaybe (var "elemDecoder" @@ var "g") $ var "opt"])

-- | Decode a Pair using the provided first and second decoders
decodePair :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> (Graph -> Term -> Either DecodingError b) -> Graph -> Term -> Either DecodingError (a, b))
decodePair = define "decodePair" $
  doc "Decode a Pair using the provided first and second decoders" $
  "firstDecoder" ~> "secondDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected pair") [
      _Term_pair>>: "p" ~>
        Eithers.bind (var "firstDecoder" @@ var "g" @@ (Pairs.first $ var "p"))
          ("f" ~> Eithers.map ("s" ~> Phantoms.pair (var "f") (var "s"))
            (var "secondDecoder" @@ var "g" @@ (Pairs.second $ var "p")))])

-- | Decode a Set using the provided element decoder
decodeSet :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError (S.Set a))
decodeSet = define "decodeSet" $
  doc "Decode a Set using the provided element decoder" $
  "elemDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected set") [
      _Term_set>>: "s" ~>
        Eithers.map (unaryFunction Sets.fromList)
          (Eithers.mapList (var "elemDecoder" @@ var "g") (Sets.toList $ var "s"))])

-- | Decode a unit value
decodeUnit :: TTermDefinition (Graph -> Term -> Either DecodingError ())
decodeUnit = define "decodeUnit" $
  doc "Decode a unit value" $
  "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected a unit value") [
      _Term_unit>>: constant $ right Phantoms.unit])

-- | Decode a wrapped value using the provided body decoder
decodeWrapped :: TTermDefinition ((Graph -> Term -> Either DecodingError a) -> Graph -> Term -> Either DecodingError a)
decodeWrapped = define "decodeWrapped" $
  doc "Decode a wrapped value using the provided body decoder" $
  "bodyDecoder" ~> "g" ~> "term" ~>
  Eithers.bind
    (stripWithDecodingError (var "g") (var "term"))
    ("stripped" ~> Phantoms.cases _Term (var "stripped")
      (Just $ left $ Error.decodingError $ Phantoms.string "expected wrapped value") [
      _Term_wrap>>: "wt" ~>
        var "bodyDecoder" @@ var "g" @@ (Core.wrappedTermBody (var "wt"))])

-- | Require a field from a field map and decode it using the provided decoder
-- Returns Left with a "missing field" error if the field is not present
requireField :: TTermDefinition (String -> (Graph -> Term -> Either DecodingError a) -> M.Map Name Term -> Graph -> Either DecodingError a)
requireField = define "requireField" $
  doc "Require a field from a record's field map and decode it" $
  "fieldName" ~> "decoder" ~> "fieldMap" ~> "g" ~>
  Maybes.maybe
    (left $ Error.decodingError $ Strings.cat $ Phantoms.list [Phantoms.string "missing field ", var "fieldName", Phantoms.string " in record"])
    ("fieldTerm" ~> var "decoder" @@ var "g" @@ var "fieldTerm")
    (Maps.lookup (Phantoms.wrap _Name $ var "fieldName") $ var "fieldMap")

-- | Convert a Record to a Map from field Name to Term
toFieldMap :: TTermDefinition (Record -> M.Map Name Term)
toFieldMap = define "toFieldMap" $
  doc "Convert a Record's fields to a Map from Name to Term" $
  "record" ~>
  Maps.fromList $
    Lists.map
      ("f" ~> Phantoms.pair (Core.fieldName $ var "f") (Core.fieldTerm $ var "f"))
      (Core.recordFields $ var "record")
