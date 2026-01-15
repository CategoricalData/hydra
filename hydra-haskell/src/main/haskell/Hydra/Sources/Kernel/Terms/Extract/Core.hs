module Hydra.Sources.Kernel.Terms.Extract.Core where

-- Standard imports for kernel terms modules (slightly modified for conflict avoidance)
import Hydra.Kernel hiding (lambdaBody, map, setType)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms hiding (
  bigfloat, bigint, binary, boolean, cases, field, float32, float64, floatValue, injection, int8, int16, int32, int64,
  integerValue, lambda, list, literal, map, pair, set, record, string, unit, wrap, uint8, uint16, uint32, uint64)
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), map)
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.extract.core"

module_ :: Module
module_ = Module ns elements
    [Monads.ns, Lexical.ns, Rewriting.ns, ShowCore.ns]
    kernelTypesNamespaces $
    Just ("Extraction and validation for hydra.core types")
  where
   elements = [
     toBinding bigfloat,
     toBinding bigfloatValue,
     toBinding bigint,
     toBinding bigintValue,
     toBinding binary,
     toBinding binaryLiteral,
     toBinding boolean,
     toBinding booleanLiteral,
     toBinding caseField,
     toBinding cases,
     toBinding field,
     toBinding float32,
     toBinding float32Value,
     toBinding float64,
     toBinding float64Value,
     toBinding floatLiteral,
     toBinding floatValue,
     toBinding eitherTerm,
     toBinding eitherType,
     toBinding functionType,
     toBinding injection,
     toBinding int16,
     toBinding int16Value,
     toBinding int32,
     toBinding int32Value,
     toBinding int64,
     toBinding int64Value,
     toBinding int8,
     toBinding int8Value,
     toBinding integerLiteral,
     toBinding integerValue,
     toBinding lambdaBody,
     toBinding lambda,
     toBinding letBinding,
     toBinding let_,
     toBinding list,
     toBinding listHead,
     toBinding listOf,
     toBinding listType,
     toBinding literal,
     toBinding map,
     toBinding mapType,
     toBinding nArgs,
     toBinding maybeTerm,
     toBinding maybeType,
     toBinding pair,
     toBinding record,
     toBinding recordType,
     toBinding set,
     toBinding setOf,
     toBinding setType,
     toBinding string,
     toBinding stringLiteral,
     toBinding termRecord,
     toBinding uint16,
     toBinding uint16Value,
     toBinding uint32,
     toBinding uint32Value,
     toBinding uint64,
     toBinding uint64Value,
     toBinding uint8,
     toBinding uint8Value,
     toBinding unionType,
     toBinding unit,
     toBinding unitVariant,
     toBinding wrap,
     toBinding wrappedType]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bigfloat :: TBinding (Term -> Flow Graph Double)
bigfloat = define "bigfloat" $
  doc "Extract an arbitrary-precision floating-point value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "f" <<~ floatLiteral @@ var "l" $
  bigfloatValue @@ var "f"

bigfloatValue :: TBinding (FloatValue -> Flow Graph Double)
bigfloatValue = define "bigfloatValue" $
  doc "Extract a bigfloat value from a FloatValue" $
  "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (Monads.unexpected
      @@ ("bigfloat")
      @@ (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_bigfloat>>: "f" ~> Flows.pure (var "f")]

bigint :: TBinding (Term -> Flow Graph Integer)
bigint = define "bigint" $
  doc "Extract an arbitrary-precision integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  bigintValue @@ var "i"

bigintValue :: TBinding (IntegerValue -> Flow Graph Integer)
bigintValue = define "bigintValue" $
  doc "Extract a bigint value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("bigint")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_bigint>>: "i" ~> Flows.pure (var "i")]

binary :: TBinding (Term -> Flow Graph String)
binary = define "binary" $
  doc "Extract a binary data value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  binaryLiteral @@ var "l"

binaryLiteral :: TBinding (Literal -> Flow Graph String)
binaryLiteral = define "binaryLiteral" $
  doc "Extract a binary literal from a Literal value" $
  "v" ~> Phantoms.cases _Literal (var "v")
    (Just (Monads.unexpected
      @@ ("binary")
      @@ (ShowCore.literal @@ var "v"))) [
    _Literal_binary>>: "b" ~> Flows.pure (var "b")]

boolean :: TBinding (Term -> Flow Graph Bool)
boolean = define "boolean" $
  doc "Extract a boolean value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  booleanLiteral @@ var "l"

booleanLiteral :: TBinding (Literal -> Flow Graph Bool)
booleanLiteral = define "booleanLiteral" $
  doc "Extract a boolean literal from a Literal value" $
  "v" ~> Phantoms.cases _Literal (var "v")
    (Just (Monads.unexpected
      @@ ("boolean")
      @@ (ShowCore.literal @@ var "v"))) [
    _Literal_boolean>>: "b" ~> Flows.pure (var "b")]

-- TODO: nonstandard; move me
caseField :: TBinding (Name -> String -> Term -> Flow Graph Field)
caseField = define "caseField" $
  doc "Extract a specific case handler from a case statement term" $
  "name" ~> "n" ~> "term" ~>
  "fieldName" <~ Core.name (var "n") $
  "cs" <<~ cases @@ var "name" @@ var "term" $
  "matching" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fieldName"))
    (Core.caseStatementCases (var "cs")) $
  Logic.ifElse (Lists.null (var "matching"))
    (Flows.fail ("not enough cases"))
    (Flows.pure (Lists.head (var "matching")))

-- TODO: nonstandard; move me
cases :: TBinding (Name -> Term -> Flow Graph CaseStatement)
cases = define "cases" $
  doc "Extract case statement from a term" $
  "name" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("case statement")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_function>>: "function" ~> Phantoms.cases _Function (var "function")
      (Just (Monads.unexpected
        @@ ("case statement")
        @@ (ShowCore.term @@ var "term"))) [
      _Function_elimination>>: "elimination" ~> Phantoms.cases _Elimination (var "elimination")
        (Just (Monads.unexpected
          @@ ("case statement")
          @@ (ShowCore.term @@ var "term"))) [
        _Elimination_union>>: "cs" ~>
          Logic.ifElse (Core.equalName_ (Core.caseStatementTypeName (var "cs")) (var "name"))
            (Flows.pure (var "cs"))
            (Monads.unexpected
              @@ (("case statement for type ") ++ (Core.unName (var "name")))
              @@ (ShowCore.term @@ var "term"))]]]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
field :: TBinding (Name -> (Term -> Flow Graph x) -> [Field] -> Flow Graph x)
field = define "field" $
  doc "Extract a field value from a list of fields" $
  "fname" ~> "mapping" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fname"))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (Flows.fail (("field ") ++ (Core.unName (var "fname")) ++ (" not found")))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) $ Phantoms.int32 1)
      ("stripped" <<~ Lexical.stripAndDereferenceTerm @@ (Core.fieldTerm (Lists.head (var "matchingFields"))) $
       var "mapping" @@ var "stripped")
      (Flows.fail (("multiple fields named ") ++ (Core.unName (var "fname")))))

float32 :: TBinding (Term -> Flow Graph Float)
float32 = define "float32" $
  doc "Extract a 32-bit floating-point value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "f" <<~ floatLiteral @@ var "l" $
  float32Value @@ var "f"

float32Value :: TBinding (FloatValue -> Flow Graph Float)
float32Value = define "float32Value" $
  doc "Extract a float32 value from a FloatValue" $
  "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (Monads.unexpected
      @@ ("float32")
      @@ (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_float32>>: "f" ~> Flows.pure (var "f")]

float64 :: TBinding (Term -> Flow Graph Double)
float64 = define "float64" $
  doc "Extract a 64-bit floating-point value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "f" <<~ floatLiteral @@ var "l" $
  float64Value @@ var "f"

float64Value :: TBinding (FloatValue -> Flow Graph Double)
float64Value = define "float64Value" $
  doc "Extract a float64 value from a FloatValue" $
  "v" ~> Phantoms.cases _FloatValue (var "v")
    (Just (Monads.unexpected
      @@ ("float64")
      @@ (ShowCore.floatValue @@ var "v"))) [
    _FloatValue_float64>>: "f" ~> Flows.pure (var "f")]

floatLiteral :: TBinding (Literal -> Flow Graph FloatValue)
floatLiteral = define "floatLiteral" $
  doc "Extract a floating-point literal from a Literal value" $
  "lit" ~> Phantoms.cases _Literal (var "lit")
    (Just (Monads.unexpected
      @@ ("floating-point value")
      @@ (ShowCore.literal @@ var "lit"))) [
    _Literal_float>>: "v" ~> Flows.pure (var "v")]

floatValue :: TBinding (Term -> Flow Graph FloatValue)
floatValue = define "floatValue" $
  doc "Extract a float value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  floatLiteral @@ var "l"

eitherTerm :: TBinding ((Term -> Flow Graph x) -> (Term -> Flow Graph y) -> Term -> Flow Graph (Either x y))
eitherTerm = define "eitherTerm" $
  doc "Extract an either value from a term, applying functions to the left and right values" $
  "leftFun" ~> "rightFun" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("either value")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_either>>: "et" ~> Eithers.either_
      ("l" ~> Flows.map (unaryFunction left) (var "leftFun" @@ var "l"))
      ("r" ~> Flows.map (unaryFunction right) (var "rightFun" @@ var "r"))
      (var "et")]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

eitherType :: TBinding (Type -> Flow s EitherType)
eitherType = define "eitherType" $
  doc "Extract the left and right types from an either type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("either type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_either>>: "et" ~> Flows.pure (var "et")]

functionType :: TBinding (Type -> Flow s FunctionType)
functionType = define "functionType" $
  doc "Extract a function type from a type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("function type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_function>>: "ft" ~> Flows.pure (var "ft")]

-- TODO: nonstandard; move me
injection :: TBinding (Name -> Term -> Flow Graph Field)
injection = define "injection" $
  doc "Extract a field from a union term" $
  "expected" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("injection")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_union>>: "injection" ~>
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "expected"))
        (Flows.pure (Core.injectionField (var "injection")))
        (Monads.unexpected
          @@ (("injection of type ") ++ (Core.unName (var "expected")))
          @@ (Core.unName (Core.injectionTypeName (var "injection"))))]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

int16 :: TBinding (Term -> Flow Graph I.Int16)
int16 = define "int16" $
  doc "Extract a 16-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  int16Value @@ var "i"

int16Value :: TBinding (IntegerValue -> Flow Graph I.Int16)
int16Value = define "int16Value" $
  doc "Extract an int16 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("int16")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int16>>: "i" ~> Flows.pure (var "i")]

int32 :: TBinding (Term -> Flow Graph Int)
int32 = define "int32" $
  doc "Extract a 32-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  int32Value @@ var "i"

int32Value :: TBinding (IntegerValue -> Flow Graph Int)
int32Value = define "int32Value" $
  doc "Extract an int32 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ "int32"
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int32>>: "i" ~> Flows.pure (var "i")]

int64 :: TBinding (Term -> Flow Graph I.Int64)
int64 = define "int64" $
  doc "Extract a 64-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  int64Value @@ var "i"

int64Value :: TBinding (IntegerValue -> Flow Graph I.Int64)
int64Value = define "int64Value" $
  doc "Extract an int64 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("int64")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int64>>: "i" ~> Flows.pure (var "i")]

int8 :: TBinding (Term -> Flow Graph I.Int8)
int8 = define "int8" $
  doc "Extract an 8-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  int8Value @@ var "i"

int8Value :: TBinding (IntegerValue -> Flow Graph I.Int8)
int8Value = define "int8Value" $
  doc "Extract an int8 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("int8")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_int8>>: "i" ~> Flows.pure (var "i")]

integerLiteral :: TBinding (Literal -> Flow Graph IntegerValue)
integerLiteral = define "integerLiteral" $
  doc "Extract an integer literal from a Literal value" $
  "lit" ~> Phantoms.cases _Literal (var "lit")
    (Just (Monads.unexpected
      @@ ("integer value")
      @@ (ShowCore.literal @@ var "lit"))) [
    _Literal_integer>>: "v" ~> Flows.pure (var "v")]

integerValue :: TBinding (Term -> Flow Graph IntegerValue)
integerValue = define "integerValue" $
  doc "Extract an integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  integerLiteral @@ var "l"

lambdaBody :: TBinding (Term -> Flow Graph Term)
lambdaBody = define "lambdaBody" $
  doc "Extract the body of a lambda term" $
  "term" ~> Flows.map (unaryFunction Core.lambdaBody) (lambda @@ var "term")

lambda :: TBinding (Term -> Flow Graph Lambda)
lambda = define "lambda" $
  doc "Extract a lambda from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ "lambda"
      @@ (ShowCore.term @@ var "term"))) [
    _Term_function>>: "function" ~> Phantoms.cases _Function (var "function")
      (Just (Monads.unexpected
        @@ ("lambda")
        @@ (ShowCore.term @@ var "term"))) [
      _Function_lambda>>: "l" ~> Flows.pure (var "l")]]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
letBinding :: TBinding (String -> Term -> Flow Graph Term)
letBinding = define "letBinding" $
  doc "Extract a binding with the given name from a let term" $
  "n" ~> "term" ~>
  "name" <~ Core.name (var "n") $
  "letExpr" <<~ let_ @@ var "term" $
  "matchingBindings" <~ Lists.filter
    ("b" ~> Core.equalName_ (Core.bindingName (var "b")) (var "name"))
    (Core.letBindings (var "letExpr")) $
  Logic.ifElse (Lists.null (var "matchingBindings"))
    (Flows.fail (("no such binding: ") ++ var "n"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingBindings")) $ Phantoms.int32 1)
      (Flows.pure (Core.bindingTerm (Lists.head (var "matchingBindings"))))
      (Flows.fail (("multiple bindings named ") ++ var "n")))

let_ :: TBinding (Term -> Flow Graph Let)
let_ = define "let" $
  doc "Extract a let expression from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("let term")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_let>>: "lt" ~> Flows.pure (var "lt")]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

list :: TBinding (Term -> Flow Graph [Term])
list = define "list" $
  doc "Extract a list of terms from a term" $
  "term" ~>
  "extract" <~ ("stripped" ~> Phantoms.cases _Term (var "stripped")
    (Just (Monads.unexpected
      @@ "list"
      @@ (ShowCore.term @@ var "stripped"))) [
    _Term_list>>: "l" ~> produce (var "l")]) $
  "stripped" <<~ Lexical.stripAndDereferenceTerm @@ var "term" $
  var "extract" @@ var "stripped"

listHead :: TBinding (Term -> Flow Graph Term)
listHead = define "listHead" $
  doc "Extract the first element of a list term" $
  "term" ~>
  "l" <<~ list @@ var "term" $
  Logic.ifElse (Lists.null (var "l"))
    (Flows.fail "empty list")
    (Flows.pure (Lists.head (var "l")))

listOf :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph [x])
listOf = define "listOf" $
  doc "Extract a list of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ list @@ var "term" $
  Flows.mapList (var "f") (var "els")

listType :: TBinding (Type -> Flow s Type)
listType = define "listType" $
  doc "Extract the element type from a list type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("list type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_list>>: "t" ~> Flows.pure (var "t")]

literal :: TBinding (Term -> Flow Graph Literal)
literal = define "literal" $
  doc "Extract a literal value from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("literal")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_literal>>: "lit" ~> Flows.pure (var "lit")]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

map :: TBinding ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (M.Map k v))
map = define "map" $
  doc "Extract a map of key-value pairs from a term, mapping functions over each key and value" $
  "fk" ~> "fv" ~> "term0" ~>
  "pair" <~ ("kvPair" ~>
    "kterm" <~ Pairs.first (var "kvPair") $
    "vterm" <~ Pairs.second (var "kvPair") $
    "kval" <<~ var "fk" @@ var "kterm" $
    "vval" <<~ var "fv" @@ var "vterm" $
    produce (Phantoms.pair (var "kval") (var "vval"))) $
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ "map"
      @@ (ShowCore.term @@ var "term"))) [
    _Term_map>>: "m" ~> Flows.map (unaryFunction Maps.fromList) (Flows.mapList (var "pair") (Maps.toList (var "m")))]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

mapType :: TBinding (Type -> Flow s MapType)
mapType = define "mapType" $
  doc "Extract the key and value types from a map type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("map type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_map>>: "mt" ~> Flows.pure (var "mt")]

-- TODO: nonstandard; move me
nArgs :: TBinding (Name -> Int -> [a] -> Flow s ())
nArgs = define "nArgs" $
  doc "Ensure a function has the expected number of arguments" $
  "name" ~> "n" ~> "args" ~>
  Logic.ifElse (Equality.equal (Lists.length (var "args")) (var "n"))
    (produce Phantoms.unit)
    (Monads.unexpected @@ (Strings.concat [
      Literals.showInt32 (var "n"),
      Phantoms.string " arguments to primitive ",
      Literals.showString (Core.unName (var "name"))]) @@ (Literals.showInt32 (Lists.length (var "args"))))

maybeTerm :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph (Maybe x))
maybeTerm = define "maybeTerm" $
  doc "Extract an optional value from a term, applying a function to the value if present" $
  "f" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("maybe value")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (produce nothing)
      ("t" ~> Flows.map (unaryFunction just) (var "f" @@ var "t"))
      (var "mt")]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

maybeType :: TBinding (Type -> Flow s Type)
maybeType = define "maybeType" $
  doc "Extract the base type from an optional type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("maybe type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_maybe>>: "t" ~> Flows.pure (var "t")]

pair :: TBinding ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (k, v))
pair = define "pair" $
  doc "Extract a pair of values from a term, applying functions to each component" $
  "kf" ~> "vf" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ "pair"
      @@ (ShowCore.term @@ var "term"))) [
    _Term_pair>>: "p" ~>
      "kVal" <<~ var "kf" @@ (Pairs.first $ var "p") $
      "vVal" <<~ var "vf" @@ (Pairs.second $ var "p") $
      produce (Phantoms.pair (var "kVal") (var "vVal"))]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
record :: TBinding (Name -> Term -> Flow Graph [Field])
record = define "record" $
  doc "Extract a record's fields from a term" $
  "expected" ~> "term0" ~>
  "record" <<~ termRecord @@ var "term0" $
  Logic.ifElse (Equality.equal (Core.recordTypeName (var "record")) (var "expected"))
    (Flows.pure (Core.recordFields (var "record")))
    (Monads.unexpected
      @@ (("record of type ") ++ (Core.unName (var "expected")))
      @@ (Core.unName (Core.recordTypeName (var "record"))))

-- TODO: nonstandard; move me
recordType :: TBinding (Name -> Type -> Flow s [FieldType])
recordType = define "recordType" $
  doc "Extract the field types from a record type" $
  "ename" ~> "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("record type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_record>>: "rowType" ~>
      Logic.ifElse (Core.equalName_ (Core.rowTypeTypeName (var "rowType")) (var "ename"))
        (Flows.pure (Core.rowTypeFields (var "rowType")))
        (Monads.unexpected
          @@ (("record of type ") ++ (Core.unName (var "ename")))
          @@ (("record of type ") ++ (Core.unName (Core.rowTypeTypeName (var "rowType")))))]

set :: TBinding (Term -> Flow Graph (S.Set Term))
set = define "set" $
  doc "Extract a set of terms from a term" $
  "term" ~>
  "extract" <~ ("stripped" ~> Phantoms.cases _Term (var "stripped")
    (Just $ Monads.unexpected
      @@ "set"
      @@ (ShowCore.term @@ var "stripped")) [
    _Term_set>>: "s" ~> produce $ var "s"]) $
  "stripped" <<~ Lexical.stripAndDereferenceTerm @@ var "term" $
  var "extract" @@ var "stripped"

setOf :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph (S.Set x))
setOf = define "setOf" $
  doc "Extract a set of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ set @@ var "term" $
  Flows.mapSet (var "f") (var "els")

setType :: TBinding (Type -> Flow s Type)
setType = define "setType" $
  doc "Extract the element type from a set type" $
  "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ "set type"
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_set>>: "t" ~> Flows.pure (var "t")]

string :: TBinding (Term -> Flow Graph String)
string = define "string" $
  doc "Extract a string value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  stringLiteral @@ var "l"

stringLiteral :: TBinding (Literal -> Flow Graph String)
stringLiteral = define "stringLiteral" $
  doc "Extract a string literal from a Literal value" $
  "v" ~> Phantoms.cases _Literal (var "v")
    (Just (Monads.unexpected
      @@ ("string")
      @@ (ShowCore.literal @@ var "v"))) [
    _Literal_string>>: "s" ~> Flows.pure (var "s")]

termRecord :: TBinding (Term -> Flow Graph Record)
termRecord = define "termRecord" $
  doc "Extract a record from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ ("record")
      @@ (ShowCore.term @@ var "term"))) [
    _Term_record>>: "record" ~> produce (var "record")]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

uint16 :: TBinding (Term -> Flow Graph Int)
uint16 = define "uint16" $
  doc "Extract a 16-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  uint16Value @@ var "i"

uint16Value :: TBinding (IntegerValue -> Flow Graph Int)
uint16Value = define "uint16Value" $
  doc "Extract a uint16 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("uint16")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint16>>: "i" ~> Flows.pure (var "i")]

uint32 :: TBinding (Term -> Flow Graph I.Int64)
uint32 = define "uint32" $
  doc "Extract a 32-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  uint32Value @@ var "i"

uint32Value :: TBinding (IntegerValue -> Flow Graph I.Int64)
uint32Value = define "uint32Value" $
  doc "Extract a uint32 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("uint32")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint32>>: "i" ~> Flows.pure (var "i")]

uint64 :: TBinding (Term -> Flow Graph Integer)
uint64 = define "uint64" $
  doc "Extract a 64-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  uint64Value @@ var "i"

uint64Value :: TBinding (IntegerValue -> Flow Graph Integer)
uint64Value = define "uint64Value" $
  doc "Extract a uint64 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("uint64")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint64>>: "i" ~> Flows.pure (var "i")]

uint8 :: TBinding (Term -> Flow Graph I.Int16)
uint8 = define "uint8" $
  doc "Extract an 8-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ literal @@ var "t" $
  "i" <<~ integerLiteral @@ var "l" $
  uint8Value @@ var "i"

uint8Value :: TBinding (IntegerValue -> Flow Graph I.Int16)
uint8Value = define "uint8Value" $
  doc "Extract a uint8 value from an IntegerValue" $
  "v" ~> Phantoms.cases _IntegerValue (var "v")
    (Just (Monads.unexpected
      @@ ("uint8")
      @@ (ShowCore.integerValue @@ var "v"))) [
    _IntegerValue_uint8>>: "i" ~> Flows.pure (var "i")]

-- TODO: nonstandard; move me
unionType :: TBinding (Name -> Type -> Flow s [FieldType])
unionType = define "unionType" $
  doc "Extract the field types from a union type" $
  "ename" ~> "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("union type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_union>>: "rowType" ~>
      Logic.ifElse (Equality.equal (Core.rowTypeTypeName (var "rowType")) (var "ename"))
        (Flows.pure (Core.rowTypeFields (var "rowType")))
        (Monads.unexpected
          @@ (("union of type ") ++ (Core.unName (var "ename")))
          @@ (("union of type ") ++ (Core.unName (Core.rowTypeTypeName (var "rowType")))))]

unit :: TBinding (Term -> Flow Graph ())
unit = define "unit" $
  doc "Extract a unit value from a term" $
  "term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ "unit"
      @@ (ShowCore.term @@ var "term"))) [
    _Term_unit>>: constant (Flows.pure Phantoms.unit)]

unitVariant :: TBinding (Name -> Term -> Flow Graph Name)
unitVariant = define "unitVariant" $
  doc "Extract a unit variant (a variant with an empty record value) from a union term" $
  "tname" ~> "term" ~>
  "field" <<~ injection @@ var "tname" @@ var "term" $
  "ignored" <<~ unit @@ (Core.fieldTerm (var "field")) $
  produce (Core.fieldName (var "field"))

-- TODO: nonstandard; move me
wrap :: TBinding (Name -> Term -> Flow Graph Term)
wrap = define "wrap" $
  doc "Extract the wrapped value from a wrapped term" $
  "expected" ~> "term0" ~>
  "extract" <~ ("term" ~> Phantoms.cases _Term (var "term")
    (Just (Monads.unexpected
      @@ (("wrap(") ++ (Core.unName (var "expected")) ++ (")"))
      @@ (ShowCore.term @@ var "term"))) [
    _Term_wrap>>: "wrappedTerm" ~>
      Logic.ifElse (Core.equalName_ (Core.wrappedTermTypeName (var "wrappedTerm")) (var "expected"))
        (Flows.pure (Core.wrappedTermBody (var "wrappedTerm")))
        (Monads.unexpected
          @@ (("wrapper of type ") ++ (Core.unName (var "expected")))
          @@ (Core.unName (Core.wrappedTermTypeName (var "wrappedTerm"))))]) $
  "term" <<~ Lexical.stripAndDereferenceTerm @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
wrappedType :: TBinding (Name -> Type -> Flow s Type)
wrappedType = define "wrappedType" $
  doc "Extract the wrapped type from a wrapper type" $
  "ename" ~> "typ" ~>
  "stripped" <~ Rewriting.deannotateType @@ var "typ" $
  Phantoms.cases _Type (var "stripped")
    (Just (Monads.unexpected
      @@ ("wrapped type")
      @@ (ShowCore.type_ @@ var "typ"))) [
    _Type_wrap>>: "wrappedType" ~>
      Logic.ifElse (Core.equalName_ (Core.wrappedTypeTypeName (var "wrappedType")) (var "ename"))
        (Flows.pure (Core.wrappedTypeBody (var "wrappedType")))
        (Monads.unexpected
          @@ (("wrapped type ") ++ (Core.unName (var "ename")))
          @@ (("wrapped type ") ++ (Core.unName (Core.wrappedTypeTypeName (var "wrappedType")))))]
