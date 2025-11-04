{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Extract.Core where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Lexical as Lexical
import qualified Hydra.Sources.Kernel.Terms.Monads as Monads
import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


module_ :: Module
module_ = Module (Namespace "hydra.extract.core") elements
    [Monads.module_, Lexical.module_, Rewriting.module_, ShowCore.module_]
    kernelTypesModules $
    Just ("A DSL for decoding and validating Hydra terms at runtime. This module provides functions to extract typed values from Hydra terms with appropriate error handling.")
  where
   elements = [
     el bigfloatDef,
     el bigfloatValueDef,
     el bigintDef,
     el bigintValueDef,
     el binaryDef,
     el binaryLiteralDef,
     el booleanDef,
     el booleanLiteralDef,
     el caseFieldDef,
     el casesDef,
     el fieldDef,
     el float32Def,
     el float32ValueDef,
     el float64Def,
     el float64ValueDef,
     el floatLiteralDef,
     el floatValueDef,
     el functionTypeDef,
     el injectionDef,
     el int16Def,
     el int16ValueDef,
     el int32Def,
     el int32ValueDef,
     el int64Def,
     el int64ValueDef,
     el int8Def,
     el int8ValueDef,
     el integerLiteralDef,
     el integerValueDef,
     el lambdaBodyDef,
     el lambdaDef,
     el letBindingDef,
     el letTermDef,
     el listDef,
     el listHeadDef,
     el listOfDef,
     el listTypeDef,
     el literalDef,
     el mapDef,
     el mapTypeDef,
     el nArgsDef,
     el maybeTermDef,
     el maybeTypeDef,
     el pairDef,
     el productTypeDef,
     el recordDef,
     el recordTypeDef,
     el setDef,
     el setOfDef,
     el setTypeDef,
     el stringDef,
     el stringLiteralDef,
     el sumTypeDef,
     el termRecordDef,
     el uint16Def,
     el uint16ValueDef,
     el uint32Def,
     el uint32ValueDef,
     el uint64Def,
     el uint64ValueDef,
     el uint8Def,
     el uint8ValueDef,
     el unionTypeDef,
     el unitDef,
     el unitVariantDef,
     el variantDef,
     el wrapDef,
     el wrappedTypeDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bigfloatDef :: TBinding (Term -> Flow Graph Double)
bigfloatDef = define "bigfloat" $
  doc "Extract an arbitrary-precision floating-point value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "f" <<~ ref floatLiteralDef @@ var "l" $
  ref bigfloatValueDef @@ var "f"

bigfloatValueDef :: TBinding (FloatValue -> Flow Graph Double)
bigfloatValueDef = define "bigfloatValue" $
  doc "Extract a bigfloat value from a FloatValue" $
  "v" ~> cases _FloatValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "bigfloat"
      @@ (ref ShowCore.floatValueDef @@ var "v"))) [
    _FloatValue_bigfloat>>: "f" ~> Flows.pure (var "f")]

bigintDef :: TBinding (Term -> Flow Graph Integer)
bigintDef = define "bigint" $
  doc "Extract an arbitrary-precision integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref bigintValueDef @@ var "i"

bigintValueDef :: TBinding (IntegerValue -> Flow Graph Integer)
bigintValueDef = define "bigintValue" $
  doc "Extract a bigint value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "bigint"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_bigint>>: "i" ~> Flows.pure (var "i")]

binaryDef :: TBinding (Term -> Flow Graph String)
binaryDef = define "binary" $
  doc "Extract a binary data value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  ref binaryLiteralDef @@ var "l"

binaryLiteralDef :: TBinding (Literal -> Flow Graph String)
binaryLiteralDef = define "binaryLiteral" $
  doc "Extract a binary literal from a Literal value" $
  "v" ~> cases _Literal (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "binary"
      @@ (ref ShowCore.literalDef @@ var "v"))) [
    _Literal_binary>>: "b" ~> Flows.pure (var "b")]

booleanDef :: TBinding (Term -> Flow Graph Bool)
booleanDef = define "boolean" $
  doc "Extract a boolean value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  ref booleanLiteralDef @@ var "l"

booleanLiteralDef :: TBinding (Literal -> Flow Graph Bool)
booleanLiteralDef = define "booleanLiteral" $
  doc "Extract a boolean literal from a Literal value" $
  "v" ~> cases _Literal (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "boolean"
      @@ (ref ShowCore.literalDef @@ var "v"))) [
    _Literal_boolean>>: "b" ~> Flows.pure (var "b")]

-- TODO: nonstandard; move me
caseFieldDef :: TBinding (Name -> String -> Term -> Flow Graph Field)
caseFieldDef = define "caseField" $
  doc "Extract a specific case handler from a case statement term" $
  "name" ~> "n" ~> "term" ~>
  "fieldName" <~ Core.name (var "n") $
  "cs" <<~ ref casesDef @@ var "name" @@ var "term" $
  "matching" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fieldName"))
    (Core.caseStatementCases (var "cs")) $
  Logic.ifElse (Lists.null (var "matching"))
    (Flows.fail "not enough cases")
    (Flows.pure (Lists.head (var "matching")))

-- TODO: nonstandard; move me
casesDef :: TBinding (Name -> Term -> Flow Graph CaseStatement)
casesDef = define "cases" $
  doc "Extract case statement from a term" $
  "name" ~> "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "case statement"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_function>>: "function" ~> cases _Function (var "function")
      (Just (ref Monads.unexpectedDef
        @@ "case statement"
        @@ (ref ShowCore.termDef @@ var "term"))) [
      _Function_elimination>>: "elimination" ~> cases _Elimination (var "elimination")
        (Just (ref Monads.unexpectedDef
          @@ "case statement"
          @@ (ref ShowCore.termDef @@ var "term"))) [
        _Elimination_union>>: "cs" ~>
          Logic.ifElse (Core.equalName_ (Core.caseStatementTypeName (var "cs")) (var "name"))
            (Flows.pure (var "cs"))
            (ref Monads.unexpectedDef
              @@ ("case statement for type " ++ (Core.unName (var "name")))
              @@ (ref ShowCore.termDef @@ var "term"))]]]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
fieldDef :: TBinding (Name -> (Term -> Flow Graph x) -> [Field] -> Flow Graph x)
fieldDef = define "field" $
  doc "Extract a field value from a list of fields" $
  "fname" ~> "mapping" ~> "fields" ~>
  "matchingFields" <~ Lists.filter
    ("f" ~> Core.equalName_ (Core.fieldName (var "f")) (var "fname"))
    (var "fields") $
  Logic.ifElse (Lists.null (var "matchingFields"))
    (Flows.fail ("field " ++ (Core.unName (var "fname")) ++ " not found"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingFields")) (int32 1))
      ("stripped" <<~ ref Lexical.stripAndDereferenceTermDef @@ (Core.fieldTerm (Lists.head (var "matchingFields"))) $
       var "mapping" @@ var "stripped")
      (Flows.fail ("multiple fields named " ++ (Core.unName (var "fname")))))

float32Def :: TBinding (Term -> Flow Graph Float)
float32Def = define "float32" $
  doc "Extract a 32-bit floating-point value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "f" <<~ ref floatLiteralDef @@ var "l" $
  ref float32ValueDef @@ var "f"

float32ValueDef :: TBinding (FloatValue -> Flow Graph Float)
float32ValueDef = define "float32Value" $
  doc "Extract a float32 value from a FloatValue" $
  "v" ~> cases _FloatValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "float32"
      @@ (ref ShowCore.floatValueDef @@ var "v"))) [
    _FloatValue_float32>>: "f" ~> Flows.pure (var "f")]

float64Def :: TBinding (Term -> Flow Graph Double)
float64Def = define "float64" $
  doc "Extract a 64-bit floating-point value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "f" <<~ ref floatLiteralDef @@ var "l" $
  ref float64ValueDef @@ var "f"

float64ValueDef :: TBinding (FloatValue -> Flow Graph Double)
float64ValueDef = define "float64Value" $
  doc "Extract a float64 value from a FloatValue" $
  "v" ~> cases _FloatValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "float64"
      @@ (ref ShowCore.floatValueDef @@ var "v"))) [
    _FloatValue_float64>>: "f" ~> Flows.pure (var "f")]

floatLiteralDef :: TBinding (Literal -> Flow Graph FloatValue)
floatLiteralDef = define "floatLiteral" $
  doc "Extract a floating-point literal from a Literal value" $
  "lit" ~> cases _Literal (var "lit")
    (Just (ref Monads.unexpectedDef
      @@ "floating-point value"
      @@ (ref ShowCore.literalDef @@ var "lit"))) [
    _Literal_float>>: "v" ~> Flows.pure (var "v")]

floatValueDef :: TBinding (Term -> Flow Graph FloatValue)
floatValueDef = define "floatValue" $
  doc "Extract a float value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  ref floatLiteralDef @@ var "l"

functionTypeDef :: TBinding (Type -> Flow s FunctionType)
functionTypeDef = define "functionType" $
  doc "Extract a function type from a type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "function type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_function>>: "ft" ~> Flows.pure (var "ft")]

-- TODO: nonstandard; move me
injectionDef :: TBinding (Name -> Term -> Flow Graph Field)
injectionDef = define "injection" $
  doc "Extract a field from a union term" $
  "expected" ~> "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "injection"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_union>>: "injection" ~>
      Logic.ifElse (Core.equalName_ (Core.injectionTypeName (var "injection")) (var "expected"))
        (Flows.pure (Core.injectionField (var "injection")))
        (ref Monads.unexpectedDef
          @@ ("injection of type " ++ (Core.unName (var "expected")))
          @@ (Core.unName (Core.injectionTypeName (var "injection"))))]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

int16Def :: TBinding (Term -> Flow Graph I.Int16)
int16Def = define "int16" $
  doc "Extract a 16-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref int16ValueDef @@ var "i"

int16ValueDef :: TBinding (IntegerValue -> Flow Graph I.Int16)
int16ValueDef = define "int16Value" $
  doc "Extract an int16 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "int16"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_int16>>: "i" ~> Flows.pure (var "i")]

int32Def :: TBinding (Term -> Flow Graph Int)
int32Def = define "int32" $
  doc "Extract a 32-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref int32ValueDef @@ var "i"

int32ValueDef :: TBinding (IntegerValue -> Flow Graph Int)
int32ValueDef = define "int32Value" $
  doc "Extract an int32 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "int32"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_int32>>: "i" ~> Flows.pure (var "i")]

int64Def :: TBinding (Term -> Flow Graph I.Int64)
int64Def = define "int64" $
  doc "Extract a 64-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref int64ValueDef @@ var "i"

int64ValueDef :: TBinding (IntegerValue -> Flow Graph I.Int64)
int64ValueDef = define "int64Value" $
  doc "Extract an int64 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "int64"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_int64>>: "i" ~> Flows.pure (var "i")]

int8Def :: TBinding (Term -> Flow Graph I.Int8)
int8Def = define "int8" $
  doc "Extract an 8-bit signed integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref int8ValueDef @@ var "i"

int8ValueDef :: TBinding (IntegerValue -> Flow Graph I.Int8)
int8ValueDef = define "int8Value" $
  doc "Extract an int8 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "int8"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_int8>>: "i" ~> Flows.pure (var "i")]

integerLiteralDef :: TBinding (Literal -> Flow Graph IntegerValue)
integerLiteralDef = define "integerLiteral" $
  doc "Extract an integer literal from a Literal value" $
  "lit" ~> cases _Literal (var "lit")
    (Just (ref Monads.unexpectedDef
      @@ "integer value"
      @@ (ref ShowCore.literalDef @@ var "lit"))) [
    _Literal_integer>>: "v" ~> Flows.pure (var "v")]

integerValueDef :: TBinding (Term -> Flow Graph IntegerValue)
integerValueDef = define "integerValue" $
  doc "Extract an integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  ref integerLiteralDef @@ var "l"

lambdaBodyDef :: TBinding (Term -> Flow Graph Term)
lambdaBodyDef = define "lambdaBody" $
  doc "Extract the body of a lambda term" $
  "term" ~> Flows.map (unaryFunction Core.lambdaBody) (ref lambdaDef @@ var "term")

lambdaDef :: TBinding (Term -> Flow Graph Lambda)
lambdaDef = define "lambda" $
  doc "Extract a lambda from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "lambda"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_function>>: "function" ~> cases _Function (var "function")
      (Just (ref Monads.unexpectedDef
        @@ "lambda"
        @@ (ref ShowCore.termDef @@ var "term"))) [
      _Function_lambda>>: "l" ~> Flows.pure (var "l")]]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
letBindingDef :: TBinding (String -> Term -> Flow Graph Term)
letBindingDef = define "letBinding" $
  doc "Extract a binding with the given name from a let term" $
  "n" ~> "term" ~>
  "name" <~ Core.name (var "n") $
  "letExpr" <<~ ref letTermDef @@ var "term" $
  "matchingBindings" <~ Lists.filter
    ("b" ~> Core.equalName_ (Core.bindingName (var "b")) (var "name"))
    (Core.letBindings (var "letExpr")) $
  Logic.ifElse (Lists.null (var "matchingBindings"))
    (Flows.fail ("no such binding: " ++ var "n"))
    (Logic.ifElse (Equality.equal (Lists.length (var "matchingBindings")) (int32 1))
      (Flows.pure (Core.bindingTerm (Lists.head (var "matchingBindings"))))
      (Flows.fail ("multiple bindings named " ++ var "n")))

letTermDef :: TBinding (Term -> Flow Graph Let)
letTermDef = define "letTerm" $
  doc "Extract a let expression from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "let term"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_let>>: "lt" ~> Flows.pure (var "lt")]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

listDef :: TBinding (Term -> Flow Graph [Term])
listDef = define "list" $
  doc "Extract a list of terms from a term" $
  "term" ~>
  "extract" <~ ("stripped" ~> cases _Term (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "list"
      @@ (ref ShowCore.termDef @@ var "stripped"))) [
    _Term_list>>: "l" ~> produce (var "l")]) $
  "stripped" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term" $
  var "extract" @@ var "stripped"

listHeadDef :: TBinding (Term -> Flow Graph Term)
listHeadDef = define "listHead" $
  doc "Extract the first element of a list term" $
  "term" ~>
  "l" <<~ ref listDef @@ var "term" $
  Logic.ifElse (Lists.null (var "l"))
    (Flows.fail "empty list")
    (Flows.pure (Lists.head (var "l")))

listOfDef :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph [x])
listOfDef = define "listOf" $
  doc "Extract a list of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ ref listDef @@ var "term" $
  Flows.mapList (var "f") (var "els")

listTypeDef :: TBinding (Type -> Flow s Type)
listTypeDef = define "listType" $
  doc "Extract the element type from a list type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "list type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_list>>: "t" ~> Flows.pure (var "t")]

literalDef :: TBinding (Term -> Flow Graph Literal)
literalDef = define "literal" $
  doc "Extract a literal value from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "literal"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_literal>>: "lit" ~> Flows.pure (var "lit")]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

mapDef :: TBinding ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (M.Map k v))
mapDef = define "map" $
  doc "Extract a map of key-value pairs from a term, mapping functions over each key and value" $
  "fk" ~> "fv" ~> "term0" ~>
  "pair" <~ ("kvPair" ~>
    "kterm" <~ first (var "kvPair") $
    "vterm" <~ second (var "kvPair") $
    "kval" <<~ var "fk" @@ var "kterm" $
    "vval" <<~ var "fv" @@ var "vterm" $
    produce (pair (var "kval") (var "vval"))) $
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "map"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_map>>: "m" ~> Flows.map (unaryFunction Maps.fromList) (Flows.mapList (var "pair") (Maps.toList (var "m")))]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

mapTypeDef :: TBinding (Type -> Flow s MapType)
mapTypeDef = define "mapType" $
  doc "Extract the key and value types from a map type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "map type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_map>>: "mt" ~> Flows.pure (var "mt")]

-- TODO: nonstandard; move me
nArgsDef :: TBinding (Name -> Int -> [a] -> Flow s ())
nArgsDef = define "nArgs" $
  doc "Ensure a function has the expected number of arguments" $
  "name" ~> "n" ~> "args" ~>
  Logic.ifElse (Equality.equal (Lists.length (var "args")) (var "n"))
    (produce unit)
    (ref Monads.unexpectedDef @@ (Strings.concat [
      Literals.showInt32 (var "n"),
      " arguments to primitive ",
      Literals.showString (Core.unName (var "name"))]) @@ (Literals.showInt32 (Lists.length (var "args"))))

maybeTermDef :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph (Maybe x))
maybeTermDef = define "maybeTerm" $
  doc "Extract an optional value from a term, applying a function to the value if present" $
  "f" ~> "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "maybe value"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_maybe>>: "mt" ~> Maybes.maybe
      (produce nothing)
      ("t" ~> Flows.map (unaryFunction just) (var "f" @@ var "t"))
      (var "mt")]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

maybeTypeDef :: TBinding (Type -> Flow s Type)
maybeTypeDef = define "maybeType" $
  doc "Extract the base type from an optional type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "maybe type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_maybe>>: "t" ~> Flows.pure (var "t")]

pairDef :: TBinding ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (k, v))
pairDef = define "pair" $
  doc "Extract a pair of values from a term, applying functions to each component" $
  "kf" ~> "vf" ~> "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "product"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_product>>: "terms" ~>
      Logic.ifElse (Equality.equal (Lists.length (var "terms")) (int32 2))
        ("kVal" <<~ var "kf" @@ (Lists.head (var "terms")) $
         "vVal" <<~ var "vf" @@ (Lists.head (Lists.tail (var "terms"))) $
         produce (pair (var "kVal") (var "vVal")))
        (ref Monads.unexpectedDef
          @@ "pair"
          @@ (ref ShowCore.termDef @@ var "term"))]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

productTypeDef :: TBinding (Type -> Flow s [Type])
productTypeDef = define "productType" $
  doc "Extract the component types from a product type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "product type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_product>>: "types" ~> Flows.pure (var "types")]

-- TODO: nonstandard; move me
recordDef :: TBinding (Name -> Term -> Flow Graph [Field])
recordDef = define "record" $
  doc "Extract a record's fields from a term" $
  "expected" ~> "term0" ~>
  "record" <<~ ref termRecordDef @@ var "term0" $
  Logic.ifElse (Equality.equal (Core.recordTypeName (var "record")) (var "expected"))
    (Flows.pure (Core.recordFields (var "record")))
    (ref Monads.unexpectedDef
      @@ ("record of type " ++ (Core.unName (var "expected")))
      @@ (Core.unName (Core.recordTypeName (var "record"))))

-- TODO: nonstandard; move me
recordTypeDef :: TBinding (Name -> Type -> Flow s [FieldType])
recordTypeDef = define "recordType" $
  doc "Extract the field types from a record type" $
  "ename" ~> "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "record type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_record>>: "rowType" ~>
      Logic.ifElse (Core.equalName_ (Core.rowTypeTypeName (var "rowType")) (var "ename"))
        (Flows.pure (Core.rowTypeFields (var "rowType")))
        (ref Monads.unexpectedDef
          @@ ("record of type " ++ (Core.unName (var "ename")))
          @@ ("record of type " ++ (Core.unName (Core.rowTypeTypeName (var "rowType")))))]

setDef :: TBinding (Term -> Flow Graph (S.Set Term))
setDef = define "set" $
  doc "Extract a set of terms from a term" $
  "term" ~>
  "extract" <~ ("stripped" ~> cases _Term (var "stripped")
    (Just $ ref Monads.unexpectedDef
      @@ string "set"
      @@ (ref ShowCore.termDef @@ var "stripped")) [
    _Term_set>>: "s" ~> produce $ var "s"]) $
  "stripped" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term" $
  var "extract" @@ var "stripped"

setOfDef :: TBinding ((Term -> Flow Graph x) -> Term -> Flow Graph (S.Set x))
setOfDef = define "setOf" $
  doc "Extract a set of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ ref setDef @@ var "term" $
  Flows.mapSet (var "f") (var "els")

setTypeDef :: TBinding (Type -> Flow s Type)
setTypeDef = define "setType" $
  doc "Extract the element type from a set type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "set type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_set>>: "t" ~> Flows.pure (var "t")]

stringDef :: TBinding (Term -> Flow Graph String)
stringDef = define "string" $
  doc "Extract a string value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  ref stringLiteralDef @@ var "l"

stringLiteralDef :: TBinding (Literal -> Flow Graph String)
stringLiteralDef = define "stringLiteral" $
  doc "Extract a string literal from a Literal value" $
  "v" ~> cases _Literal (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "string"
      @@ (ref ShowCore.literalDef @@ var "v"))) [
    _Literal_string>>: "s" ~> Flows.pure (var "s")]

sumTypeDef :: TBinding (Type -> Flow s [Type])
sumTypeDef = define "sumType" $
  doc "Extract the component types from a sum type" $
  "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "sum type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_sum>>: "types" ~> Flows.pure (var "types")]

termRecordDef :: TBinding (Term -> Flow Graph Record)
termRecordDef = define "termRecord" $
  doc "Extract a record from a term" $
  "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "record"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_record>>: "record" ~> produce (var "record")]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

uint16Def :: TBinding (Term -> Flow Graph Int)
uint16Def = define "uint16" $
  doc "Extract a 16-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref uint16ValueDef @@ var "i"

uint16ValueDef :: TBinding (IntegerValue -> Flow Graph Int)
uint16ValueDef = define "uint16Value" $
  doc "Extract a uint16 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "uint16"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_uint16>>: "i" ~> Flows.pure (var "i")]

uint32Def :: TBinding (Term -> Flow Graph I.Int64)
uint32Def = define "uint32" $
  doc "Extract a 32-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref uint32ValueDef @@ var "i"

uint32ValueDef :: TBinding (IntegerValue -> Flow Graph I.Int64)
uint32ValueDef = define "uint32Value" $
  doc "Extract a uint32 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "uint32"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_uint32>>: "i" ~> Flows.pure (var "i")]

uint64Def :: TBinding (Term -> Flow Graph Integer)
uint64Def = define "uint64" $
  doc "Extract a 64-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref uint64ValueDef @@ var "i"

uint64ValueDef :: TBinding (IntegerValue -> Flow Graph Integer)
uint64ValueDef = define "uint64Value" $
  doc "Extract a uint64 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "uint64"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_uint64>>: "i" ~> Flows.pure (var "i")]

uint8Def :: TBinding (Term -> Flow Graph I.Int16)
uint8Def = define "uint8" $
  doc "Extract an 8-bit unsigned integer value from a term" $
  "t" ~>
  "l" <<~ ref literalDef @@ var "t" $
  "i" <<~ ref integerLiteralDef @@ var "l" $
  ref uint8ValueDef @@ var "i"

uint8ValueDef :: TBinding (IntegerValue -> Flow Graph I.Int16)
uint8ValueDef = define "uint8Value" $
  doc "Extract a uint8 value from an IntegerValue" $
  "v" ~> cases _IntegerValue (var "v")
    (Just (ref Monads.unexpectedDef
      @@ "uint8"
      @@ (ref ShowCore.integerValueDef @@ var "v"))) [
    _IntegerValue_uint8>>: "i" ~> Flows.pure (var "i")]

-- TODO: nonstandard; move me
unionTypeDef :: TBinding (Name -> Type -> Flow s [FieldType])
unionTypeDef = define "unionType" $
  doc "Extract the field types from a union type" $
  "ename" ~> "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "union type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_union>>: "rowType" ~>
      Logic.ifElse (Equality.equal (Core.rowTypeTypeName (var "rowType")) (var "ename"))
        (Flows.pure (Core.rowTypeFields (var "rowType")))
        (ref Monads.unexpectedDef
          @@ ("union of type " ++ (Core.unName (var "ename")))
          @@ ("union of type " ++ (Core.unName (Core.rowTypeTypeName (var "rowType")))))]

unitDef :: TBinding (Term -> Flow Graph ())
unitDef = define "unit" $
  doc "Extract a unit value from a term" $
  "term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ "unit"
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_unit>>: constant (Flows.pure unit)]

unitVariantDef :: TBinding (Name -> Term -> Flow Graph Name)
unitVariantDef = define "unitVariant" $
  doc "Extract a unit variant (a variant with an empty record value) from a union term" $
  "tname" ~> "term" ~>
  "field" <<~ ref variantDef @@ var "tname" @@ var "term" $
  "ignored" <<~ ref unitDef @@ (Core.fieldTerm (var "field")) $
  produce (Core.fieldName (var "field"))

variantDef :: TBinding (Name -> Term -> Flow Graph Field)
variantDef = define "variant" $
  doc "Extract a field from a union term (alias for injection)" $
  ref injectionDef

-- TODO: nonstandard; move me
wrapDef :: TBinding (Name -> Term -> Flow Graph Term)
wrapDef = define "wrap" $
  doc "Extract the wrapped value from a wrapped term" $
  "expected" ~> "term0" ~>
  "extract" <~ ("term" ~> cases _Term (var "term")
    (Just (ref Monads.unexpectedDef
      @@ ("wrap(" ++ (Core.unName (var "expected")) ++ ")")
      @@ (ref ShowCore.termDef @@ var "term"))) [
    _Term_wrap>>: "wrappedTerm" ~>
      Logic.ifElse (Core.equalName_ (Core.wrappedTermTypeName (var "wrappedTerm")) (var "expected"))
        (Flows.pure (Core.wrappedTermBody (var "wrappedTerm")))
        (ref Monads.unexpectedDef
          @@ ("wrapper of type " ++ (Core.unName (var "expected")))
          @@ (Core.unName (Core.wrappedTermTypeName (var "wrappedTerm"))))]) $
  "term" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term0" $
  var "extract" @@ var "term"

-- TODO: nonstandard; move me
wrappedTypeDef :: TBinding (Name -> Type -> Flow s Type)
wrappedTypeDef = define "wrappedType" $
  doc "Extract the wrapped type from a wrapper type" $
  "ename" ~> "typ" ~>
  "stripped" <~ ref Rewriting.deannotateTypeDef @@ var "typ" $
  cases _Type (var "stripped")
    (Just (ref Monads.unexpectedDef
      @@ "wrapped type"
      @@ (ref ShowCore.typeDef @@ var "typ"))) [
    _Type_wrap>>: "wrappedType" ~>
      Logic.ifElse (Core.equalName_ (Core.wrappedTypeTypeName (var "wrappedType")) (var "ename"))
        (Flows.pure (Core.wrappedTypeBody (var "wrappedType")))
        (ref Monads.unexpectedDef
          @@ ("wrapped type " ++ (Core.unName (var "ename")))
          @@ ("wrapped type " ++ (Core.unName (Core.wrappedTypeTypeName (var "wrappedType")))))]
