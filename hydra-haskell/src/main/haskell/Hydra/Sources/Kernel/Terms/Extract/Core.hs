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
import qualified Hydra.Dsl.Lib.Optionals as Optionals
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
     el optionalDef,
     el optionalTypeDef,
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

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

bigfloatDef :: TElement (Term -> Flow Graph Double)
bigfloatDef = define "bigfloat" $
  doc "Extract an arbitrary-precision floating-point value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref floatLiteralDef @@ var "l") $ lambda "f" $
      ref bigfloatValueDef @@ var "f"

bigfloatValueDef :: TElement (FloatValue -> Flow Graph Double)
bigfloatValueDef = define "bigfloatValue" $
  doc "Extract a bigfloat value from a FloatValue" $
  lambda "v" $ cases _FloatValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "bigfloat" @@ (ref ShowCore.floatValueDef @@ var "v")) [
    _FloatValue_bigfloat>>: lambda "f" $ Flows.pure $ var "f"]

bigintDef :: TElement (Term -> Flow Graph Integer)
bigintDef = define "bigint" $
  doc "Extract an arbitrary-precision integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref bigintValueDef @@ var "i"

bigintValueDef :: TElement (IntegerValue -> Flow Graph Integer)
bigintValueDef = define "bigintValue" $
  doc "Extract a bigint value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "bigint" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_bigint>>: lambda "i" $ Flows.pure $ var "i"]

binaryDef :: TElement (Term -> Flow Graph String)
binaryDef = define "binary" $
  doc "Extract a binary data value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ ref binaryLiteralDef

binaryLiteralDef :: TElement (Literal -> Flow Graph String)
binaryLiteralDef = define "binaryLiteral" $
  doc "Extract a binary literal from a Literal value" $
  lambda "v" $ cases _Literal (var "v") (Just $ ref Monads.unexpectedDef @@ string "binary" @@ (ref ShowCore.literalDef @@ var "v")) [
    _Literal_binary>>: lambda "b" $ Flows.pure $ var "b"]

booleanDef :: TElement (Term -> Flow Graph Bool)
booleanDef = define "boolean" $
  doc "Extract a boolean value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ ref booleanLiteralDef

booleanLiteralDef :: TElement (Literal -> Flow Graph Bool)
booleanLiteralDef = define "booleanLiteral" $
  doc "Extract a boolean literal from a Literal value" $
  lambda "v" $ cases _Literal (var "v") (Just $ ref Monads.unexpectedDef @@ string "boolean" @@ (ref ShowCore.literalDef @@ var "v")) [
    _Literal_boolean>>: lambda "b" $ Flows.pure $ var "b"]

-- TODO: nonstandard; move me
caseFieldDef :: TElement (Name -> String -> Term -> Flow Graph Field)
caseFieldDef = define "caseField" $
  doc "Extract a specific case handler from a case statement term" $
  lambdas ["name", "n", "term"] $ lets [
    "fieldName">: Core.name $ var "n"] $ binds [
    "cs">: ref casesDef @@ var "name" @@ var "term"] $ lets [
    "matching">: Lists.filter
      (lambda "f" $ Core.equalName_ (Core.fieldName $ var "f") (var "fieldName"))
      (Core.caseStatementCases $ var "cs")] $
    Logic.ifElse (Lists.null $ var "matching")
      (Flows.fail $ string "not enough cases")
      (Flows.pure $ Lists.head $ var "matching")

-- TODO: nonstandard; move me
casesDef :: TElement (Name -> Term -> Flow Graph CaseStatement)
casesDef = define "cases" $
  doc "Extract case statement from a term" $
  lambdas ["name", "term0"] $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "case statement" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_function>>: lambda "function" $ cases _Function (var "function") (Just $ ref Monads.unexpectedDef @@ string "case statement" @@ (ref ShowCore.termDef @@ var "term")) [
        _Function_elimination>>: lambda "elimination" $ cases _Elimination (var "elimination") (Just $ ref Monads.unexpectedDef @@ string "case statement" @@ (ref ShowCore.termDef @@ var "term")) [
          _Elimination_union>>: lambda "cs" $
            Logic.ifElse (Core.equalName_ (Core.caseStatementTypeName $ var "cs") (var "name"))
              (Flows.pure $ var "cs")
              (ref Monads.unexpectedDef @@ ("case statement for type " ++ (Core.unName $ var "name")) @@ (ref ShowCore.termDef @@ var "term"))]]]

-- TODO: nonstandard; move me
fieldDef :: TElement (Name -> (Term -> Flow Graph x) -> [Field] -> Flow Graph x)
fieldDef = define "field" $
  doc "Extract a field value from a list of fields" $
  lambdas ["fname", "mapping", "fields"] $ lets [
    "matchingFields">: Lists.filter
      (lambda "f" $ Core.equalName_ (Core.fieldName $ var "f") (var "fname"))
      (var "fields")]
    $ Logic.ifElse (Lists.null $ var "matchingFields")
      (Flows.fail $ "field " ++ (Core.unName $ var "fname") ++ " not found")
      (Logic.ifElse (Equality.equal (Lists.length $ var "matchingFields") $ int32 1)
        (Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ (Core.fieldTerm $ Lists.head $ var "matchingFields")) $ var "mapping")
        (Flows.fail $ "multiple fields named " ++ (Core.unName $ var "fname")))

float32Def :: TElement (Term -> Flow Graph Float)
float32Def = define "float32" $
  doc "Extract a 32-bit floating-point value from a term" $
  lambda "t" $ binds [
    "l">: ref literalDef @@ var "t",
    "f">: ref floatLiteralDef @@ var "l"] $
    ref float32ValueDef @@ var "f"

float32ValueDef :: TElement (FloatValue -> Flow Graph Float)
float32ValueDef = define "float32Value" $
  doc "Extract a float32 value from a FloatValue" $
  lambda "v" $ cases _FloatValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "float32" @@ (ref ShowCore.floatValueDef @@ var "v")) [
    _FloatValue_float32>>: lambda "f" $ Flows.pure $ var "f"]

float64Def :: TElement (Term -> Flow Graph Double)
float64Def = define "float64" $
  doc "Extract a 64-bit floating-point value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref floatLiteralDef @@ var "l") $ lambda "f" $
      ref float64ValueDef @@ var "f"

float64ValueDef :: TElement (FloatValue -> Flow Graph Double)
float64ValueDef = define "float64Value" $
  doc "Extract a float64 value from a FloatValue" $
  lambda "v" $ cases _FloatValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "float64" @@ (ref ShowCore.floatValueDef @@ var "v")) [
    _FloatValue_float64>>: lambda "f" $ Flows.pure $ var "f"]

floatLiteralDef :: TElement (Literal -> Flow Graph FloatValue)
floatLiteralDef = define "floatLiteral" $
  doc "Extract a floating-point literal from a Literal value" $
  lambda "lit" $ cases _Literal (var "lit") (Just $ ref Monads.unexpectedDef @@ string "floating-point value" @@ (ref ShowCore.literalDef @@ var "lit")) [
    _Literal_float>>: lambda "v" $ Flows.pure $ var "v"]

floatValueDef :: TElement (Term -> Flow Graph FloatValue)
floatValueDef = define "floatValue" $
  doc "Extract a float value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") (ref floatLiteralDef)

functionTypeDef :: TElement (Type -> Flow s FunctionType)
functionTypeDef = define "functionType" $
  doc "Extract a function type from a type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "function type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_function>>: lambda "ft" $ Flows.pure $ var "ft"]

-- TODO: nonstandard; move me
injectionDef :: TElement (Name -> Term -> Flow Graph Field)
injectionDef = define "injection" $
  doc "Extract a field from a union term" $
  lambdas ["expected", "term0"] $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "injection" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_union>>: lambda "injection" $
        Logic.ifElse (Core.equalName_ (Core.injectionTypeName $ var "injection") (var "expected"))
          (Flows.pure $ Core.injectionField $ var "injection")
          (ref Monads.unexpectedDef @@ ("injection of type " ++ (Core.unName $ var "expected")) @@ (Core.unName $ Core.injectionTypeName $ var "injection"))]

int16Def :: TElement (Term -> Flow Graph I.Int16)
int16Def = define "int16" $
  doc "Extract a 16-bit signed integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref int16ValueDef @@ var "i"

int16ValueDef :: TElement (IntegerValue -> Flow Graph I.Int16)
int16ValueDef = define "int16Value" $
  doc "Extract an int16 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "int16" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_int16>>: lambda "i" $ Flows.pure $ var "i"]

int32Def :: TElement (Term -> Flow Graph Int)
int32Def = define "int32" $
  doc "Extract a 32-bit signed integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref int32ValueDef @@ var "i"

int32ValueDef :: TElement (IntegerValue -> Flow Graph Int)
int32ValueDef = define "int32Value" $
  doc "Extract an int32 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "int32" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_int32>>: lambda "i" $ Flows.pure $ var "i"]

int64Def :: TElement (Term -> Flow Graph I.Int64)
int64Def = define "int64" $
  doc "Extract a 64-bit signed integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref int64ValueDef @@ var "i"

int64ValueDef :: TElement (IntegerValue -> Flow Graph I.Int64)
int64ValueDef = define "int64Value" $
  doc "Extract an int64 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "int64" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_int64>>: lambda "i" $ Flows.pure $ var "i"]

int8Def :: TElement (Term -> Flow Graph I.Int8)
int8Def = define "int8" $
  doc "Extract an 8-bit signed integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref int8ValueDef @@ var "i"

int8ValueDef :: TElement (IntegerValue -> Flow Graph I.Int8)
int8ValueDef = define "int8Value" $
  doc "Extract an int8 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "int8" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_int8>>: lambda "i" $ Flows.pure $ var "i"]

integerLiteralDef :: TElement (Literal -> Flow Graph IntegerValue)
integerLiteralDef = define "integerLiteral" $
  doc "Extract an integer literal from a Literal value" $
  lambda "lit" $ cases _Literal (var "lit") (Just $ ref Monads.unexpectedDef @@ string "integer value" @@ (ref ShowCore.literalDef @@ var "lit")) [
    _Literal_integer>>: lambda "v" $ Flows.pure $ var "v"]

integerValueDef :: TElement (Term -> Flow Graph IntegerValue)
integerValueDef = define "integerValue" $
  doc "Extract an integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") (ref integerLiteralDef)

lambdaBodyDef :: TElement (Term -> Flow Graph Term)
lambdaBodyDef = define "lambdaBody" $
  doc "Extract the body of a lambda term" $
  lambda "term" $ Flows.map (unaryFunction Core.lambdaBody) $ ref lambdaDef @@ var "term"

lambdaDef :: TElement (Term -> Flow Graph Lambda)
lambdaDef = define "lambda" $
  doc "Extract a lambda from a term" $
  lambda "term0" $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "lambda" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_function>>: lambda "function" $ cases _Function (var "function") (Just $ ref Monads.unexpectedDef @@ string "lambda" @@ (ref ShowCore.termDef @@ var "term")) [
        _Function_lambda>>: lambda "l" $ Flows.pure $ var "l"]]

-- TODO: nonstandard; move me
letBindingDef :: TElement (String -> Term -> Flow Graph Term)
letBindingDef = define "letBinding" $
  doc "Extract a binding with the given name from a let term" $
  lambdas ["n", "term"] $ lets [
    "name">: Core.name $ var "n"]
    $ Flows.bind (ref letTermDef @@ var "term") $
      lambda "letExpr" $ lets [
        "matchingBindings">: Lists.filter
          (lambda "b" $ Core.equalName_ (Core.letBindingName $ var "b") (var "name"))
          (Core.letBindings $ var "letExpr")]
        $ Logic.ifElse (Lists.null $ var "matchingBindings")
          (Flows.fail $ "no such binding: " ++ var "n")
          (Logic.ifElse (Equality.equal (Lists.length $ var "matchingBindings") $ int32 1)
            (Flows.pure $ Core.letBindingTerm $ Lists.head $ var "matchingBindings")
            (Flows.fail $ "multiple bindings named " ++ var "n"))

letTermDef :: TElement (Term -> Flow Graph Let)
letTermDef = define "letTerm" $
  doc "Extract a let expression from a term" $
  lambda "term0" $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "let term" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_let>>: lambda "lt" $ Flows.pure $ var "lt"]

listDef :: TElement (Term -> Flow Graph [Term])
listDef = define "list" $
  doc "Extract a list of terms from a term" $
  "term" ~>
  "stripped" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term" $
   cases _Term (var "stripped")
     (Just $ ref Monads.unexpectedDef @@ string "list" @@ (ref ShowCore.termDef @@ var "stripped")) [
     _Term_list>>: "l" ~> produce $ var "l"]

listHeadDef :: TElement (Term -> Flow Graph Term)
listHeadDef = define "listHead" $
  doc "Extract the first element of a list term" $
  lambda "term" $ Flows.bind (ref listDef @@ var "term") $
    lambda "l" $ Logic.ifElse (Lists.null $ var "l")
      (Flows.fail $ string "empty list")
      (Flows.pure $ Lists.head $ var "l")

listOfDef :: TElement ((Term -> Flow Graph x) -> Term -> Flow Graph [x])
listOfDef = define "listOf" $
  doc "Extract a list of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ ref listDef @@ var "term" $
  Flows.mapList (var "f") (var "els")

listTypeDef :: TElement (Type -> Flow s Type)
listTypeDef = define "listType" $
  doc "Extract the element type from a list type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "list type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_list>>: lambda "t" $ Flows.pure $ var "t"]

literalDef :: TElement (Term -> Flow Graph Literal)
literalDef = define "literal" $
  doc "Extract a literal value from a term" $
  lambda "term0" $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "literal" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_literal>>: lambda "lit" $ Flows.pure $ var "lit"]

mapDef :: TElement ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (M.Map k v))
mapDef = define "map" $
  doc "Extract a map of key-value pairs from a term, mapping functions over each key and value" $
  lambdas ["fk", "fv", "term0"] $ lets [
    "pair">: lambda "kvPair" $ lets [
      "kterm">: first $ var "kvPair",
      "vterm">: second $ var "kvPair"]
      $ Flows.bind (var "fk" @@ var "kterm") $
        lambda "kval" $ Flows.bind (var "fv" @@ var "vterm") $
          lambda "vval" $ Flows.pure $ pair (var "kval") (var "vval")]
    $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
      lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "map" @@ (ref ShowCore.termDef @@ var "term")) [
        _Term_map>>: lambda "m" $ Flows.map (unaryFunction Maps.fromList) $ Flows.mapList (var "pair") $ Maps.toList $ var "m"]

mapTypeDef :: TElement (Type -> Flow s MapType)
mapTypeDef = define "mapType" $
  doc "Extract the key and value types from a map type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "map type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_map>>: lambda "mt" $ Flows.pure $ var "mt"]

-- TODO: nonstandard; move me
nArgsDef :: TElement (Name -> Int -> [Term] -> Flow s ())
nArgsDef = define "nArgs" $
  doc "Ensure a function has the expected number of arguments" $
  lambdas ["name", "n", "args"] $
    Logic.ifElse (Equality.equal (Lists.length $ var "args") (var "n"))
      (Flows.pure unit)
      (ref Monads.unexpectedDef @@ (Strings.concat [
        Literals.showInt32 $ var "n",
        " arguments to primitive ",
        Literals.showString (Core.unName $ var "name")]) @@ (Literals.showInt32 (Lists.length $ var "args")))

optionalDef :: TElement ((Term -> Flow Graph x) -> Term -> Flow Graph (Maybe x))
optionalDef = define "optional" $
  doc "Extract an optional value from a term, applying a function to the value if present" $
  lambdas ["f", "term0"] $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "optional value" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_optional>>: lambda "mt" $ Optionals.maybe
        (Flows.pure nothing)
        (lambda "t" $ Flows.map (unaryFunction just) $ var "f" @@ var "t")
        (var "mt")]

optionalTypeDef :: TElement (Type -> Flow s Type)
optionalTypeDef = define "optionalType" $
  doc "Extract the base type from an optional type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "optional type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_optional>>: lambda "t" $ Flows.pure $ var "t"]

pairDef :: TElement ((Term -> Flow Graph k) -> (Term -> Flow Graph v) -> Term -> Flow Graph (k, v))
pairDef = define "pair" $
  doc "Extract a pair of values from a term, applying functions to each component" $
  lambdas ["kf", "vf", "term0"] $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ string "product" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_product>>: lambda "terms" $
        Logic.ifElse (Equality.equal (Lists.length $ var "terms") $ int32 2)
          (Flows.bind (var "kf" @@ (Lists.head $ var "terms")) $
            lambda "kVal" $ Flows.bind (var "vf" @@ (Lists.head $ Lists.tail $ var "terms")) $
              lambda "vVal" $ Flows.pure $ pair (var "kVal") (var "vVal"))
          (ref Monads.unexpectedDef @@ string "pair" @@ (ref ShowCore.termDef @@ var "term"))]

productTypeDef :: TElement (Type -> Flow s [Type])
productTypeDef = define "productType" $
  doc "Extract the component types from a product type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "product type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_product>>: lambda "types" $ Flows.pure $ var "types"]

-- TODO: nonstandard; move me
recordDef :: TElement (Name -> Term -> Flow Graph [Field])
recordDef = define "record" $
  doc "Extract a record's fields from a term" $
  lambdas ["expected", "term0"] $ binds [
    "record">: ref termRecordDef @@ var "term0"] $
    Logic.ifElse (Equality.equal (Core.recordTypeName $ var "record") (var "expected"))
      (Flows.pure $ Core.recordFields $ var "record")
      (ref Monads.unexpectedDef @@ ("record of type " ++ (Core.unName $ var "expected")) @@ (Core.unName $ Core.recordTypeName $ var "record"))

-- TODO: nonstandard; move me
recordTypeDef :: TElement (Name -> Type -> Flow s [FieldType])
recordTypeDef = define "recordType" $
  doc "Extract the field types from a record type" $
  lambdas ["ename", "typ"] $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "record type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_record>>: lambda "rowType" $
        Logic.ifElse (Core.equalName_ (Core.rowTypeTypeName $ var "rowType") (var "ename"))
          (Flows.pure $ Core.rowTypeFields $ var "rowType")
          (ref Monads.unexpectedDef @@ ("record of type " ++ (Core.unName $ var "ename")) @@ ("record of type " ++ (Core.unName $ Core.rowTypeTypeName $ var "rowType")))]

setDef :: TElement (Term -> Flow Graph (S.Set Term))
setDef = define "set" $
  doc "Extract a set of terms from a term" $
  "term" ~>
  "stripped" <<~ ref Lexical.stripAndDereferenceTermDef @@ var "term" $
  cases _Term (var "stripped")
    (Just $ ref Monads.unexpectedDef @@ string "set" @@ (ref ShowCore.termDef @@ var "stripped")) [
    _Term_set>>: "s" ~> produce $ var "s"]

setOfDef :: TElement ((Term -> Flow Graph x) -> Term -> Flow Graph (S.Set x))
setOfDef = define "setOf" $
  doc "Extract a set of values from a term, mapping a function over each element" $
  "f" ~> "term" ~>
  "els" <<~ ref setDef @@ var "term" $
  Flows.mapSet (var "f") (var "els")

setTypeDef :: TElement (Type -> Flow s Type)
setTypeDef = define "setType" $
  doc "Extract the element type from a set type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "set type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_set>>: lambda "t" $ Flows.pure $ var "t"]

stringDef :: TElement (Term -> Flow Graph String)
stringDef = define "string" $
  doc "Extract a string value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ ref stringLiteralDef

stringLiteralDef :: TElement (Literal -> Flow Graph String)
stringLiteralDef = define "stringLiteral" $
  doc "Extract a string literal from a Literal value" $
  lambda "v" $ cases _Literal (var "v") (Just $ ref Monads.unexpectedDef @@ string "string" @@ (ref ShowCore.literalDef @@ var "v")) [
    _Literal_string>>: lambda "s" $ Flows.pure $ var "s"]

sumTypeDef :: TElement (Type -> Flow s [Type])
sumTypeDef = define "sumType" $
  doc "Extract the component types from a sum type" $
  lambda "typ" $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "sum type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_sum>>: lambda "types" $ Flows.pure $ var "types"]

termRecordDef :: TElement (Term -> Flow Graph Record)
termRecordDef = define "termRecord" $
  doc "Extract a record from a term" $
  lambdas ["term0"] $ binds [
    "term">: ref Lexical.stripAndDereferenceTermDef @@ var "term0"] $
    cases _Term (var "term")
      (Just $ ref Monads.unexpectedDef @@ string "record" @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_record>>: lambda "record" $ produce $ var "record"]

uint16Def :: TElement (Term -> Flow Graph Int)
uint16Def = define "uint16" $
  doc "Extract a 16-bit unsigned integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref uint16ValueDef @@ var "i"

uint16ValueDef :: TElement (IntegerValue -> Flow Graph Int)
uint16ValueDef = define "uint16Value" $
  doc "Extract a uint16 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "uint16" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_uint16>>: lambda "i" $ Flows.pure $ var "i"]

uint32Def :: TElement (Term -> Flow Graph I.Int64)
uint32Def = define "uint32" $
  doc "Extract a 32-bit unsigned integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref uint32ValueDef @@ var "i"

uint32ValueDef :: TElement (IntegerValue -> Flow Graph I.Int64)
uint32ValueDef = define "uint32Value" $
  doc "Extract a uint32 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "uint32" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_uint32>>: lambda "i" $ Flows.pure $ var "i"]

uint64Def :: TElement (Term -> Flow Graph Integer)
uint64Def = define "uint64" $
  doc "Extract a 64-bit unsigned integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref uint64ValueDef @@ var "i"

uint64ValueDef :: TElement (IntegerValue -> Flow Graph Integer)
uint64ValueDef = define "uint64Value" $
  doc "Extract a uint64 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "uint64" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_uint64>>: lambda "i" $ Flows.pure $ var "i"]

uint8Def :: TElement (Term -> Flow Graph I.Int16)
uint8Def = define "uint8" $
  doc "Extract an 8-bit unsigned integer value from a term" $
  lambda "t" $ Flows.bind (ref literalDef @@ var "t") $ lambda "l" $
    Flows.bind (ref integerLiteralDef @@ var "l") $ lambda "i" $
      ref uint8ValueDef @@ var "i"

uint8ValueDef :: TElement (IntegerValue -> Flow Graph I.Int16)
uint8ValueDef = define "uint8Value" $
  doc "Extract a uint8 value from an IntegerValue" $
  lambda "v" $ cases _IntegerValue (var "v") (Just $ ref Monads.unexpectedDef @@ string "uint8" @@ (ref ShowCore.integerValueDef @@ var "v")) [
    _IntegerValue_uint8>>: lambda "i" $ Flows.pure $ var "i"]

-- TODO: nonstandard; move me
unionTypeDef :: TElement (Name -> Type -> Flow s [FieldType])
unionTypeDef = define "unionType" $
  doc "Extract the field types from a union type" $
  lambdas ["ename", "typ"] $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "union type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_union>>: lambda "rowType" $
        Logic.ifElse (Equality.equal (Core.rowTypeTypeName $ var "rowType") (var "ename"))
          (Flows.pure $ Core.rowTypeFields $ var "rowType")
          (ref Monads.unexpectedDef @@ ("union of type " ++ (Core.unName $ var "ename")) @@ ("union of type " ++ (Core.unName $ Core.rowTypeTypeName $ var "rowType")))]

unitDef :: TElement (Term -> Flow Graph ())
unitDef = define "unit" $
  doc "Extract a unit value from a term" $
  lambda "term" $ cases _Term (var "term")
    (Just $ ref Monads.unexpectedDef @@ string "unit" @@ (ref ShowCore.termDef @@ var "term")) [
    _Term_unit>>: constant $ Flows.pure unit]

unitVariantDef :: TElement (Name -> Term -> Flow Graph Name)
unitVariantDef = define "unitVariant" $
  doc "Extract a unit variant (a variant with an empty record value) from a union term" $
  lambdas ["tname", "term"] $
    bind "field" (ref variantDef @@ var "tname" @@ var "term") $
    bind "ignored" (ref unitDef @@ (Core.fieldTerm $ var "field")) $
    Flows.pure $ Core.fieldName $ var "field"

variantDef :: TElement (Name -> Term -> Flow Graph Field)
variantDef = define "variant" $
  doc "Extract a field from a union term (alias for injection)" $
  ref injectionDef

-- TODO: nonstandard; move me
wrapDef :: TElement (Name -> Term -> Flow Graph Term)
wrapDef = define "wrap" $
  doc "Extract the wrapped value from a wrapped term" $
  lambdas ["expected", "term0"] $ Flows.bind (ref Lexical.stripAndDereferenceTermDef @@ var "term0") $
    lambda "term" $ cases _Term (var "term") (Just $ ref Monads.unexpectedDef @@ ("wrap(" ++ (Core.unName $ var "expected") ++ ")") @@ (ref ShowCore.termDef @@ var "term")) [
      _Term_wrap>>: lambda "wrappedTerm" $
        Logic.ifElse (Core.equalName_ (Core.wrappedTermTypeName $ var "wrappedTerm") (var "expected"))
          (Flows.pure $ Core.wrappedTermObject $ var "wrappedTerm")
          (ref Monads.unexpectedDef @@ ("wrapper of type " ++ (Core.unName $ var "expected")) @@ (Core.unName $ Core.wrappedTermTypeName $ var "wrappedTerm"))]

-- TODO: nonstandard; move me
wrappedTypeDef :: TElement (Name -> Type -> Flow s Type)
wrappedTypeDef = define "wrappedType" $
  doc "Extract the wrapped type from a wrapper type" $
  lambdas ["ename", "typ"] $ lets [
    "stripped">: ref Rewriting.deannotateTypeDef @@ var "typ"]
    $ cases _Type (var "stripped") (Just $ ref Monads.unexpectedDef @@ string "wrapped type" @@ (ref ShowCore.typeDef @@ var "typ")) [
      _Type_wrap>>: lambda "wrappedType" $
        Logic.ifElse (Core.equalName_ (Core.wrappedTypeTypeName $ var "wrappedType") (var "ename"))
          (Flows.pure $ Core.wrappedTypeObject $ var "wrappedType")
          (ref Monads.unexpectedDef @@ ("wrapped type " ++ (Core.unName $ var "ename")) @@ ("wrapped type " ++ (Core.unName $ Core.wrappedTypeTypeName $ var "wrappedType")))]
