module Hydra.Sources.Kernel.Terms.Decoding where

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

import qualified Hydra.Sources.Kernel.Terms.Rewriting as Rewriting


module_ :: Module
module_ = Module (Namespace "hydra.decoding") elements
    [Rewriting.module_]
    kernelTypesModules $
    Just "A module for decoding terms to native objects"
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
      el int16Def,
      el int16ValueDef,
      el int32Def,
      el int32ValueDef,
      el int64Def,
      el int64ValueDef,
      el int8Def,
      el int8ValueDef,
      el integerLiteralDef,
      el lambdaDef,
      el letBindingDef,
      el letBindingWithKeyDef,
      el letTermDef,
      el listDef,
      el literalDef,
      el mapDef,
      el nameDef,
      el nominalDef,
      el optionalDef,
      el pairDef,
      el recordDef,
      el setDef,
      el stringDef,
      el stringLiteralDef,
      el uint16Def,
      el uint16ValueDef,
      el uint32Def,
      el uint32ValueDef,
      el uint64Def,
      el uint64ValueDef,
      el uint8Def,
      el uint8ValueDef,
      el unitDef,
      el unitVariantDef,
      el variableDef,
      el variantDef,
      el wrapDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bigfloatDef :: TBinding (Term -> Maybe Float)
bigfloatDef = define "bigfloat" $
  compose3 (ref literalDef) (ref floatLiteralDef) (ref bigfloatValueDef)

bigfloatValueDef :: TBinding (FloatValue -> Maybe Float)
bigfloatValueDef = define "bigfloatValue" $
  matchVariant _FloatValue _FloatValue_bigfloat

bigintDef :: TBinding (Term -> Maybe Int)
bigintDef = define "bigint" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref bigintValueDef)

bigintValueDef :: TBinding (IntegerValue -> Maybe Int)
bigintValueDef = define "bigintValue" $
  matchVariant _IntegerValue _IntegerValue_bigint

binaryDef :: TBinding (Term -> Maybe String)
binaryDef = define "binary" $
  compose2 (ref literalDef) (ref binaryLiteralDef)

binaryLiteralDef :: TBinding (Literal -> Maybe String)
binaryLiteralDef = define "binaryLiteral" $
  matchVariant _Literal _Literal_binary

booleanDef :: TBinding (Term -> Maybe Bool)
booleanDef = define "boolean" $
  compose2 (ref literalDef) (ref booleanLiteralDef)

booleanLiteralDef :: TBinding (Literal -> Maybe Bool)
booleanLiteralDef = define "booleanLiteral" $
  matchVariant _Literal _Literal_boolean

casesDef :: TBinding (Name -> Term -> Maybe [Field])
casesDef = define "cases" $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchElimination">: matchVariant _Function _Function_elimination,
    "matchUnion">: matchVariant _Elimination _Elimination_union]
    $ ref nominalDef
      @@ (unaryFunction Core.caseStatementTypeName)
      @@ (unaryFunction Core.caseStatementCases)
      @@ compose3 (var "matchFunction") (var "matchElimination") (var "matchUnion")

caseFieldDef :: TBinding (Name -> Name -> Term -> Y.Maybe Term)
caseFieldDef = define "caseField" $
 lambda "tname" $ lambda "fname" $
   compose2
     (ref casesDef @@ var "tname" )
     (ref fieldDef @@ var "fname")

fieldDef :: TBinding (Name -> [Field] -> Maybe Term)
fieldDef = define "field" $
  lambdas ["fname", "fields"] $ lets [
    "matches">: Lists.filter
      (lambda "f" $ Equality.equal (Core.fieldName $ var "f") $ var "fname")
      (var "fields")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Core.fieldTerm $ (Lists.head $ var "matches")))
      nothing

float32Def :: TBinding (Term -> Maybe Float)
float32Def = define "float32" $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float32ValueDef)

float32ValueDef :: TBinding (FloatValue -> Maybe Float)
float32ValueDef = define "float32Value" $
  matchVariant _FloatValue _FloatValue_float32

float64Def :: TBinding (Term -> Maybe Float)
float64Def = define "float64" $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float64ValueDef)

float64ValueDef :: TBinding (FloatValue -> Maybe Float)
float64ValueDef = define "float64Value" $
  matchVariant _FloatValue _FloatValue_float64

floatLiteralDef = define "floatLiteral" $
  matchVariant _Literal _Literal_float

int8Def :: TBinding (Term -> Maybe Int)
int8Def = define "int8" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int8ValueDef)

int8ValueDef :: TBinding (IntegerValue -> Maybe Int)
int8ValueDef = define "int8Value" $
  matchVariant _IntegerValue _IntegerValue_int8

int16Def :: TBinding (Term -> Maybe Int)
int16Def = define "int16" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int16ValueDef)

int16ValueDef :: TBinding (IntegerValue -> Maybe Int)
int16ValueDef = define "int16Value" $
  matchVariant _IntegerValue _IntegerValue_int16

int32Def :: TBinding (Term -> Maybe Int)
int32Def = define "int32" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int32ValueDef)

int32ValueDef :: TBinding (IntegerValue -> Maybe Int)
int32ValueDef = define "int32Value" $
  matchVariant _IntegerValue _IntegerValue_int32

int64Def :: TBinding (Term -> Maybe Int)
int64Def = define "int64" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int64ValueDef)

int64ValueDef :: TBinding (IntegerValue -> Maybe Int)
int64ValueDef = define "int64Value" $
  matchVariant _IntegerValue _IntegerValue_int64

integerLiteralDef :: TBinding (Literal -> Maybe IntegerValue)
integerLiteralDef = define "integerLiteral" $
  matchVariant _Literal _Literal_integer

lambdaDef :: TBinding (Term -> Maybe Lambda)
lambdaDef = define "lambda" $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchLambda">: matchVariant _Function _Function_lambda]
    $ compose2 (var "matchFunction") (var "matchLambda")

letBindingDef :: TBinding (Name -> Term -> Maybe Binding)
letBindingDef = define "letBinding" $
  lambda "fname" $ lambda "term" $ Optionals.bind
    (Optionals.map
      (unaryFunction Core.letBindings)
      (ref letTermDef @@ var "term"))
    (ref letBindingWithKeyDef @@ var "fname")

letBindingWithKeyDef :: TBinding (Name -> [Binding] -> Maybe Binding)
letBindingWithKeyDef = define "letBindingWithKey" $
  lambda "fname" $ lambda "bindings" $ lets [
    "matches">: Lists.filter
      (lambda "b" $ Equality.equal (Core.bindingName $ var "b") $ var "fname")
      (var "bindings")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Lists.head $ var "matches"))
      nothing

letTermDef :: TBinding (Term -> Maybe Let)
letTermDef = define "letTerm" $
  matchTermVariant _Term_let

listDef :: TBinding (Term -> Maybe [Term])
listDef = define "list" $
  matchTermVariant _Term_list

literalDef :: TBinding (Term -> Maybe Literal)
literalDef = define "literal" $
  matchTermVariant _Term_literal

mapDef :: TBinding (Term -> Maybe (M.Map Term Term))
mapDef = define "map" $
  matchTermVariant _Term_map

nameDef :: TBinding (Term -> Maybe Name)
nameDef = define "name" $
  lambda "term" $ Optionals.map nm
    (Optionals.bind
      (ref wrapDef @@ Core.nameLift _Name @@ var "term")
      (ref stringDef))
  where
    nm :: TTerm (String -> Name)
    nm = TTerm $ Terms.lambda "s" $ TermWrap $ WrappedTerm _Name $ Terms.var "s"

nominalDef :: TBinding ((a -> Name) -> (a -> b) -> (c -> Maybe a) -> Name -> c -> Maybe b)
nominalDef = define "nominal" $
  lambda "getName" $ lambda "getB" $ lambda "getA" $ lambda "expected" $
    lets [
      "namesEqual">: lambda "n1" $ lambda "n2" $ Equality.equal (Core.unName $ var "n1") (Core.unName $ var "n2")] $
      compose2
        (var "getA")
        (lambda "a" $ (Logic.ifElse (var "namesEqual" @@ (var "getName" @@ var "a") @@ (var "expected")))
          (just (var "getB" @@ var "a"))
          nothing)

optionalDef :: TBinding (Term -> Maybe (Maybe Term))
optionalDef = define "optional" $
  matchTermVariant _Term_optional

pairDef :: TBinding (Term -> Maybe (Term, Term))
pairDef = define "pair" $
  lets [
    "matchProduct">: matchTermVariant _Term_product]
    $ compose2
      (var "matchProduct")
      (lambda "l" $ Logic.ifElse (Equality.equal (int32 2) (Lists.length $ var "l"))
        (just $ pair (Lists.at (int32 0) $ var "l") (Lists.at (int32 1) $ var "l"))
        nothing)

recordDef :: TBinding (Name -> Term -> Maybe [Field])
recordDef = define "record" $
  matchNominal _Term_record
    (unaryFunction Core.recordTypeName)
    (unaryFunction Core.recordFields)

setDef :: TBinding (Term -> Maybe (S.Set Term))
setDef = define "set" $
  matchTermVariant _Term_set

stringDef :: TBinding (Term -> Maybe String)
stringDef = define "string" $
  compose2 (ref literalDef) (ref stringLiteralDef)

stringLiteralDef :: TBinding (Literal -> Maybe String)
stringLiteralDef = define "stringLiteral" $
  matchVariant _Literal _Literal_string

uint8Def :: TBinding (Term -> Maybe Int)
uint8Def = define "uint8" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint8ValueDef)

uint8ValueDef :: TBinding (IntegerValue -> Maybe Int)
uint8ValueDef = define "uint8Value" $
  matchVariant _IntegerValue _IntegerValue_uint8

uint16Def :: TBinding (Term -> Maybe Int)
uint16Def = define "uint16" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint16ValueDef)

uint16ValueDef :: TBinding (IntegerValue -> Maybe Int)
uint16ValueDef = define "uint16Value" $
  matchVariant _IntegerValue _IntegerValue_uint16

uint32Def :: TBinding (Term -> Maybe Int)
uint32Def = define "uint32" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint32ValueDef)

uint32ValueDef :: TBinding (IntegerValue -> Maybe Int)
uint32ValueDef = define "uint32Value" $
  matchVariant _IntegerValue _IntegerValue_uint32

uint64Def :: TBinding (Term -> Maybe Int)
uint64Def = define "uint64" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint64ValueDef)

uint64ValueDef :: TBinding (IntegerValue -> Maybe Int)
uint64ValueDef = define "uint64Value" $
  matchVariant _IntegerValue _IntegerValue_uint64

unitDef :: TBinding (Term -> Maybe ())
unitDef = define "unit" $
  lambda "term" $ cases _Term (var "term") (Just nothing) [
    _Term_unit>>: constant $ just unit]

unitVariantDef :: TBinding (Name -> Term -> Maybe Name)
unitVariantDef = define "unitVariant" $
  lambda "tname" $ lambda "term" $ Optionals.map
    (unaryFunction Core.fieldName)
    (ref variantDef @@ var "tname" @@ var "term")

variableDef :: TBinding (Term -> Y.Maybe Name)
variableDef = define "variable" $
  matchTermVariant _Term_variable

variantDef :: TBinding (Name -> Term -> Maybe Field)
variantDef = define "variant" $
  matchNominal _Term_union
    (unaryFunction Core.injectionTypeName)
    (unaryFunction Core.injectionField)

wrapDef :: TBinding (Name -> Term -> Maybe Term)
wrapDef = define "wrap" $
  matchNominal _Term_wrap
    (unaryFunction Core.wrappedTermTypeName)
    (unaryFunction Core.wrappedTermBody)

--

compose2 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose2 = Optionals.compose

compose3 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (c -> Maybe d) -> TTerm (a -> Maybe d)
compose3 f g h = Optionals.compose (Optionals.compose f g) h

matchNominal :: Name -> TTerm (a -> Name) -> TTerm (a -> b) -> TTerm (Name -> Term -> Maybe b)
matchNominal fname getName getB = ref nominalDef @@ getName @@ getB @@ matchTermVariant fname

matchTermVariant :: Name -> TTerm (Term -> Maybe a)
matchTermVariant fname = matchVariant _Term fname <.> ref Rewriting.deannotateTermDef

matchVariant :: Name -> Name -> TTerm (a -> Maybe b)
matchVariant tname fname = match tname (Just nothing) [
  fname>>: lambda "matched_" $ Optionals.pure $ var "matched_"]
