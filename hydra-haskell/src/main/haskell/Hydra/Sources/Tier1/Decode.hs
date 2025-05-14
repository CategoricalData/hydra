module Hydra.Sources.Tier1.Decode where

-- Standard term-level Tier-1 imports
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import Hydra.Sources.Tier1.Strip


decodeDefinition :: String -> TTerm a -> TElement a
decodeDefinition = definitionInModule hydraDecodeModule

hydraDecodeModule :: Module
hydraDecodeModule = Module (Namespace "hydra.decode") elements [hydraStripModule] [hydraCoreModule] $
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
      el casesCaseDef,
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

bigfloatDef :: TElement (Term -> Maybe Float)
bigfloatDef = decodeDefinition "bigfloat" $
  compose3 (ref literalDef) (ref floatLiteralDef) (ref bigfloatValueDef)

bigfloatValueDef :: TElement (FloatValue -> Maybe Float)
bigfloatValueDef = decodeDefinition "bigfloatValue" $
  matchVariant _FloatValue _FloatValue_bigfloat

bigintDef :: TElement (Term -> Maybe Int)
bigintDef = decodeDefinition "bigint" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref bigintValueDef)

bigintValueDef :: TElement (IntegerValue -> Maybe Int)
bigintValueDef = decodeDefinition "bigintValue" $
  matchVariant _IntegerValue _IntegerValue_bigint

binaryDef :: TElement (Term -> Maybe String)
binaryDef = decodeDefinition "binary" $
  compose2 (ref literalDef) (ref binaryLiteralDef)

binaryLiteralDef :: TElement (Literal -> Maybe String)
binaryLiteralDef = decodeDefinition "binaryLiteral" $
  matchVariant _Literal _Literal_binary

booleanDef :: TElement (Term -> Maybe Bool)
booleanDef = decodeDefinition "boolean" $
  compose2 (ref literalDef) (ref booleanLiteralDef)

booleanLiteralDef :: TElement (Literal -> Maybe Bool)
booleanLiteralDef = decodeDefinition "booleanLiteral" $
  matchVariant _Literal _Literal_boolean

casesDef :: TElement (Name -> Term -> Maybe [Field])
casesDef = decodeDefinition "cases" $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchElimination">: matchVariant _Function _Function_elimination,
    "matchUnion">: matchVariant _Elimination _Elimination_union]
    $ ref nominalDef
      @@ Core.caseStatementTypeName
      @@ Core.caseStatementCases
      @@ compose3 (var "matchFunction") (var "matchElimination") (var "matchUnion")

casesCaseDef :: TElement (Name -> Name -> Term -> Y.Maybe Term)
casesCaseDef = decodeDefinition "casesCase" $
 lambda "tname" $ lambda "fname" $
   compose2
     (ref casesDef @@ var "tname" )
     (ref fieldDef @@ var "fname")

fieldDef :: TElement (Name -> [Field] -> Maybe Term)
fieldDef = decodeDefinition "field" $
  lambdas ["fname", "fields"] $ lets [
    "matches">: Lists.filter
      (lambda "f" $ Equality.equal (Core.fieldName @@ var "f") $ var "fname")
      (var "fields")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Core.fieldTerm @@ (Lists.head $ var "matches")))
      nothing

float32Def :: TElement (Term -> Maybe Float)
float32Def = decodeDefinition "float32" $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float32ValueDef)

float32ValueDef :: TElement (FloatValue -> Maybe Float)
float32ValueDef = decodeDefinition "float32Value" $
  matchVariant _FloatValue _FloatValue_float32

float64Def :: TElement (Term -> Maybe Float)
float64Def = decodeDefinition "float64" $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float64ValueDef)

float64ValueDef :: TElement (FloatValue -> Maybe Float)
float64ValueDef = decodeDefinition "float64Value" $
  matchVariant _FloatValue _FloatValue_float64

floatLiteralDef = decodeDefinition "floatLiteral" $
  matchVariant _Literal _Literal_float

int8Def :: TElement (Term -> Maybe Int)
int8Def = decodeDefinition "int8" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int8ValueDef)

int8ValueDef :: TElement (IntegerValue -> Maybe Int)
int8ValueDef = decodeDefinition "int8Value" $
  matchVariant _IntegerValue _IntegerValue_int8

int16Def :: TElement (Term -> Maybe Int)
int16Def = decodeDefinition "int16" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int16ValueDef)

int16ValueDef :: TElement (IntegerValue -> Maybe Int)
int16ValueDef = decodeDefinition "int16Value" $
  matchVariant _IntegerValue _IntegerValue_int16

int32Def :: TElement (Term -> Maybe Int)
int32Def = decodeDefinition "int32" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int32ValueDef)

int32ValueDef :: TElement (IntegerValue -> Maybe Int)
int32ValueDef = decodeDefinition "int32Value" $
  matchVariant _IntegerValue _IntegerValue_int32

int64Def :: TElement (Term -> Maybe Int)
int64Def = decodeDefinition "int64" $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int64ValueDef)

int64ValueDef :: TElement (IntegerValue -> Maybe Int)
int64ValueDef = decodeDefinition "int64Value" $
  matchVariant _IntegerValue _IntegerValue_int64

integerLiteralDef :: TElement (Literal -> Maybe IntegerValue)
integerLiteralDef = decodeDefinition "integerLiteral" $
  matchVariant _Literal _Literal_integer

lambdaDef :: TElement (Term -> Maybe Lambda)
lambdaDef = decodeDefinition "lambda" $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchLambda">: matchVariant _Function _Function_lambda]
    $ compose2 (var "matchFunction") (var "matchLambda")

letBindingDef :: TElement (Name -> Term -> Maybe LetBinding)
letBindingDef = decodeDefinition "letBinding" $
  lambda "fname" $ lambda "term" $ Optionals.bind
    (Optionals.map
      Core.letBindings
      (ref letTermDef @@ var "term"))
    (ref letBindingWithKeyDef @@ var "fname")

letBindingWithKeyDef :: TElement (Name -> [LetBinding] -> Maybe LetBinding)
letBindingWithKeyDef = decodeDefinition "letBindingWithKey" $
  lambda "fname" $ lambda "bindings" $ lets [
    "matches">: Lists.filter
      (lambda "b" $ Equality.equal (Core.letBindingName @@ var "b") $ var "fname")
      (var "bindings")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Lists.head $ var "matches"))
      nothing

letTermDef :: TElement (Term -> Maybe Let)
letTermDef = decodeDefinition "letTerm" $
  matchTermVariant _Term_let

listDef :: TElement (Term -> Maybe [Term])
listDef = decodeDefinition "list" $
  matchTermVariant _Term_list

literalDef :: TElement (Term -> Maybe Literal)
literalDef = decodeDefinition "literal" $
  matchTermVariant _Term_literal

mapDef :: TElement (Term -> Maybe (M.Map Term Term))
mapDef = decodeDefinition "map" $
  matchTermVariant _Term_map

nameDef :: TElement (Term -> Name)
nameDef = decodeDefinition "name" $
  lambda "term" $ Optionals.map nm
    (Optionals.bind
      (ref wrapDef @@ Core.name _Name @@ var "term")
      (ref stringDef))
  where
    nm :: TTerm (String -> Name)
    nm = TTerm $ Terms.lambda "s" $ TermWrap $ WrappedTerm _Name $ Terms.var "s"

nominalDef :: TElement ((a -> Name) -> (a -> b) -> (c -> Maybe a) -> Name -> c -> Maybe b)
nominalDef = decodeDefinition "nominal" $
  lambda "getName" $ lambda "getB" $ lambda "getA" $ lambda "expected" $
    lets [
      "namesEqual">: lambda "n1" $ lambda "n2" $ Equality.equalString (Core.unName @@ var "n1") (Core.unName @@ var "n2")] $
      compose2
        (var "getA")
        (lambda "a" $ (Logic.ifElse (var "namesEqual" @@ (var "getName" @@ var "a") @@ (var "expected")))
          (just (var "getB" @@ var "a"))
          nothing)

optionalDef :: TElement (Term -> Maybe (Maybe Term))
optionalDef = decodeDefinition "optional" $
  matchTermVariant _Term_optional

pairDef :: TElement (Term -> Maybe (Term, Term))
pairDef = decodeDefinition "pair" $
  lets [
    "matchProduct">: matchTermVariant _Term_product]
    $ compose2
      (var "matchProduct")
      (lambda "l" $ Logic.ifElse (Equality.equal (int32 2) (Lists.length $ var "l"))
        (just $ pair (Lists.at (int32 0) $ var "l") (Lists.at (int32 1) $ var "l"))
        nothing)

recordDef :: TElement (Name -> Term -> Maybe [Field])
recordDef = decodeDefinition "record" $
  matchNominal _Term_record Core.recordTypeName Core.recordFields

setDef :: TElement (Term -> Maybe (S.Set Term))
setDef = decodeDefinition "set" $
  matchTermVariant _Term_set

stringDef :: TElement (Term -> Maybe String)
stringDef = decodeDefinition "string" $
  compose2 (ref literalDef) (ref stringLiteralDef)

stringLiteralDef :: TElement (Literal -> Maybe String)
stringLiteralDef = decodeDefinition "stringLiteral" $
  matchVariant _Literal _Literal_string

uint8Def :: TElement (Term -> Maybe Int)
uint8Def = decodeDefinition "uint8" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint8ValueDef)

uint8ValueDef :: TElement (IntegerValue -> Maybe Int)
uint8ValueDef = decodeDefinition "uint8Value" $
  matchVariant _IntegerValue _IntegerValue_uint8

uint16Def :: TElement (Term -> Maybe Int)
uint16Def = decodeDefinition "uint16" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint16ValueDef)

uint16ValueDef :: TElement (IntegerValue -> Maybe Int)
uint16ValueDef = decodeDefinition "uint16Value" $
  matchVariant _IntegerValue _IntegerValue_uint16

uint32Def :: TElement (Term -> Maybe Int)
uint32Def = decodeDefinition "uint32" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint32ValueDef)

uint32ValueDef :: TElement (IntegerValue -> Maybe Int)
uint32ValueDef = decodeDefinition "uint32Value" $
  matchVariant _IntegerValue _IntegerValue_uint32

uint64Def :: TElement (Term -> Maybe Int)
uint64Def = decodeDefinition "uint64" $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint64ValueDef)

uint64ValueDef :: TElement (IntegerValue -> Maybe Int)
uint64ValueDef = decodeDefinition "uint64Value" $
  matchVariant _IntegerValue _IntegerValue_uint64

unitDef :: TElement (Term -> Maybe ())
unitDef = decodeDefinition "unit" $
  lambda "term" $ Optionals.map
    (constant unit)
    (ref recordDef @@ Core.name _Unit @@ var "term")

unitVariantDef :: TElement (Name -> Term -> Maybe Name)
unitVariantDef = decodeDefinition "unitVariant" $
  lambda "tname" $ lambda "term" $ Optionals.map
    Core.fieldName
    (ref variantDef @@ var "tname" @@ var "term")

variableDef :: TElement (Term -> Y.Maybe Name)
variableDef = decodeDefinition "variable" $
  matchTermVariant _Term_variable

variantDef :: TElement (Name -> Term -> Maybe Field)
variantDef = decodeDefinition "variant" $
  matchNominal _Term_union Core.injectionTypeName Core.injectionField

wrapDef :: TElement (Name -> Term -> Maybe Term)
wrapDef = decodeDefinition "wrap" $
  matchNominal _Term_wrap Core.wrappedTermTypeName Core.wrappedTermObject

--

compose2 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose2 = Optionals.compose

compose3 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (c -> Maybe d) -> TTerm (a -> Maybe d)
compose3 f g h = Optionals.compose (Optionals.compose f g) h

matchNominal :: Name -> TTerm (a -> Name) -> TTerm (a -> b) -> TTerm (Name -> Term -> Maybe b)
matchNominal fname getName getB = ref nominalDef @@ getName @@ getB @@ matchTermVariant fname

matchTermVariant :: Name -> TTerm (Term -> Maybe a)
matchTermVariant fname = matchVariant _Term fname <.> ref fullyStripTermDef

matchVariant :: Name -> Name -> TTerm (a -> Maybe b)
matchVariant tname fname = match tname (Just nothing) [TCase fname --> lambda "matched_" $ Optionals.pure $ var "matched_"]
