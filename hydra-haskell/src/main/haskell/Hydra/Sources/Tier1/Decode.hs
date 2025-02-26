module Hydra.Sources.Tier1.Decode where

-- Standard term-level Tier-1 imports
import           Hydra.Dsl.Base          as Base
import qualified Hydra.Dsl.Core          as Core
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

decodeFunctionDefinition :: String -> Type -> Type -> TTerm a -> TElement a
decodeFunctionDefinition name dom cod term = decodeDefinition name $
  function dom (tOpt cod) term

decodeNominalFunctionDefinition :: String -> Type -> TTerm a -> TElement a
decodeNominalFunctionDefinition name cod term = decodeDefinition name $
  function nameT (tFun termT $ tOpt cod) term

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
    el optCasesDef,
    el optCasesJustDef,
    el optCasesNothingDef,
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
bigfloatDef = decodeFunctionDefinition "bigfloat" termT tBigfloat $
  compose3 (ref literalDef) (ref floatLiteralDef) (ref bigfloatValueDef)

bigfloatValueDef :: TElement (FloatValue -> Maybe Float)
bigfloatValueDef = decodeFunctionDefinition "bigfloatValue" floatValueT tBigfloat $
  matchVariant _FloatValue _FloatValue_bigfloat

bigintDef :: TElement (Term -> Maybe Int)
bigintDef = decodeFunctionDefinition "bigint" termT tBigint $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref bigintValueDef)

bigintValueDef :: TElement (IntegerValue -> Maybe Int)
bigintValueDef = decodeFunctionDefinition "bigintValue" integerValueT tBigint $
  matchVariant _IntegerValue _IntegerValue_bigint

binaryDef :: TElement (Term -> Maybe String)
binaryDef = decodeFunctionDefinition "binary" termT tBinary $
  compose2 (ref literalDef) (ref binaryLiteralDef)

binaryLiteralDef :: TElement (Literal -> Maybe String)
binaryLiteralDef = decodeFunctionDefinition "binaryLiteral" literalT tBinary $
  matchVariant _Literal _Literal_binary

booleanDef :: TElement (Term -> Maybe Bool)
booleanDef = decodeFunctionDefinition "boolean" termT tBoolean $
  compose2 (ref literalDef) (ref booleanLiteralDef)

booleanLiteralDef :: TElement (Literal -> Maybe Bool)
booleanLiteralDef = decodeFunctionDefinition "booleanLiteral" literalT tBoolean $
  matchVariant _Literal _Literal_boolean

casesDef :: TElement (Name -> Term -> Maybe [Field])
casesDef = decodeNominalFunctionDefinition "cases" (tList fieldT) $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchElimination">: matchVariant _Function _Function_elimination,
    "matchUnion">: matchVariant _Elimination _Elimination_union
    ] $ ref nominalDef
      @@ Core.caseStatementTypeName
      @@ Core.caseStatementCases
      @@ compose3 (var "matchFunction") (var "matchElimination") (var "matchUnion")

casesCaseDef :: TElement (Name -> Name -> Term -> Y.Maybe Term)
casesCaseDef = decodeDefinition "casesCase" $
 functionN [nameT, nameT, termT, tOpt termT] $
 lambda "tname" $ lambda "fname" $
   compose2
     (ref casesDef @@ var "tname" )
     (ref fieldDef @@ var "fname")

fieldDef :: TElement (Name -> [Field] -> Maybe Term)
fieldDef = decodeDefinition "field" $
  functionN [nameT, tList fieldT, tOpt termT] $
  lambdas ["fname", "fields"] $ lets [
    "matches">: Lists.filter
      (lambda "f" $ Equality.equal (Core.fieldName @@ var "f") $ var "fname")
      (var "fields")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Core.fieldTerm @@ (Lists.head $ var "matches")))
      nothing

float32Def :: TElement (Term -> Maybe Float)
float32Def = decodeFunctionDefinition "float32" termT tFloat32 $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float32ValueDef)

float32ValueDef :: TElement (FloatValue -> Maybe Float)
float32ValueDef = decodeFunctionDefinition "float32Value" floatValueT tFloat32 $
  matchVariant _FloatValue _FloatValue_float32

float64Def :: TElement (Term -> Maybe Float)
float64Def = decodeFunctionDefinition "float64" termT tFloat64 $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float64ValueDef)

float64ValueDef :: TElement (FloatValue -> Maybe Float)
float64ValueDef = decodeFunctionDefinition "float64Value" floatValueT tFloat64 $
  matchVariant _FloatValue _FloatValue_float64

floatLiteralDef = decodeFunctionDefinition "floatLiteral" literalT floatValueT $
  matchVariant _Literal _Literal_float

int8Def :: TElement (Term -> Maybe Int)
int8Def = decodeFunctionDefinition "int8" termT tInt8 $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int8ValueDef)

int8ValueDef :: TElement (IntegerValue -> Maybe Int)
int8ValueDef = decodeFunctionDefinition "int8Value" integerValueT tInt8 $
  matchVariant _IntegerValue _IntegerValue_int8

int16Def :: TElement (Term -> Maybe Int)
int16Def = decodeFunctionDefinition "int16" termT tInt16 $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int16ValueDef)

int16ValueDef :: TElement (IntegerValue -> Maybe Int)
int16ValueDef = decodeFunctionDefinition "int16Value" integerValueT tInt16 $
  matchVariant _IntegerValue _IntegerValue_int16

int32Def :: TElement (Term -> Maybe Int)
int32Def = decodeFunctionDefinition "int32" termT tInt32 $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int32ValueDef)

int32ValueDef :: TElement (IntegerValue -> Maybe Int)
int32ValueDef = decodeFunctionDefinition "int32Value" integerValueT tInt32 $
  matchVariant _IntegerValue _IntegerValue_int32

int64Def :: TElement (Term -> Maybe Int)
int64Def = decodeFunctionDefinition "int64" termT tInt64 $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int64ValueDef)

int64ValueDef :: TElement (IntegerValue -> Maybe Int)
int64ValueDef = decodeFunctionDefinition "int64Value" integerValueT tInt64 $
  matchVariant _IntegerValue _IntegerValue_int64

integerLiteralDef :: TElement (Literal -> Maybe IntegerValue)
integerLiteralDef = decodeFunctionDefinition "integerLiteral" literalT integerValueT $
  matchVariant _Literal _Literal_integer

lambdaDef :: TElement (Term -> Maybe Lambda)
lambdaDef = decodeFunctionDefinition "lambda" termT lambdaT $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchLambda">: matchVariant _Function _Function_lambda
    ] $ compose2 (var "matchFunction") (var "matchLambda")

letBindingDef :: TElement (Name -> Term -> Maybe LetBinding)
letBindingDef = decodeDefinition "letBinding" $
  functionN [nameT, termT, tOpt letBindingT] $
  lambda "fname" $ lambda "term" $ Optionals.bind
    (Optionals.map
      Core.letBindings
      (ref letTermDef @@ var "term"))
    (ref letBindingWithKeyDef @@ var "fname")

letBindingWithKeyDef :: TElement (Name -> [LetBinding] -> Maybe LetBinding)
letBindingWithKeyDef = decodeDefinition "letBindingWithKey" $
  functionN [nameT, tList letBindingT, tOpt letBindingT] $
  lambda "fname" $ lambda "bindings" $ lets [
    "matches">: Lists.filter
      (lambda "b" $ Equality.equal (Core.letBindingName @@ var "b") $ var "fname")
      (var "bindings")]
    $ Logic.ifElse (Equality.equal (int32 1) (Lists.length $ var "matches"))
      (just (Lists.head $ var "matches"))
      nothing

letTermDef :: TElement (Term -> Maybe Let)
letTermDef = decodeFunctionDefinition "letTerm" termT letT $
  matchTermVariant _Term_let

listDef :: TElement (Term -> Maybe [Term])
listDef = decodeFunctionDefinition "list" termT (tList termT) $
  matchTermVariant _Term_list

literalDef :: TElement (Term -> Maybe Literal)
literalDef = decodeFunctionDefinition "literal" termT literalT $
  matchTermVariant _Term_literal

mapDef :: TElement (Term -> Maybe (M.Map Term Term))
mapDef = decodeFunctionDefinition "map" termT (tMap termT termT) $
  matchTermVariant _Term_map

nameDef :: TElement (Term -> Name)
nameDef = decodeFunctionDefinition "name" termT nameT $
  lambda "term" $ Optionals.map nm
    (Optionals.bind
      (ref wrapDef @@ Core.name _Name @@ var "term")
      (ref stringDef))
  where
    nm :: TTerm (String -> Name)
    nm = TTerm $ Terms.lambda "s" $ TermWrap $ WrappedTerm _Name $ Terms.var "s"

nominalDef :: TElement ((a -> Name) -> (a -> b) -> (c -> Maybe a) -> Name -> c -> Maybe b)
nominalDef = decodeDefinition "nominal" $
    functionN [tFun tA nameT, tFun tA tB, tFun tC (tOpt tA), nameT, tC, tOpt tB] $
    lambda "getName" $ lambda "getB" $ lambda "getA" $ lambda "expected" $
    compose2
      (var "getA")
      (lambda "a" $ (Logic.ifElse (Equality.equal (var "getName" @@ var "a") $ var "expected"))
        (just (var "getB" @@ var "a"))
        nothing)

optCasesDef :: TElement (Term -> Maybe OptionalCases)
optCasesDef = decodeFunctionDefinition "optCases" termT optionalCasesT $
  lets [
    "matchFunction">: matchTermVariant _Term_function,
    "matchElimination">: matchVariant _Function _Function_elimination,
    "matchOptional">: matchVariant _Elimination _Elimination_optional
    ] $ compose3 (var "matchFunction") (var "matchElimination") (var "matchOptional")

optCasesJustDef :: TElement (Term -> Maybe Term)
optCasesJustDef = decodeFunctionDefinition "optCasesJust" termT termT $
  lambda "term" $ Optionals.map Core.optionalCasesJust (ref optCasesDef @@ var "term")

optCasesNothingDef :: TElement (Term -> Maybe Term)
optCasesNothingDef = decodeFunctionDefinition "optCasesNothing" termT termT $
  lambda "term" $ Optionals.map Core.optionalCasesNothing (ref optCasesDef @@ var "term")

optionalDef :: TElement (Term -> Maybe (Maybe Term))
optionalDef = decodeFunctionDefinition "optional" termT (tOpt termT) $
  matchTermVariant _Term_optional

pairDef :: TElement (Term -> Maybe (Term, Term))
pairDef = decodeFunctionDefinition "pair" termT (tPair termT termT) $
  lets [
    "matchProduct">: matchTermVariant _Term_product
    ] $ compose2
      (var "matchProduct")
      (lambda "l" $ Logic.ifElse (Equality.equal (int32 2) (Lists.length $ var "l"))
        (just $ pair (Lists.at (int32 0) $ var "l") (Lists.at (int32 1) $ var "l"))
        nothing)

recordDef :: TElement (Name -> Term -> Maybe [Field])
recordDef = decodeNominalFunctionDefinition "record" (tList fieldT) $
  matchNominal _Term_record Core.recordTypeName Core.recordFields

setDef :: TElement (Term -> Maybe (S.Set Term))
setDef = decodeFunctionDefinition "set" termT (tSet termT) $
  matchTermVariant _Term_set

stringDef :: TElement (Term -> Maybe String)
stringDef = decodeFunctionDefinition "string" termT tString $
  compose2 (ref literalDef) (ref stringLiteralDef)

stringLiteralDef :: TElement (Literal -> Maybe String)
stringLiteralDef = decodeFunctionDefinition "stringLiteral" literalT tString $
  matchVariant _Literal _Literal_string

uint8Def :: TElement (Term -> Maybe Int)
uint8Def = decodeFunctionDefinition "uint8" termT tUint8 $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint8ValueDef)

uint8ValueDef :: TElement (IntegerValue -> Maybe Int)
uint8ValueDef = decodeFunctionDefinition "uint8Value" integerValueT tUint8 $
  matchVariant _IntegerValue _IntegerValue_uint8

uint16Def :: TElement (Term -> Maybe Int)
uint16Def = decodeFunctionDefinition "uint16" termT tUint16 $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint16ValueDef)

uint16ValueDef :: TElement (IntegerValue -> Maybe Int)
uint16ValueDef = decodeFunctionDefinition "uint16Value" integerValueT tUint16 $
  matchVariant _IntegerValue _IntegerValue_uint16

uint32Def :: TElement (Term -> Maybe Int)
uint32Def = decodeFunctionDefinition "uint32" termT tUint32 $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint32ValueDef)

uint32ValueDef :: TElement (IntegerValue -> Maybe Int)
uint32ValueDef = decodeFunctionDefinition "uint32Value" integerValueT tUint32 $
  matchVariant _IntegerValue _IntegerValue_uint32

uint64Def :: TElement (Term -> Maybe Int)
uint64Def = decodeFunctionDefinition "uint64" termT tUint64 $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint64ValueDef)

uint64ValueDef :: TElement (IntegerValue -> Maybe Int)
uint64ValueDef = decodeFunctionDefinition "uint64Value" integerValueT tUint64 $
  matchVariant _IntegerValue _IntegerValue_uint64

unitDef :: TElement (Term -> Maybe ())
unitDef = decodeFunctionDefinition "unit" termT tUnit $
  lambda "term" $ Optionals.map
    (constant unit)
    (ref recordDef @@ Core.name _Unit @@ var "term")

unitVariantDef :: TElement (Name -> Term -> Maybe Name)
unitVariantDef = decodeDefinition "unitVariant" $
  functionN [nameT, termT, tOpt nameT] $
  lambda "tname" $ lambda "term" $ Optionals.map
    Core.fieldName
    (ref variantDef @@ var "tname" @@ var "term")

variableDef :: TElement (Term -> Y.Maybe Name)
variableDef = decodeFunctionDefinition "variable" termT nameT $
  matchTermVariant _Term_variable

variantDef :: TElement (Name -> Term -> Maybe Field)
variantDef = decodeNominalFunctionDefinition "variant" fieldT $
  matchNominal _Term_union Core.injectionTypeName Core.injectionField

wrapDef :: TElement (Name -> Term -> Maybe Term)
wrapDef = decodeNominalFunctionDefinition "wrap" termT $
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
matchVariant tname fname = match tname (Just nothing) [TCase fname --> lambda "x" $ Optionals.pure $ var "x"]
