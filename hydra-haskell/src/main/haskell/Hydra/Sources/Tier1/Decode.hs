module Hydra.Sources.Tier1.Decode where

-- Standard Tier-1 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier0.All

import Hydra.Sources.Tier1.Strip

import qualified Data.Map as M
import qualified Data.Set as S


decodeDefinition :: String -> TTerm a -> TElement a
decodeDefinition = definitionInModule decodeModule

decodeFunctionDefinition :: String -> Type -> Type -> TTerm a -> TElement a
decodeFunctionDefinition name dom cod term = decodeDefinition name $
  function dom (optionalT cod) term

decodeNominalFunctionDefinition :: String -> Type -> TTerm a -> TElement a
decodeNominalFunctionDefinition name cod term = decodeDefinition name $
  function nameT (funT termT $ optionalT cod) term

decodeModule :: Module
decodeModule = Module (Namespace "hydra/decode") elements [hydraStripModule] tier0Modules $
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
bigfloatDef = decodeFunctionDefinition "bigfloat" termT bigfloatT $
  compose3 (ref literalDef) (ref floatLiteralDef) (ref bigfloatValueDef)

bigfloatValueDef :: TElement (FloatValue -> Maybe Float)
bigfloatValueDef = decodeFunctionDefinition "bigfloatValue" floatValueT bigfloatT $
  matchVariant _FloatValue _FloatValue_bigfloat

bigintDef :: TElement (Term -> Maybe Int)
bigintDef = decodeFunctionDefinition "bigint" termT bigintT $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref bigintValueDef)

bigintValueDef :: TElement (IntegerValue -> Maybe Int)
bigintValueDef = decodeFunctionDefinition "bigintValue" integerValueT bigintT $
  matchVariant _IntegerValue _IntegerValue_bigint

binaryDef :: TElement (Term -> Maybe String)
binaryDef = decodeFunctionDefinition "binary" termT binaryT $
  compose2 (ref literalDef) (ref binaryLiteralDef)

binaryLiteralDef :: TElement (Literal -> Maybe String)
binaryLiteralDef = decodeFunctionDefinition "binaryLiteral" literalT binaryT $
  matchVariant _Literal _Literal_binary

booleanDef :: TElement (Term -> Maybe Bool)
booleanDef = decodeFunctionDefinition "boolean" termT booleanT $
  compose2 (ref literalDef) (ref booleanLiteralDef)

booleanLiteralDef :: TElement (Literal -> Maybe Bool)
booleanLiteralDef = decodeFunctionDefinition "booleanLiteral" literalT booleanT $
  matchVariant _Literal _Literal_boolean

casesDef :: TElement (Name -> Term -> Maybe [Field])
casesDef = decodeNominalFunctionDefinition "cases" (listT fieldT) $
  ref nominalDef
    @@ Core.caseStatementTypeName
    @@ Core.caseStatementCases
    @@ compose3
      (matchTermVariant _Term_function)
      (matchVariant _Function _Function_elimination)
      (matchVariant _Elimination _Elimination_union)

casesCaseDef :: TElement (Name -> Name -> Term -> Y.Maybe Term)
casesCaseDef = decodeDefinition "casesCase" $
 functionN [nameT, nameT, termT, optionalT termT] $
 lambda "tname" $ lambda "fname" $
   compose2
     (ref casesDef @@ var "tname" )
     (ref fieldDef @@ var "fname")

fieldDef :: TElement (Name -> [Field] -> Maybe Term)
fieldDef = decodeDefinition "field" $
  functionN [nameT, listT fieldT, optionalT termT] $
  lambda "fname" $ lambda "fields" ((Logic.ifElse
        @@ just (Core.fieldTerm @@ (Lists.head @@ var "matches"))
        @@ nothing
        @@ (Equality.equal @@ int32 1 @@ (Lists.length @@ var "matches"))
    ) `with` [
      "matches">: Lists.filter @@ (lambda "f" $ Equality.equal @@ (Core.fieldName @@ var "f") @@ var "fname") @@ var "fields"])

float32Def :: TElement (Term -> Maybe Float)
float32Def = decodeFunctionDefinition "float32" termT float32T $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float32ValueDef)

float32ValueDef :: TElement (FloatValue -> Maybe Float)
float32ValueDef = decodeFunctionDefinition "float32Value" floatValueT float32T $
  matchVariant _FloatValue _FloatValue_float32

float64Def :: TElement (Term -> Maybe Float)
float64Def = decodeFunctionDefinition "float64" termT float64T $
  compose3
    (ref literalDef)
    (ref floatLiteralDef)
    (ref float64ValueDef)

float64ValueDef :: TElement (FloatValue -> Maybe Float)
float64ValueDef = decodeFunctionDefinition "float64Value" floatValueT float64T $
  matchVariant _FloatValue _FloatValue_float64

floatLiteralDef = decodeFunctionDefinition "floatLiteral" literalT floatValueT $
  matchVariant _Literal _Literal_float

int8Def :: TElement (Term -> Maybe Int)
int8Def = decodeFunctionDefinition "int8" termT int8T $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int8ValueDef)

int8ValueDef :: TElement (IntegerValue -> Maybe Int)
int8ValueDef = decodeFunctionDefinition "int8Value" integerValueT int8T $
  matchVariant _IntegerValue _IntegerValue_int8

int16Def :: TElement (Term -> Maybe Int)
int16Def = decodeFunctionDefinition "int16" termT int16T $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int16ValueDef)

int16ValueDef :: TElement (IntegerValue -> Maybe Int)
int16ValueDef = decodeFunctionDefinition "int16Value" integerValueT int16T $
  matchVariant _IntegerValue _IntegerValue_int16

int32Def :: TElement (Term -> Maybe Int)
int32Def = decodeFunctionDefinition "int32" termT int32T $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int32ValueDef)

int32ValueDef :: TElement (IntegerValue -> Maybe Int)
int32ValueDef = decodeFunctionDefinition "int32Value" integerValueT int32T $
  matchVariant _IntegerValue _IntegerValue_int32

int64Def :: TElement (Term -> Maybe Int)
int64Def = decodeFunctionDefinition "int64" termT int64T $
  compose3
    (ref literalDef)
    (ref integerLiteralDef)
    (ref int64ValueDef)

int64ValueDef :: TElement (IntegerValue -> Maybe Int)
int64ValueDef = decodeFunctionDefinition "int64Value" integerValueT int64T $
  matchVariant _IntegerValue _IntegerValue_int64

integerLiteralDef :: TElement (Literal -> Maybe IntegerValue)
integerLiteralDef = decodeFunctionDefinition "integerLiteral" literalT integerValueT $
  matchVariant _Literal _Literal_integer

lambdaDef :: TElement (Term -> Maybe Lambda)
lambdaDef = decodeFunctionDefinition "lambda" termT lambdaT $
  compose2
    (matchTermVariant _Term_function)
    (matchVariant _Function _Function_lambda)

letBindingDef :: TElement (Name -> Term -> Maybe LetBinding)
letBindingDef = decodeDefinition "letBinding" $
  functionN [nameT, termT, optionalT letBindingT] $
  lambda "fname" $ lambda "term" $ Optionals.bind
    @@ (Optionals.map
      @@ Core.letBindings
      @@ (ref letTermDef @@ var "term"))
    @@ (ref letBindingWithKeyDef @@ var "fname")

letBindingWithKeyDef :: TElement (Name -> [LetBinding] -> Maybe LetBinding)
letBindingWithKeyDef = decodeDefinition "letBindingWithKey" $
  functionN [nameT, listT letBindingT, optionalT letBindingT] $
  lambda "fname" $ lambda "bindings" ((Logic.ifElse
        @@ just (Lists.head @@ var "matches")
        @@ nothing
        @@ (Equality.equal @@ int32 1 @@ (Lists.length @@ var "matches"))
    ) `with` [
      "matches">: Lists.filter @@ (lambda "b" $ Equality.equal @@ (Core.letBindingName @@ var "b") @@ var "fname") @@ var "bindings"])

letTermDef :: TElement (Term -> Maybe Let)
letTermDef = decodeFunctionDefinition "letTerm" termT letT $
  matchTermVariant _Term_let

listDef :: TElement (Term -> Maybe [Term])
listDef = decodeFunctionDefinition "list" termT (listT termT) $
  matchTermVariant _Term_list

literalDef :: TElement (Term -> Maybe Literal)
literalDef = decodeFunctionDefinition "literal" termT literalT $
  matchTermVariant _Term_literal

mapDef :: TElement (Term -> Maybe (M.Map Term Term))
mapDef = decodeFunctionDefinition "map" termT (mapT termT termT) $
  matchTermVariant _Term_map

nominalDef :: TElement ((a -> Name) -> (a -> b) -> (c -> Maybe a) -> Name -> c -> Maybe b)
nominalDef = decodeDefinition "nominal" $
    functionN [funT aT nameT, funT aT bT, funT cT (optionalT aT), nameT, cT, optionalT bT] $
    lambda "getName" $ lambda "getB" $ lambda "getA" $ lambda "expected" $
    compose2
      (var "getA")
      (lambda "a" $ (Logic.ifElse
        @@ (just (var "getB" @@ var "a"))
        @@ nothing
        @@ (Equality.equal @@ (var "getName" @@ var "a") @@ var "expected")))

optCasesDef :: TElement (Term -> Maybe OptionalCases)
optCasesDef = decodeFunctionDefinition "optCases" termT optionalCasesT $
  compose3
    (matchTermVariant _Term_function)
    (matchVariant _Function _Function_elimination)
    (matchVariant _Elimination _Elimination_optional)

optCasesJustDef :: TElement (Term -> Maybe Term)
optCasesJustDef = decodeFunctionDefinition "optCasesJust" termT termT $
  lambda "term" $ Optionals.map @@ Core.optionalCasesJust @@ (ref optCasesDef @@ var "term")

optCasesNothingDef :: TElement (Term -> Maybe Term)
optCasesNothingDef = decodeFunctionDefinition "optCasesNothing" termT termT $
  lambda "term" $ Optionals.map @@ Core.optionalCasesNothing @@ (ref optCasesDef @@ var "term")

optionalDef :: TElement (Term -> Maybe (Maybe Term))
optionalDef = decodeFunctionDefinition "optional" termT (optionalT termT) $
  matchTermVariant _Term_optional

pairDef :: TElement (Term -> Maybe (Term, Term))
pairDef = decodeFunctionDefinition "pair" termT (pairT termT termT) $
  compose2
    (matchTermVariant _Term_product)
    (lambda "l" $ Logic.ifElse
      @@ (just $ pair (Lists.at @@ int32 0 @@ var "l") (Lists.at @@ int32 1 @@ var "l"))
      @@ nothing
      @@ (Equality.equal @@ int32 2 @@ (Lists.length @@ var "l")))

recordDef :: TElement (Name -> Term -> Maybe [Field])
recordDef = decodeNominalFunctionDefinition "record" (listT fieldT) $
  matchNominal _Term_record Core.recordTypeName Core.recordFields

setDef :: TElement (Term -> Maybe (S.Set Term))
setDef = decodeFunctionDefinition "set" termT (setT termT) $
  matchTermVariant _Term_set

stringDef :: TElement (Term -> Maybe String)
stringDef = decodeFunctionDefinition "string" termT stringT $
  compose2 (ref literalDef) (ref stringLiteralDef)

stringLiteralDef :: TElement (Literal -> Maybe String)
stringLiteralDef = decodeFunctionDefinition "stringLiteral" literalT stringT $
  matchVariant _Literal _Literal_string

uint8Def :: TElement (Term -> Maybe Int)
uint8Def = decodeFunctionDefinition "uint8" termT uint8T $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint8ValueDef)

uint8ValueDef :: TElement (IntegerValue -> Maybe Int)
uint8ValueDef = decodeFunctionDefinition "uint8Value" integerValueT uint8T $
  matchVariant _IntegerValue _IntegerValue_uint8

uint16Def :: TElement (Term -> Maybe Int)
uint16Def = decodeFunctionDefinition "uint16" termT uint16T $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint16ValueDef)

uint16ValueDef :: TElement (IntegerValue -> Maybe Int)
uint16ValueDef = decodeFunctionDefinition "uint16Value" integerValueT uint16T $
  matchVariant _IntegerValue _IntegerValue_uint16

uint32Def :: TElement (Term -> Maybe Int)
uint32Def = decodeFunctionDefinition "uint32" termT uint32T $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint32ValueDef)

uint32ValueDef :: TElement (IntegerValue -> Maybe Int)
uint32ValueDef = decodeFunctionDefinition "uint32Value" integerValueT uint32T $
  matchVariant _IntegerValue _IntegerValue_uint32

uint64Def :: TElement (Term -> Maybe Int)
uint64Def = decodeFunctionDefinition "uint64" termT uint64T $
  compose3 (ref literalDef) (ref integerLiteralDef) (ref uint64ValueDef)

uint64ValueDef :: TElement (IntegerValue -> Maybe Int)
uint64ValueDef = decodeFunctionDefinition "uint64Value" integerValueT uint64T $
  matchVariant _IntegerValue _IntegerValue_uint64

unitDef :: TElement (Term -> Maybe ())
unitDef = decodeFunctionDefinition "unit" termT unitT $
  lambda "term" $ Optionals.map
    @@ (constant unit)
    @@ (ref recordDef @@ Core.name _Unit @@ var "term")

unitVariantDef :: TElement (Name -> Term -> Maybe Name)
unitVariantDef = decodeDefinition "unitVariant" $
  functionN [nameT, termT, optionalT nameT] $
  lambda "tname" $ lambda "term" $ Optionals.map
    @@ Core.fieldName
    @@ (ref variantDef @@ var "tname" @@ var "term")

variableDef :: TElement (Term -> Y.Maybe Name)
variableDef = decodeFunctionDefinition "variable" termT nameT $
  matchTermVariant _Term_variable <.> ref fullyStripTermDef

variantDef :: TElement (Name -> Term -> Maybe Field)
variantDef = decodeNominalFunctionDefinition "variant" fieldT $
  matchNominal _Term_union Core.injectionTypeName Core.injectionField

wrapDef :: TElement (Name -> Term -> Maybe Term)
wrapDef = decodeNominalFunctionDefinition "wrap" termT $
  matchNominal _Term_wrap Core.wrappedTermTypeName Core.wrappedTermObject

--

compose2 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (a -> Maybe c)
compose2 f g = Optionals.compose @@ f @@ g

compose3 :: TTerm (a -> Maybe b) -> TTerm (b -> Maybe c) -> TTerm (c -> Maybe d) -> TTerm (a -> Maybe d)
compose3 f g h = Optionals.compose @@ (Optionals.compose @@ f @@ g) @@ h

matchNominal :: Name -> TTerm (a -> Name) -> TTerm (a -> b) -> TTerm (Name -> Term -> Maybe b)
matchNominal fname getName getB = ref nominalDef @@ getName @@ getB @@ matchTermVariant fname

matchTermVariant :: Name -> TTerm (Term -> Maybe a)
matchTermVariant fname = matchVariant _Term fname <.> ref fullyStripTermDef

matchVariant :: Name -> Name -> TTerm (a -> Maybe b)
matchVariant tname fname = match tname (Just nothing) [TCase fname --> Optionals.pure]
