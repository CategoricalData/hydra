-- | A module for decoding terms to native objects

module Hydra.Decode where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Strip as Strip
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

bigfloat :: (Core.Term -> Maybe Double)
bigfloat = (Optionals.compose (Optionals.compose literal floatLiteral) bigfloatValue)

bigfloatValue :: (Core.FloatValue -> Maybe Double)
bigfloatValue x = case x of
  Core.FloatValueBigfloat v75 -> (Optionals.pure v75)
  _ -> Nothing

bigint :: (Core.Term -> Maybe Integer)
bigint = (Optionals.compose (Optionals.compose literal integerLiteral) bigintValue)

bigintValue :: (Core.IntegerValue -> Maybe Integer)
bigintValue x = case x of
  Core.IntegerValueBigint v76 -> (Optionals.pure v76)
  _ -> Nothing

binary :: (Core.Term -> Maybe String)
binary = (Optionals.compose literal binaryLiteral)

binaryLiteral :: (Core.Literal -> Maybe String)
binaryLiteral x = case x of
  Core.LiteralBinary v77 -> (Optionals.pure v77)
  _ -> Nothing

boolean :: (Core.Term -> Maybe Bool)
boolean = (Optionals.compose literal booleanLiteral)

booleanLiteral :: (Core.Literal -> Maybe Bool)
booleanLiteral x = case x of
  Core.LiteralBoolean v78 -> (Optionals.pure v78)
  _ -> Nothing

casesCase :: (Core.Name -> Core.Name -> Core.Term -> Maybe Core.Term)
casesCase tname fname = (Optionals.compose (cases tname) (field fname))

cases :: (Core.Name -> Core.Term -> Maybe [Core.Field])
cases = (nominal Core.caseStatementTypeName Core.caseStatementCases (Optionals.compose (Optionals.compose (\x -> (\x -> case x of
  Core.TermFunction v79 -> (Optionals.pure v79)
  _ -> Nothing) (Strip.fullyStripTerm x)) (\x -> case x of
  Core.FunctionElimination v80 -> (Optionals.pure v80)
  _ -> Nothing)) (\x -> case x of
  Core.EliminationUnion v81 -> (Optionals.pure v81)
  _ -> Nothing)))

field :: (Core.Name -> [Core.Field] -> Maybe Core.Term)
field fname fields =  
  let matches = (Lists.filter (\f -> Equality.equal (Core.fieldName f) fname) fields)
  in (Logic.ifElse (Just (Core.fieldTerm (Lists.head matches))) Nothing (Equality.equal 1 (Lists.length matches)))

float32 :: (Core.Term -> Maybe Float)
float32 = (Optionals.compose (Optionals.compose literal floatLiteral) float32Value)

float32Value :: (Core.FloatValue -> Maybe Float)
float32Value x = case x of
  Core.FloatValueFloat32 v82 -> (Optionals.pure v82)
  _ -> Nothing

float64 :: (Core.Term -> Maybe Double)
float64 = (Optionals.compose (Optionals.compose literal floatLiteral) float64Value)

float64Value :: (Core.FloatValue -> Maybe Double)
float64Value x = case x of
  Core.FloatValueFloat64 v83 -> (Optionals.pure v83)
  _ -> Nothing

floatLiteral :: (Core.Literal -> Maybe Core.FloatValue)
floatLiteral x = case x of
  Core.LiteralFloat v84 -> (Optionals.pure v84)
  _ -> Nothing

int16 :: (Core.Term -> Maybe Int16)
int16 = (Optionals.compose (Optionals.compose literal integerLiteral) int16Value)

int16Value :: (Core.IntegerValue -> Maybe Int16)
int16Value x = case x of
  Core.IntegerValueInt16 v85 -> (Optionals.pure v85)
  _ -> Nothing

int32 :: (Core.Term -> Maybe Int)
int32 = (Optionals.compose (Optionals.compose literal integerLiteral) int32Value)

int32Value :: (Core.IntegerValue -> Maybe Int)
int32Value x = case x of
  Core.IntegerValueInt32 v86 -> (Optionals.pure v86)
  _ -> Nothing

int64 :: (Core.Term -> Maybe Int64)
int64 = (Optionals.compose (Optionals.compose literal integerLiteral) int64Value)

int64Value :: (Core.IntegerValue -> Maybe Int64)
int64Value x = case x of
  Core.IntegerValueInt64 v87 -> (Optionals.pure v87)
  _ -> Nothing

int8 :: (Core.Term -> Maybe Int8)
int8 = (Optionals.compose (Optionals.compose literal integerLiteral) int8Value)

int8Value :: (Core.IntegerValue -> Maybe Int8)
int8Value x = case x of
  Core.IntegerValueInt8 v88 -> (Optionals.pure v88)
  _ -> Nothing

integerLiteral :: (Core.Literal -> Maybe Core.IntegerValue)
integerLiteral x = case x of
  Core.LiteralInteger v89 -> (Optionals.pure v89)
  _ -> Nothing

lambda :: (Core.Term -> Maybe Core.Lambda)
lambda = (Optionals.compose (\x -> (\x -> case x of
  Core.TermFunction v90 -> (Optionals.pure v90)
  _ -> Nothing) (Strip.fullyStripTerm x)) (\x -> case x of
  Core.FunctionLambda v91 -> (Optionals.pure v91)
  _ -> Nothing))

letBinding :: (Core.Name -> Core.Term -> Maybe Core.LetBinding)
letBinding fname term = (Optionals.bind (Optionals.map Core.letBindings (letTerm term)) (letBindingWithKey fname))

letBindingWithKey :: (Core.Name -> [Core.LetBinding] -> Maybe Core.LetBinding)
letBindingWithKey fname bindings =  
  let matches = (Lists.filter (\b -> Equality.equal (Core.letBindingName b) fname) bindings)
  in (Logic.ifElse (Just (Lists.head matches)) Nothing (Equality.equal 1 (Lists.length matches)))

letTerm :: (Core.Term -> Maybe Core.Let)
letTerm x = ((\x -> case x of
  Core.TermLet v92 -> (Optionals.pure v92)
  _ -> Nothing) (Strip.fullyStripTerm x))

list :: (Core.Term -> Maybe [Core.Term])
list x = ((\x -> case x of
  Core.TermList v93 -> (Optionals.pure v93)
  _ -> Nothing) (Strip.fullyStripTerm x))

literal :: (Core.Term -> Maybe Core.Literal)
literal x = ((\x -> case x of
  Core.TermLiteral v94 -> (Optionals.pure v94)
  _ -> Nothing) (Strip.fullyStripTerm x))

map :: (Core.Term -> Maybe (Map Core.Term Core.Term))
map x = ((\x -> case x of
  Core.TermMap v95 -> (Optionals.pure v95)
  _ -> Nothing) (Strip.fullyStripTerm x))

name :: (Core.Term -> Maybe Core.Name)
name term = (Optionals.map (\s -> Core.Name s) (Optionals.bind (wrap (Core.Name "hydra/core.Name") term) string))

nominal :: ((a -> Core.Name) -> (a -> b) -> (c -> Maybe a) -> Core.Name -> c -> Maybe b)
nominal getName getB getA expected = (Optionals.compose getA (\a -> Logic.ifElse (Just (getB a)) Nothing (Equality.equal (getName a) expected)))

optCases :: (Core.Term -> Maybe Core.OptionalCases)
optCases = (Optionals.compose (Optionals.compose (\x -> (\x -> case x of
  Core.TermFunction v96 -> (Optionals.pure v96)
  _ -> Nothing) (Strip.fullyStripTerm x)) (\x -> case x of
  Core.FunctionElimination v97 -> (Optionals.pure v97)
  _ -> Nothing)) (\x -> case x of
  Core.EliminationOptional v98 -> (Optionals.pure v98)
  _ -> Nothing))

optCasesJust :: (Core.Term -> Maybe Core.Term)
optCasesJust term = (Optionals.map Core.optionalCasesJust (optCases term))

optCasesNothing :: (Core.Term -> Maybe Core.Term)
optCasesNothing term = (Optionals.map Core.optionalCasesNothing (optCases term))

optional :: (Core.Term -> Maybe (Maybe Core.Term))
optional x = ((\x -> case x of
  Core.TermOptional v99 -> (Optionals.pure v99)
  _ -> Nothing) (Strip.fullyStripTerm x))

pair :: (Core.Term -> Maybe (Core.Term, Core.Term))
pair = (Optionals.compose (\x -> (\x -> case x of
  Core.TermProduct v100 -> (Optionals.pure v100)
  _ -> Nothing) (Strip.fullyStripTerm x)) (\l -> Logic.ifElse (Just (Lists.at 0 l, (Lists.at 1 l))) Nothing (Equality.equal 2 (Lists.length l))))

record :: (Core.Name -> Core.Term -> Maybe [Core.Field])
record = (nominal Core.recordTypeName Core.recordFields (\x -> (\x -> case x of
  Core.TermRecord v101 -> (Optionals.pure v101)
  _ -> Nothing) (Strip.fullyStripTerm x)))

set :: (Core.Term -> Maybe (Set Core.Term))
set x = ((\x -> case x of
  Core.TermSet v102 -> (Optionals.pure v102)
  _ -> Nothing) (Strip.fullyStripTerm x))

string :: (Core.Term -> Maybe String)
string = (Optionals.compose literal stringLiteral)

stringLiteral :: (Core.Literal -> Maybe String)
stringLiteral x = case x of
  Core.LiteralString v103 -> (Optionals.pure v103)
  _ -> Nothing

uint16 :: (Core.Term -> Maybe Int)
uint16 = (Optionals.compose (Optionals.compose literal integerLiteral) uint16Value)

uint16Value :: (Core.IntegerValue -> Maybe Int)
uint16Value x = case x of
  Core.IntegerValueUint16 v104 -> (Optionals.pure v104)
  _ -> Nothing

uint32 :: (Core.Term -> Maybe Int64)
uint32 = (Optionals.compose (Optionals.compose literal integerLiteral) uint32Value)

uint32Value :: (Core.IntegerValue -> Maybe Int64)
uint32Value x = case x of
  Core.IntegerValueUint32 v105 -> (Optionals.pure v105)
  _ -> Nothing

uint64 :: (Core.Term -> Maybe Integer)
uint64 = (Optionals.compose (Optionals.compose literal integerLiteral) uint64Value)

uint64Value :: (Core.IntegerValue -> Maybe Integer)
uint64Value x = case x of
  Core.IntegerValueUint64 v106 -> (Optionals.pure v106)
  _ -> Nothing

uint8 :: (Core.Term -> Maybe Int16)
uint8 = (Optionals.compose (Optionals.compose literal integerLiteral) uint8Value)

uint8Value :: (Core.IntegerValue -> Maybe Int16)
uint8Value x = case x of
  Core.IntegerValueUint8 v107 -> (Optionals.pure v107)
  _ -> Nothing

unit :: (Core.Term -> Maybe ())
unit term = (Optionals.map (\_ -> ()) (record (Core.Name "hydra/core.Unit") term))

unitVariant :: (Core.Name -> Core.Term -> Maybe Core.Name)
unitVariant tname term = (Optionals.map Core.fieldName (variant tname term))

variable :: (Core.Term -> Maybe Core.Name)
variable x = ((\x -> (\x -> case x of
  Core.TermVariable v108 -> (Optionals.pure v108)
  _ -> Nothing) (Strip.fullyStripTerm x)) (Strip.fullyStripTerm x))

variant :: (Core.Name -> Core.Term -> Maybe Core.Field)
variant = (nominal Core.injectionTypeName Core.injectionField (\x -> (\x -> case x of
  Core.TermUnion v109 -> (Optionals.pure v109)
  _ -> Nothing) (Strip.fullyStripTerm x)))

wrap :: (Core.Name -> Core.Term -> Maybe Core.Term)
wrap = (nominal Core.wrappedTermTypeName Core.wrappedTermObject (\x -> (\x -> case x of
  Core.TermWrap v110 -> (Optionals.pure v110)
  _ -> Nothing) (Strip.fullyStripTerm x)))