module Hydra.Impl.Haskell.Dsl.Terms where

import Hydra.Core
import Hydra.Graph
import Hydra.Steps
import Hydra.Impl.Haskell.Extras
import Prelude hiding (map)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int


apply :: Default a => Term a -> Term a -> Term a
apply func arg = defaultTerm $ TermExprApplication $ Application func arg

atomic :: Default a => Literal -> Term a
atomic = defaultTerm . TermExprLiteral

bigfloat :: Default a => Double -> Term a
bigfloat = float . FloatValueBigfloat

bigint :: Default a => Integer -> Term a
bigint = integer . IntegerValueBigint . fromIntegral

binaryTerm :: Default a => String -> Term a
binaryTerm = defaultTerm . TermExprLiteral . LiteralBinary

boolean :: Default a => Bool -> Term a
boolean b = defaultTerm $ TermExprLiteral $ LiteralBoolean $ if b then BooleanValueTrue else BooleanValueFalse

cases :: Default a => [Field a] -> Term a
cases = defaultTerm . TermExprFunction . FunctionElimination . EliminationUnion

compareTo :: Default a => Term a -> Term a
compareTo = defaultTerm . TermExprFunction . FunctionCompareTo

compose :: Default a => Term a -> Term a -> Term a
compose f2 f1 = lambda var $ apply f2 (apply f1 (variable var))
  where var = "x"

constFunction :: Default a => Term a -> Term a
constFunction = lambda "_"

delta :: Default a => Term a
delta = defaultTerm $ TermExprFunction $ FunctionElimination EliminationElement

defaultTerm  :: Default a => TermExpr a -> Term a
defaultTerm e = Term e dflt

element :: Default a => Name -> Term a
element = defaultTerm . TermExprElement

elementRef :: Default a => Element a -> Term a
elementRef el = apply delta $ defaultTerm $ TermExprElement $ elementName el

elementRefByName :: Default a => Name -> Term a
elementRefByName name = apply delta $ defaultTerm $ TermExprElement name

eliminateNominal :: Default a => Name -> Term a
eliminateNominal name = defaultTerm $ TermExprFunction $ FunctionElimination $ EliminationNominal name

expectInt32 :: Show a => Term a -> Result Int
expectInt32 term = case termExpr term of
  TermExprLiteral (LiteralInteger (IntegerValueInt32 v)) -> pure v
  _ -> fail $ "expected an int32, got " ++ show term

expectList :: Show m => (Term m -> Result a) -> Term m -> Result [a]
expectList f term = expectListPoly term >>= CM.mapM f

expectListPoly :: Show m => Term m -> Result [Term m]
expectListPoly term = case termExpr term of
  TermExprList els -> pure els
  _ -> fail $ "expected a list, got " ++ show term

expectLiteral :: Show a => Term a -> Result Literal
expectLiteral term = case termExpr term of
  TermExprLiteral av -> pure av
  _ -> fail $ "expected a literal value, got " ++ show term

expectNArgs :: Int -> [Term a] -> Result ()
expectNArgs n args = if L.length args /= n
  then fail $ "expected " ++ show n ++ " arguments, but found " ++ show (L.length args)
  else pure ()

expectRecord :: Show a => Term a -> Result [Field a]
expectRecord term = case termExpr term of
  TermExprRecord fields -> pure fields
  _ -> fail $ "expected a record, got " ++ show term

expectSet :: (Ord a, Show m) => (Term m -> Result a) -> Term m -> Result (S.Set a)
expectSet f term = case termExpr term of
  TermExprSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> fail $ "expected a set, got " ++ show term

expectString :: Show a => Term a -> Result String
expectString term = case termExpr term of
  TermExprLiteral (LiteralString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnion :: Show a => Term a -> Result (Field a)
expectUnion term = case termExpr term of
  TermExprUnion field -> pure field
  _ -> fail $ "expected a union, got " ++ show term

field :: String -> Term a -> Field a
field n = Field (FieldName n)

fieldsToMap :: [Field a] -> M.Map FieldName (Term a)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

float32 :: Default a => Float -> Term a
float32 = float . FloatValueFloat32

float64 :: Default a => Double -> Term a
float64 = float . FloatValueFloat64

float :: Default a => FloatValue -> Term a
float = defaultTerm . TermExprLiteral . LiteralFloat

int16 :: Default a => Int16 -> Term a
int16 = integer . IntegerValueInt16 . fromIntegral

int32 :: Default a => Int -> Term a
int32 = integer . IntegerValueInt32

int64 :: Default a => Int64 -> Term a
int64 = integer . IntegerValueInt64 . fromIntegral

int8 :: Default a => Int8 -> Term a
int8 = integer . IntegerValueInt8 . fromIntegral

integer :: Default a => IntegerValue -> Term a
integer = defaultTerm . TermExprLiteral . LiteralInteger

lambda :: Default a => String -> Term a -> Term a
lambda param body = defaultTerm $ TermExprFunction $ FunctionLambda $ Lambda (Variable param) body

letTerm :: Default a => Variable -> Term a -> Term a -> Term a
letTerm v t1 t2 = defaultTerm $ TermExprLet $ Let v t1 t2

list :: Default a => [Term a] -> Term a
list = defaultTerm . TermExprList

map :: Default a => M.Map (Term a) (Term a) -> Term a
map = defaultTerm . TermExprMap

mapTerm :: Default a => M.Map (Term a) (Term a) -> Term a
mapTerm = defaultTerm . TermExprMap

match :: Default a => [(FieldName, Term a)] -> Term a
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchOptional :: Default a => Term a -> Term a -> Term a
matchOptional n j = defaultTerm $ TermExprFunction $ FunctionElimination $ EliminationOptional $ OptionalCases n j

matchWithVariants :: Default a => [(FieldName, FieldName)] -> Term a
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominal :: Default a => Name -> Term a -> Term a
nominal name term = defaultTerm $ TermExprNominal $ Named name term

optional :: Default a => Y.Maybe (Term a) -> Term a
optional = defaultTerm . TermExprOptional

primitive :: Default a => Name -> Term a
primitive = defaultTerm . TermExprFunction . FunctionPrimitive

projection :: Default a => FieldName -> Term a
projection = defaultTerm . TermExprFunction . FunctionElimination . EliminationRecord

record :: Default a => [Field a] -> Term a
record = defaultTerm . TermExprRecord

requireField :: M.Map FieldName (Term a) -> FieldName -> Result (Term a)
requireField fields fname = Y.maybe error ResultSuccess $ M.lookup fname fields
  where
    error = fail $ "no such field: " ++ unFieldName fname

set :: Default a => S.Set (Term a) -> Term a
set = defaultTerm . TermExprSet

setMeta :: m -> Term m -> Term m
setMeta meta dat = dat {termMeta = meta}

stringList :: Default a => [String] -> Term a
stringList l = list (string <$> l)

stringSet :: (Default a, Ord a) => S.Set String -> Term a
stringSet strings = set $ S.fromList $ string <$> S.toList strings

string :: Default a => String -> Term a
string = defaultTerm . TermExprLiteral . LiteralString

uint16 :: Default a => Integer -> Term a
uint16 = integer . IntegerValueUint16 . fromIntegral

uint32 :: Default a => Integer -> Term a
uint32 = integer . IntegerValueUint32 . fromIntegral

uint64 :: Default a => Integer -> Term a
uint64 = integer . IntegerValueUint64 . fromIntegral

uint8 :: Default a => Integer -> Term a
uint8 = integer . IntegerValueUint8 . fromIntegral

union :: Default a => Field a -> Term a
union field = defaultTerm $ TermExprUnion field

unit :: Default a => Term a
unit = defaultTerm $ TermExprRecord []

unitVariant :: Default a => FieldName -> Term a
unitVariant fname = variant fname unit

variable :: Default a => String -> Term a
variable = defaultTerm . TermExprVariable . Variable

variant :: Default a => FieldName -> Term a -> Term a
variant fname term = defaultTerm $ TermExprUnion $ Field fname term

withFunction :: Default a => FieldName -> Element a -> Term a
withFunction name el = lambda var $ variant name $ apply (elementRef el) (variable var)
  where var = "x"

withVariant :: Default a => FieldName -> Term a
withVariant = constFunction . unitVariant
