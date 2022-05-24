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


apply :: Default a => Data a -> Data a -> Data a
apply func arg = defaultData $ DataTermApplication $ Application func arg

atomic :: Default a => Literal -> Data a
atomic = defaultData . DataTermLiteral

bigfloatValue :: Default a => Double -> Data a
bigfloatValue = floatValue . FloatValueBigfloat

bigintValue :: Default a => Integer -> Data a
bigintValue = integerValue . IntegerValueBigint . fromIntegral

binaryData :: Default a => String -> Data a
binaryData = defaultData . DataTermLiteral . LiteralBinary

booleanValue :: Default a => Bool -> Data a
booleanValue b = defaultData $ DataTermLiteral $ LiteralBoolean $ if b then BooleanValueTrue else BooleanValueFalse

cases :: Default a => [Field a] -> Data a
cases = defaultData . DataTermFunction . FunctionCases

compareTo :: Default a => Data a -> Data a
compareTo = defaultData . DataTermFunction . FunctionCompareTo

compose :: Default a => Data a -> Data a -> Data a
compose f2 f1 = lambda var $ apply f2 (apply f1 (variable var))
  where var = "x"

constFunction :: Default a => Data a -> Data a
constFunction = lambda "_"

delta :: Default a => Data a
delta = defaultData $ DataTermFunction FunctionDelta

defaultData  :: Default a => DataTerm a -> Data a
defaultData e = Data e dflt

element :: Default a => Name -> Data a
element = defaultData . DataTermElement

elementRef :: Default a => Element a -> Data a
elementRef el = apply delta $ defaultData $ DataTermElement $ elementName el

elementRefByName :: Default a => Name -> Data a
elementRefByName name = apply delta $ defaultData $ DataTermElement name

expectInt32 :: Show a => Data a -> Result Int
expectInt32 term = case dataTerm term of
  DataTermLiteral (LiteralInteger (IntegerValueInt32 v)) -> pure v
  _ -> fail $ "expected an int32, got " ++ show term

expectList :: Show m => (Data m -> Result a) -> Data m -> Result [a]
expectList f term = expectListPoly term >>= CM.mapM f

expectListPoly :: Show m => Data m -> Result [Data m]
expectListPoly term = case dataTerm term of
  DataTermList els -> pure els
  _ -> fail $ "expected a list, got " ++ show term

expectLiteral :: Show a => Data a -> Result Literal
expectLiteral term = case dataTerm term of
  DataTermLiteral av -> pure av
  _ -> fail $ "expected a literal value, got " ++ show term

expectNArgs :: Int -> [Data a] -> Result ()
expectNArgs n args = if L.length args /= n
  then fail $ "expected " ++ show n ++ " arguments, but found " ++ show (L.length args)
  else pure ()

expectRecord :: Show a => Data a -> Result [Field a]
expectRecord term = case dataTerm term of
  DataTermRecord fields -> pure fields
  _ -> fail $ "expected a record, got " ++ show term

expectSet :: (Ord a, Show m) => (Data m -> Result a) -> Data m -> Result (S.Set a)
expectSet f term = case dataTerm term of
  DataTermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> fail $ "expected a set, got " ++ show term

expectString :: Show a => Data a -> Result String
expectString term = case dataTerm term of
  DataTermLiteral (LiteralString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnion :: Show a => Data a -> Result (Field a)
expectUnion term = case dataTerm term of
  DataTermUnion field -> pure field
  _ -> fail $ "expected a union, got " ++ show term

fieldsToMap :: [Field a] -> M.Map FieldName (Data a)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

float32Value :: Default a => Float -> Data a
float32Value = floatValue . FloatValueFloat32

float64Value :: Default a => Double -> Data a
float64Value = floatValue . FloatValueFloat64

floatValue :: Default a => FloatValue -> Data a
floatValue = defaultData . DataTermLiteral . LiteralFloat

int16Value :: Default a => Int -> Data a
int16Value = integerValue . IntegerValueInt16 . fromIntegral

int32Value :: Default a => Int -> Data a
int32Value = integerValue . IntegerValueInt32

int64Value :: Default a => Integer -> Data a
int64Value = integerValue . IntegerValueInt64

int8Value :: Default a => Int -> Data a
int8Value = integerValue . IntegerValueInt8 . fromIntegral

integerValue :: Default a => IntegerValue -> Data a
integerValue = defaultData . DataTermLiteral . LiteralInteger

lambda :: Default a => String -> Data a -> Data a
lambda param body = defaultData $ DataTermFunction $ FunctionLambda $ Lambda (Variable param) body

letData :: Default a => Variable -> Data a -> Data a -> Data a
letData v t1 t2 = defaultData $ DataTermLet $ Let v t1 t2

list :: Default a => [Data a] -> Data a
list = defaultData . DataTermList

map :: Default a => M.Map (Data a) (Data a) -> Data a
map = defaultData . DataTermMap

mapData :: Default a => M.Map (Data a) (Data a) -> Data a
mapData = defaultData . DataTermMap

match :: Default a => [(FieldName, Data a)] -> Data a
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchWithVariants :: Default a => [(FieldName, FieldName)] -> Data a
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominal :: Default a => Name -> Data a -> Data a
nominal name term = defaultData $ DataTermNominal $ Named name term

optional :: Default a => Y.Maybe (Data a) -> Data a
optional = defaultData . DataTermOptional

primitive :: Default a => Name -> Data a
primitive = defaultData . DataTermFunction . FunctionPrimitive

projection :: Default a => FieldName -> Data a
projection = defaultData . DataTermFunction . FunctionProjection

record :: Default a => [Field a] -> Data a
record = defaultData . DataTermRecord

requireField :: M.Map FieldName (Data a) -> FieldName -> Result (Data a)
requireField fields fname = Y.maybe error ResultSuccess $ M.lookup fname fields
  where
    error = fail $ "no such field: " ++ unFieldName fname

set :: Default a => S.Set (Data a) -> Data a
set = defaultData . DataTermSet

setMeta :: m -> Data m -> Data m
setMeta meta dat = dat {dataMeta = meta}

stringList :: Default a => [String] -> Data a
stringList l = list (stringValue <$> l)

stringSet :: (Default a, Ord a) => S.Set String -> Data a
stringSet strings = set $ S.fromList $ stringValue <$> S.toList strings

stringValue :: Default a => String -> Data a
stringValue = defaultData . DataTermLiteral . LiteralString

uint16Value :: Default a => Integer -> Data a
uint16Value = integerValue . IntegerValueUint16 . fromIntegral

uint32Value :: Default a => Integer -> Data a
uint32Value = integerValue . IntegerValueUint32 . fromIntegral

uint64Value :: Default a => Integer -> Data a
uint64Value = integerValue . IntegerValueUint64 . fromIntegral

uint8Value :: Default a => Integer -> Data a
uint8Value = integerValue . IntegerValueUint8 . fromIntegral

union :: Default a => Field a -> Data a
union field = defaultData $ DataTermUnion field

unitData :: Default a => Data a
unitData = defaultData $ DataTermRecord []

unitVariant :: Default a => FieldName -> Data a
unitVariant fname = variant fname unitData

variable :: Default a => String -> Data a
variable = defaultData . DataTermVariable . Variable

variant :: Default a => FieldName -> Data a -> Data a
variant fname term = defaultData $ DataTermUnion $ Field fname term

withFunction :: Default a => FieldName -> Element a -> Data a
withFunction name el = lambda var $ variant name $ apply (elementRef el) (variable var)
  where var = "x"

withVariant :: Default a => FieldName -> Data a
withVariant = constFunction . unitVariant
