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
import Data.String(IsString(..))


instance IsString (Term m) where fromString = string

annot :: m -> Term m -> Term m
annot ann t = TermAnnotated $ Annotated t ann

apply :: Term m -> Term m -> Term m
apply func arg = TermApplication $ Application func arg

bigfloat :: Double -> Term m
bigfloat = float . FloatValueBigfloat

bigint :: Integer -> Term m
bigint = integer . IntegerValueBigint . fromIntegral

binaryTerm :: String -> Term m
binaryTerm = TermLiteral . LiteralBinary

boolean :: Bool -> Term m
boolean b = TermLiteral $ LiteralBoolean b

cases :: [Field m] -> Term m
cases = TermFunction . FunctionElimination . EliminationUnion

compareTo :: Term m -> Term m
compareTo = TermFunction . FunctionCompareTo

compose :: Term m -> Term m -> Term m
compose f2 f1 = lambda var $ apply f2 (apply f1 (variable var))
  where var = "x"

constFunction :: Term m -> Term m
constFunction = lambda "_"

delta :: Term m
delta = TermFunction $ FunctionElimination EliminationElement

element :: Name -> Term m
element = TermElement

elementRef :: Element a -> Term m
elementRef = apply delta . TermElement . elementName

elementRefByName :: Name -> Term m
elementRefByName = apply delta . TermElement

eliminateNominal :: Name -> Term m
eliminateNominal = TermFunction . FunctionElimination . EliminationNominal

expectInt32 :: Show m => Term m -> Result Int
expectInt32 term = case termExpr term of
  TermLiteral (LiteralInteger (IntegerValueInt32 v)) -> pure v
  _ -> fail $ "expected an int32, got " ++ show term

expectList :: Show m => (Term m -> Result a) -> Term m -> Result [a]
expectList f term = expectListPoly term >>= CM.mapM f

expectListPoly :: Show m => Term m -> Result [Term m]
expectListPoly term = case termExpr term of
  TermList els -> pure els
  _ -> fail $ "expected a list, got " ++ show term

expectLiteral :: Show m => Term m -> Result Literal
expectLiteral term = case termExpr term of
  TermLiteral av -> pure av
  _ -> fail $ "expected a literal value, got " ++ show term

expectNArgs :: Int -> [Term m] -> Result ()
expectNArgs n args = if L.length args /= n
  then fail $ "expected " ++ show n ++ " arguments, but found " ++ show (L.length args)
  else pure ()

expectRecord :: Show m => Term m -> Result [Field m]
expectRecord term = case termExpr term of
  TermRecord fields -> pure fields
  _ -> fail $ "expected a record, got " ++ show term

expectSet :: (Ord a, Show m) => (Term m -> Result a) -> Term m -> Result (S.Set a)
expectSet f term = case termExpr term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> fail $ "expected a set, got " ++ show term

expectString :: Show m => Term m -> Result String
expectString term = case termExpr term of
  TermLiteral (LiteralString s) -> pure s
  _ -> fail $ "expected a string, got " ++ show term

expectUnion :: Show m => Term m -> Result (Field m)
expectUnion term = case termExpr term of
  TermUnion field -> pure field
  _ -> fail $ "expected a union, got " ++ show term

field :: String -> Term m -> Field m
field n = Field (FieldName n)

fieldsToMap :: [Field m] -> M.Map FieldName (Term m)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

float32 :: Float -> Term m
float32 = float . FloatValueFloat32

float64 :: Double -> Term m
float64 = float . FloatValueFloat64

float :: FloatValue -> Term m
float = TermLiteral . LiteralFloat

int16 :: Int16 -> Term m
int16 = integer . IntegerValueInt16 . fromIntegral

int32 :: Int -> Term m
int32 = integer . IntegerValueInt32

int64 :: Int64 -> Term m
int64 = integer . IntegerValueInt64 . fromIntegral

int8 :: Int8 -> Term m
int8 = integer . IntegerValueInt8 . fromIntegral

integer :: IntegerValue -> Term m
integer = TermLiteral . LiteralInteger

lambda :: String -> Term m -> Term m
lambda param body = TermFunction $ FunctionLambda $ Lambda (Variable param) body

letTerm :: Variable -> Term m -> Term m -> Term m
letTerm v t1 t2 = TermLet $ Let v t1 t2

list :: [Term m] -> Term m
list = TermList

literal :: Literal -> Term m
literal = TermLiteral

map :: M.Map (Term m) (Term m) -> Term m
map = TermMap

mapTerm :: M.Map (Term m) (Term m) -> Term m
mapTerm = TermMap

match :: [(FieldName, Term m)] -> Term m
match = cases . fmap toField
  where
    toField (name, term) = Field name term

matchOptional :: Term m -> Term m -> Term m
matchOptional n j = TermFunction $ FunctionElimination $ EliminationOptional $ OptionalCases n j

matchWithVariants :: [(FieldName, FieldName)] -> Term m
matchWithVariants = cases . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant to

nominal :: Name -> Term m -> Term m
nominal name term = TermNominal $ Named name term

optional :: Y.Maybe (Term m) -> Term m
optional = TermOptional

primitive :: Name -> Term m
primitive = TermFunction . FunctionPrimitive

projection :: FieldName -> Term m
projection = TermFunction . FunctionElimination . EliminationRecord

record :: [Field m] -> Term m
record = TermRecord

requireField :: M.Map FieldName (Term m) -> FieldName -> Result (Term m)
requireField fields fname = Y.maybe err ResultSuccess $ M.lookup fname fields
  where
    err = fail $ "no such field: " ++ unFieldName fname

set :: S.Set (Term m) -> Term m
set = TermSet

stringList :: [String] -> Term m
stringList l = list (string <$> l)

stringSet :: Ord m => S.Set String -> Term m
stringSet strings = set $ S.fromList $ string <$> S.toList strings

string :: String -> Term m
string = TermLiteral . LiteralString

uint16 :: Integer -> Term m
uint16 = integer . IntegerValueUint16 . fromIntegral

uint32 :: Integer -> Term m
uint32 = integer . IntegerValueUint32 . fromIntegral

uint64 :: Integer -> Term m
uint64 = integer . IntegerValueUint64 . fromIntegral

uint8 :: Integer -> Term m
uint8 = integer . IntegerValueUint8 . fromIntegral

union :: Field m -> Term m
union = TermUnion

unit :: Term m
unit = TermRecord []

unitVariant :: FieldName -> Term m
unitVariant fname = variant fname unit

variable :: String -> Term m
variable = TermVariable . Variable

variant :: FieldName -> Term m -> Term m
variant fname term = TermUnion $ Field fname term

withFunction :: FieldName -> Element a -> Term m
withFunction name el = lambda var $ variant name $ apply (elementRef el) (variable var)
  where var = "x"

withVariant :: FieldName -> Term m
withVariant = constFunction . unitVariant
