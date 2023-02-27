-- | A DSL for constructing Hydra terms

module Hydra.Dsl.Terms where

import Hydra.Common
import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import Hydra.Lexical
import Hydra.Monads
import qualified Hydra.Dsl.Literals as Literals

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
bigfloat = literal . Literals.bigfloat

bigint :: Integer -> Term m
bigint = literal . Literals.bigint

binary :: String -> Term m
binary = literal . Literals.binary

boolean :: Bool -> Term m
boolean = literal . Literals.boolean

cases :: Name -> [Field m] -> Term m
cases n fields = TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement n fields

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

elimination :: Elimination m -> Term m
elimination = TermFunction . FunctionElimination

expectBinary :: Show m => Term m -> Flow s String
expectBinary = expectLiteral Literals.expectBinary

expectBoolean :: Show m => Term m -> Flow s Bool
expectBoolean = expectLiteral Literals.expectBoolean

expectFloat32 :: Show m => Term m -> Flow s Float
expectFloat32 = expectLiteral Literals.expectFloat32

expectFloat64 :: Show m => Term m -> Flow s Double
expectFloat64 = expectLiteral Literals.expectFloat64

expectInt32 :: Show m => Term m -> Flow s Int
expectInt32 = expectLiteral Literals.expectInt32

expectInt64 :: Show m => Term m -> Flow s Integer
expectInt64 = expectLiteral Literals.expectInt64

expectList :: Show m => (Term m -> Flow s a) -> Term m -> Flow s [a]
expectList f term = case stripTerm term of
  TermList l -> CM.mapM f l
  _ -> unexpected "list" term

expectLiteral :: Show m => (Literal -> Flow s a) -> Term m -> Flow s a
expectLiteral expect term = case stripTerm term of
  TermLiteral lit -> expect lit
  _ -> unexpected "literal" term

expectMap :: (Ord k, Show m) => (Term m -> Flow s k) -> (Term m -> Flow s v) -> Term m -> Flow s (M.Map k v)
expectMap fk fv term = case stripTerm term of
  TermMap m -> M.fromList <$> CM.mapM expectPair (M.toList m)
    where
      expectPair (kterm, vterm) = do
        kval <- fk kterm
        vval <- fv vterm
        return (kval, vval)
  _ -> unexpected "map" term

expectNArgs :: Int -> [Term m] -> Flow s ()
expectNArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") (L.length args)
  else pure ()

expectOptional :: Show m => (Term m -> Flow s a) -> Term m -> Flow s (Y.Maybe a)
expectOptional f term = case stripTerm term of
  TermOptional mt -> case mt of
    Nothing -> pure Nothing
    Just t -> Just <$> f t
  _ -> unexpected "optional value" term

expectRecord :: Show m => Term m -> Flow s [Field m]
expectRecord term = case stripTerm term of
  TermRecord (Record _ fields) -> pure fields
  _ -> unexpected "record" term

expectSet :: (Ord a, Show m) => (Term m -> Flow s a) -> Term m -> Flow s (S.Set a)
expectSet f term = case stripTerm term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> unexpected "set" term

expectString :: Show m => Term m -> Flow s String
expectString = expectLiteral Literals.expectString

expectUnion :: Show m => Term m -> Flow s (Field m)
expectUnion term = case stripTerm term of
  TermUnion (Injection _ field) -> pure field
  _ -> unexpected "union" term

field :: String -> Term m -> Field m
field n = Field (FieldName n)

fieldsToMap :: [Field m] -> M.Map FieldName (Term m)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

float32 :: Float -> Term m
float32 = literal . Literals.float32

float64 :: Double -> Term m
float64 = literal . Literals.float64

float :: FloatValue -> Term m
float = literal . Literals.float

fold :: Term m -> Term m
fold = TermFunction . FunctionElimination . EliminationList

inject :: Name -> Field m -> Term m
inject n = TermUnion . Injection n

int16 :: Int16 -> Term m
int16 = literal . Literals.int16

int32 :: Int -> Term m
int32 = literal . Literals.int32

int64 :: Int64 -> Term m
int64 = literal . Literals.int64

int8 :: Int8 -> Term m
int8 = literal . Literals.int8

integer :: IntegerValue -> Term m
integer = literal . Literals.integer

isUnit :: Eq m => Term m -> Bool
isUnit t = stripTerm t == TermRecord (Record unitTypeName [])

lambda :: String -> Term m -> Term m
lambda param body = TermFunction $ FunctionLambda $ Lambda (Name param) body

-- Construct a 'let' term with a single binding
letTerm :: Name -> Term m -> Term m -> Term m
letTerm v t1 t2 = TermLet $ Let (M.fromList [(v, t1)]) t2

list :: [Term m] -> Term m
list = TermList

literal :: Literal -> Term m
literal = TermLiteral

map :: M.Map (Term m) (Term m) -> Term m
map = TermMap

mapTerm :: M.Map (Term m) (Term m) -> Term m
mapTerm = TermMap

match :: Name -> [(FieldName, Term m)] -> Term m
match n = cases n . fmap toField
  where
    toField (name, term) = Field name term

matchOptional :: Term m -> Term m -> Term m
matchOptional n j = TermFunction $ FunctionElimination $ EliminationOptional $ OptionalCases n j

matchWithVariants :: Name -> [(FieldName, FieldName)] -> Term m
matchWithVariants n = cases n . fmap toField
  where
    toField (from, to) = Field from $ constFunction $ unitVariant n to

optional :: Y.Maybe (Term m) -> Term m
optional = TermOptional

primitive :: Name -> Term m
primitive = TermFunction . FunctionPrimitive

product :: [Term m] -> Term m
product = TermProduct

projection :: Name -> FieldName -> Term m
projection n fname = TermFunction $ FunctionElimination $ EliminationRecord $ Projection n fname

record :: Name -> [Field m] -> Term m
record n fields = TermRecord $ Record n fields

requireField :: M.Map FieldName (Term m) -> FieldName -> GraphFlow m (Term m)
requireField fields fname = Y.maybe err pure $ M.lookup fname fields
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

sum :: Int -> Int -> Term m -> Term m
sum i s term = TermSum $ Sum i s term

uint16 :: Integer -> Term m
uint16 = literal . Literals.uint16

uint32 :: Integer -> Term m
uint32 = literal . Literals.uint32

uint64 :: Integer -> Term m
uint64 = literal . Literals.uint64

uint8 :: Integer -> Term m
uint8 = literal . Literals.uint8

unit :: Term m
unit = TermRecord $ Record (Name "hydra/core.UnitType") []

unitVariant :: Name -> FieldName -> Term m
unitVariant n fname = variant n fname unit

unwrap :: Name -> Term m
unwrap = TermFunction . FunctionElimination . EliminationWrap

variable :: String -> Term m
variable = TermVariable . Name

variant :: Name -> FieldName -> Term m -> Term m
variant n fname term = TermUnion $ Injection n $ Field fname term

withVariant :: Name -> FieldName -> Term m
withVariant n = constFunction . unitVariant n

wrap :: Name -> Term m -> Term m
wrap name term = TermWrap $ Nominal name term
