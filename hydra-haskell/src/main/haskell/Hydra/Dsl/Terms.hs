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


instance IsString (Term a) where fromString = string

(@@) :: Term a -> Term a -> Term a
f @@ x = apply f x

annot :: a -> Term a -> Term a
annot ann t = TermAnnotated $ Annotated t ann

apply :: Term a -> Term a -> Term a
apply func arg = TermApplication $ Application func arg

bigfloat :: Double -> Term a
bigfloat = literal . Literals.bigfloat

bigint :: Integer -> Term a
bigint = literal . Literals.bigint

binary :: String -> Term a
binary = literal . Literals.binary

boolean :: Bool -> Term a
boolean = literal . Literals.boolean

cases :: Name -> Maybe (Term a) -> [Field a] -> Term a
cases n def fields = TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement n def fields

constFunction :: Term a -> Term a
constFunction = lambda "_"

delta :: Term a
delta = TermFunction $ FunctionElimination EliminationElement

elementRef :: Element a -> Term a
elementRef = apply delta . TermElement . elementName

elementRefByName :: Name -> Term a
elementRefByName = apply delta . TermElement

elimination :: Elimination a -> Term a
elimination = TermFunction . FunctionElimination

expectBinary :: Show a => Term a -> Flow s String
expectBinary t = expectLiteral t >>= Literals.expectBinary

expectBoolean :: Show a => Term a -> Flow s Bool
expectBoolean t = expectLiteral t >>= Literals.expectBoolean

expectFloat32 :: Show a => Term a -> Flow s Float
expectFloat32 t = expectLiteral t >>= Literals.expectFloat32

expectFloat64 :: Show a => Term a -> Flow s Double
expectFloat64 t = expectLiteral t >>= Literals.expectFloat64

expectInt32 :: Show a => Term a -> Flow s Int
expectInt32 t = expectLiteral t >>= Literals.expectInt32

expectInt64 :: Show a => Term a -> Flow s Integer
expectInt64 t = expectLiteral t >>= Literals.expectInt64

expectList :: Show a => (Term a -> Flow s x) -> Term a -> Flow s [x]
expectList f term = case stripTerm term of
  TermList l -> CM.mapM f l
  _ -> unexpected "list" term

expectLiteral :: Show a => Term a -> Flow s Literal
expectLiteral term = case stripTerm term of
  TermLiteral lit -> pure lit
  _ -> unexpected "literal" term

expectMap :: (Ord k, Show a) => (Term a -> Flow s k) -> (Term a -> Flow s v) -> Term a -> Flow s (M.Map k v)
expectMap fk fv term = case stripTerm term of
  TermMap m -> M.fromList <$> CM.mapM expectPair (M.toList m)
    where
      expectPair (kterm, vterm) = do
        kval <- fk kterm
        vval <- fv vterm
        return (kval, vval)
  _ -> unexpected "map" term

expectNArgs :: Int -> [Term a] -> Flow s ()
expectNArgs n args = if L.length args /= n
  then unexpected (show n ++ " arguments") (L.length args)
  else pure ()

expectOptional :: Show a => (Term a -> Flow s x) -> Term a -> Flow s (Y.Maybe x)
expectOptional f term = case stripTerm term of
  TermOptional mt -> case mt of
    Nothing -> pure Nothing
    Just t -> Just <$> f t
  _ -> unexpected "optional value" term

expectRecord :: Show a => Term a -> Flow s [Field a]
expectRecord term = case stripTerm term of
  TermRecord (Record _ fields) -> pure fields
  _ -> unexpected "record" term

expectSet :: (Ord x, Show a) => (Term a -> Flow s x) -> Term a -> Flow s (S.Set x)
expectSet f term = case stripTerm term of
  TermSet s -> S.fromList <$> CM.mapM f (S.toList s)
  _ -> unexpected "set" term

expectString :: Show a => Term a -> Flow s String
expectString t = expectLiteral t >>= Literals.expectString

expectUnion :: Show a => Term a -> Flow s (Field a)
expectUnion term = case stripTerm term of
  TermUnion (Injection _ field) -> pure field
  _ -> unexpected "union" term

field :: String -> Term a -> Field a
field n = Field (FieldName n)

fieldsToMap :: [Field a] -> M.Map FieldName (Term a)
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

float32 :: Float -> Term a
float32 = literal . Literals.float32

float64 :: Double -> Term a
float64 = literal . Literals.float64

float :: FloatValue -> Term a
float = literal . Literals.float

fold :: Term a -> Term a
fold = TermFunction . FunctionElimination . EliminationList

inject :: Name -> Field a -> Term a
inject n = TermUnion . Injection n

int16 :: Int16 -> Term a
int16 = literal . Literals.int16

int32 :: Int -> Term a
int32 = literal . Literals.int32

int64 :: Int64 -> Term a
int64 = literal . Literals.int64

int8 :: Int8 -> Term a
int8 = literal . Literals.int8

integer :: IntegerValue -> Term a
integer = literal . Literals.integer

isUnit :: Eq a => Term a -> Bool
isUnit t = stripTerm t == TermRecord (Record unitTypeName [])

lambda :: String -> Term a -> Term a
lambda param body = TermFunction $ FunctionLambda $ Lambda (Name param) body

-- Construct a 'let' term with a single binding
letTerm :: Name -> Term a -> Term a -> Term a
letTerm v t1 t2 = TermLet $ Let (M.fromList [(v, t1)]) t2

list :: [Term a] -> Term a
list = TermList

literal :: Literal -> Term a
literal = TermLiteral

map :: M.Map (Term a) (Term a) -> Term a
map = TermMap

mapTerm :: M.Map (Term a) (Term a) -> Term a
mapTerm = TermMap

match :: Name -> Maybe (Term a) -> [(FieldName, Term a)] -> Term a
match n def pairs = cases n def (toField <$> pairs)
  where
    toField (name, term) = Field name term

matchOptional :: Term a -> Term a -> Term a
matchOptional n j = TermFunction $ FunctionElimination $ EliminationOptional $ OptionalCases n j

matchWithVariants :: Name -> Maybe (Term a) -> [(FieldName, FieldName)] -> Term a
matchWithVariants n def pairs = cases n def (toField <$> pairs)
  where
    toField (from, to) = Field from $ constFunction $ unitVariant n to

optional :: Y.Maybe (Term a) -> Term a
optional = TermOptional

primitive :: Name -> Term a
primitive = TermFunction . FunctionPrimitive

product :: [Term a] -> Term a
product = TermProduct

projection :: Name -> FieldName -> Term a
projection n fname = TermFunction $ FunctionElimination $ EliminationRecord $ Projection n fname

record :: Name -> [Field a] -> Term a
record n fields = TermRecord $ Record n fields

requireField :: M.Map FieldName (Term a) -> FieldName -> GraphFlow a (Term a)
requireField fields fname = Y.maybe err pure $ M.lookup fname fields
  where
    err = fail $ "no such field: " ++ unFieldName fname

set :: S.Set (Term a) -> Term a
set = TermSet

string :: String -> Term a
string = TermLiteral . LiteralString

sum :: Int -> Int -> Term a -> Term a
sum i s term = TermSum $ Sum i s term

uint16 :: Integer -> Term a
uint16 = literal . Literals.uint16

uint32 :: Integer -> Term a
uint32 = literal . Literals.uint32

uint64 :: Integer -> Term a
uint64 = literal . Literals.uint64

uint8 :: Integer -> Term a
uint8 = literal . Literals.uint8

unit :: Term a
unit = TermRecord $ Record (Name "hydra/core.UnitType") []

unitVariant :: Name -> FieldName -> Term a
unitVariant n fname = variant n fname unit

unwrap :: Name -> Term a
unwrap = TermFunction . FunctionElimination . EliminationWrap

variable :: String -> Term a
variable = TermVariable . Name

variant :: Name -> FieldName -> Term a -> Term a
variant n fname term = TermUnion $ Injection n $ Field fname term

withVariant :: Name -> FieldName -> Term a
withVariant n = constFunction . unitVariant n

wrap :: Name -> Term a -> Term a
wrap name term = TermWrap $ Nominal name term
