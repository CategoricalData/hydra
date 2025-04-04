-- | A domain-specific language for constructing Hydra terms in Haskell.

{-# LANGUAGE FlexibleInstances #-} -- TODO: temporary, for IsString Term
module Hydra.Dsl.Terms where

import Hydra.Compute
import Hydra.Constants
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Literals as Literals

import Prelude hiding (map, product, sum)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Control.Monad as CM
import Data.Int
import Data.String(IsString(..))


instance IsString Term where fromString = string

-- Two alternative symbols for term application
(@@) :: Term -> Term -> Term
f @@ x = apply f x
($$) :: Term -> Term -> Term
f $$ x = apply f x

--(<$>) :: Term -> Term -> Term
--f <$> x = apply f x

(<.>) :: Term -> Term -> Term
f <.> g = compose f g

infixr 0 >:
(>:) :: String -> Term -> Field
n >: t = field n t

annot :: M.Map Name Term -> Term -> Term
annot ann t = TermAnnotated $ AnnotatedTerm t ann

apply :: Term -> Term -> Term
apply func arg = TermApplication $ Application func arg

bigfloat :: Double -> Term
bigfloat = literal . Literals.bigfloat

bigint :: Integer -> Term
bigint = literal . Literals.bigint

binary :: String -> Term
binary = literal . Literals.binary

boolean :: Bool -> Term
boolean = literal . Literals.boolean

compose :: Term -> Term -> Term
compose f g = lambda "arg_" $ apply f (apply g $ var "arg_")

constant :: Term -> Term
constant = lambda ignoredVariable

elimination :: Elimination -> Term
elimination = TermFunction . FunctionElimination

false :: Term
false = boolean False

field :: String -> Term -> Field
field n = Field (Name n)

fieldsToMap :: [Field] -> M.Map Name Term
fieldsToMap fields = M.fromList $ (\(Field name term) -> (name, term)) <$> fields

first :: Term
first = untuple 2 0 Nothing

float32 :: Float -> Term
float32 = literal . Literals.float32

float64 :: Double -> Term
float64 = literal . Literals.float64

float :: FloatValue -> Term
float = literal . Literals.float

identity :: Term
identity = lambda "any_" $ var "any_"

inject :: Name -> Field -> Term
inject tname = TermUnion . Injection tname

int16 :: Int16 -> Term
int16 = literal . Literals.int16

int32 :: Int -> Term
int32 = literal . Literals.int32

int64 :: Int64 -> Term
int64 = literal . Literals.int64

int8 :: Int8 -> Term
int8 = literal . Literals.int8

integer :: IntegerValue -> Term
integer = literal . Literals.integer

just :: Term -> Term
just = optional . Just

lambda :: String -> Term -> Term
lambda param body = TermFunction $ FunctionLambda $ Lambda (Name param) Nothing body

lambdas :: [String] -> Term -> Term
lambdas params body = case params of
  [] -> body
  (h:r) -> lambda h $ lambdas r body

lambdaTyped :: String -> Type -> Term -> Term
lambdaTyped param dom body = TermFunction $ FunctionLambda $ Lambda (Name param) (Just dom) body

let1 :: String -> Term -> Term -> Term
let1 v t1 t2 = TermLet $ Let [LetBinding (Name v) t1 Nothing] t2

lets :: [Field] -> Term -> Term
lets bindings env = TermLet $ Let (toBinding <$> bindings) env
  where
    toBinding (Field name value) = LetBinding name value Nothing

list :: [Term] -> Term
list = TermList

literal :: Literal -> Term
literal = TermLiteral

map :: M.Map Term Term -> Term
map = TermMap

mapTerm :: M.Map Term Term -> Term
mapTerm = TermMap

match :: Name -> Maybe Term -> [Field] -> Term
match tname def fields = TermFunction $ FunctionElimination $ EliminationUnion $ CaseStatement tname def fields

matchWithVariants :: Name -> Maybe Term -> [(Name, Name)] -> Term
matchWithVariants tname def pairs = match tname def (toField <$> pairs)
  where
    toField (from, to) = Field from $ constant $ unitVariant tname to

nothing :: Term
nothing = optional Nothing

optional :: Y.Maybe Term -> Term
optional = TermOptional

pair :: Term -> Term -> Term
pair a b = TermProduct [a, b]

primitive :: Name -> Term
primitive = TermFunction . FunctionPrimitive

product :: [Term] -> Term
product = TermProduct

project :: Name -> Name -> Term
project tname fname = TermFunction $ FunctionElimination $ EliminationRecord $ Projection tname fname

record :: Name -> [Field] -> Term
record tname fields = TermRecord $ Record tname fields

second :: Term
second = untuple 2 1 Nothing

set :: S.Set Term -> Term
set = TermSet

string :: String -> Term
string = TermLiteral . LiteralString

sum :: Int -> Int -> Term -> Term
sum i s term = TermSum $ Sum i s term

true :: Term
true = boolean True

typeAbstraction :: [Name] -> Term -> Term
typeAbstraction vars body = L.foldl (\b v -> TermTypeAbstraction $ TypeAbstraction v b) body vars

typeApplication :: Term -> [Type] -> Term
typeApplication term types = L.foldl (\t ty -> TermTypeApplication $ TypedTerm t ty) term types

typed :: Type -> Term -> Term
typed typ term = TermTyped $ TypedTerm term typ

uint16 :: Int -> Term
uint16 = literal . Literals.uint16

uint32 :: Int64 -> Term
uint32 = literal . Literals.uint32

uint64 :: Integer -> Term
uint64 = literal . Literals.uint64

uint8 :: Int16 -> Term
uint8 = literal . Literals.uint8

unit :: Term
unit = TermRecord $ Record _Unit []

unitVariant :: Name -> Name -> Term
unitVariant tname fname = variant tname fname unit

untuple :: Int -> Int -> Maybe [Type] -> Term
untuple arity idx mtypes = TermFunction $ FunctionElimination $ EliminationProduct $ TupleProjection arity idx mtypes

unwrap :: Name -> Term
unwrap = TermFunction . FunctionElimination . EliminationWrap

var :: String -> Term
var = TermVariable . Name

variant :: Name -> Name -> Term -> Term
variant tname fname term = TermUnion $ Injection tname $ Field fname term

withVariant :: Name -> Name -> Term
withVariant tname = constant . unitVariant tname

wrap :: Name -> Term -> Term
wrap name term = TermWrap $ WrappedTerm name term
