-- | A domain-specific language for constructing term-encoded Hydra terms in Haskell;
--   these functions enable you to build terms (programs) which build terms.

module Hydra.Dsl.TTerms (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTerms,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.TBase
import qualified Hydra.Dsl.Base as Base

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import Data.Int
import Prelude hiding (map, product, sum)


(@@) :: TTerm Term -> TTerm Term -> TTerm Term
f @@ x = apply f x

apply :: TTerm Term -> TTerm Term -> TTerm Term
apply func arg = Core.termApplication $ Core.application func arg

boolean :: Bool -> TTerm Term
boolean = Core.termLiteral . Core.literalBoolean . TTerm . Terms.boolean

constant :: TTerm Term -> TTerm Term
constant = lambda ignoredVariable

false :: TTerm Term
false = boolean False

field :: String -> TTerm Term -> TTerm Field
field s = Core.field (name s)

float32 :: Float -> TTerm Term
float32 = float32Term . TTerm . Terms.float32

float32Term :: TTerm Float -> TTerm Term
float32Term = Core.termLiteral . Core.literalFloat . Core.floatValueFloat32

float64 :: Double -> TTerm Term
float64 = float64Term . TTerm . Terms.float64

float64Term :: TTerm Float -> TTerm Term
float64Term = Core.termLiteral . Core.literalFloat . Core.floatValueFloat64

inject :: TTerm Name -> String -> TTerm Term -> TTerm Term
inject tname fname = Core.termUnion . Core.injection tname . Core.field (name fname)

int16 :: Int16 -> TTerm Term
int16 = int16Term . TTerm . Terms.int16

int16Term :: TTerm Int16 -> TTerm Term
int16Term = Core.termLiteral . Core.literalInteger . Core.integerValueInt16

int32 :: Int -> TTerm Term
int32 = int32Term . TTerm . Terms.int32

int32Term :: TTerm Int -> TTerm Term
int32Term = Core.termLiteral . Core.literalInteger . Core.integerValueInt32

just :: TTerm Term -> TTerm (Maybe Term)
just = Base.just

lambda :: String -> TTerm Term -> TTerm Term
lambda var body = Core.termFunction $ Core.functionLambda $ Core.lambda (name var) Base.nothing body

lambdas :: [String] -> TTerm Term -> TTerm Term
lambdas params body = case params of
  [] -> body
  (h:rest) -> Core.termFunction $ Core.functionLambda $ Core.lambda (name h) Base.nothing $ lambdas rest body

let1 :: String -> TTerm Term -> TTerm Term -> TTerm Term
let1 v t1 t2 = Core.termLet $ Core.letExpression (Base.list [Core.letBinding (name v) t1 Base.nothing]) t2

lets :: [(TTerm Name, TTerm Term)] -> TTerm Term -> TTerm Term
lets pairs body = Core.termLet $ Core.letExpression (Base.list $ toBinding pairs) body
  where
    toBinding = fmap (\(n, t) -> Core.letBinding n t Base.nothing)

list :: [TTerm Term] -> TTerm Term
list = Core.termList . Base.list

mapTerm :: TTerm (M.Map Term Term) -> TTerm Term
mapTerm = Core.termMap

match :: TTerm Name -> TTerm (Maybe Term) -> [(TTerm Name, TTerm Term)] -> TTerm Term
match tname def pairs = Core.termFunction $ Core.functionElimination $ Core.eliminationUnion
    $ Core.caseStatement tname def $ Base.list $ toField pairs
  where
    toField = fmap (\(n, t) -> Core.field n t)

nothing :: TTerm (Maybe Term)
nothing = Base.nothing

optional :: TTerm (Maybe Term) -> TTerm Term
optional = Core.termOptional

pair :: TTerm Term -> TTerm Term -> TTerm Term
pair t1 t2 = product [t1, t2]

primitive :: Name -> TTerm Term
primitive = Core.termFunction . Core.functionPrimitive . TTerm . coreEncodeName

product :: [TTerm Term] -> TTerm Term
product terms = Core.termProduct $ TTerm $ TermList (unTTerm <$> terms)

project :: TTerm Name -> TTerm Name -> TTerm Term
project tname fname = Core.termFunction $ Core.functionElimination $ Core.eliminationRecord
  $ Core.projection tname fname

record :: TTerm Name -> [(TTerm Name, TTerm Term)] -> TTerm Term
record name pairs = Core.termRecord $ Core.record name $ Base.list (toField <$> pairs)
  where
    toField (n, t) = Core.field n t

set :: [TTerm Term] -> TTerm Term
set els = Core.termSet $ TTerm $ TermSet $ S.fromList (unTTerm <$> els)

string :: String -> TTerm Term
string = Core.termLiteral . Core.literalString . TTerm . Terms.string

sum :: Int -> Int -> TTerm Term -> TTerm Term
sum i s = Core.termSum . Core.sum (Base.int32 i) (Base.int32 s)

true :: TTerm Term
true = boolean True

tuple :: [TTerm Term] -> TTerm Term
tuple = Core.termProduct . Base.list

uint64 :: Integer -> TTerm Term
uint64 = uint64Term . TTerm . Terms.uint64

uint64Term :: TTerm Integer -> TTerm Term
uint64Term = Core.termLiteral . Core.literalInteger . Core.integerValueUint64

untuple :: Int -> Int -> TTerm Term
untuple arity idx = Core.termFunction $ Core.functionElimination $ Core.eliminationProduct
  $ Core.tupleProjection (Base.int32 arity) (Base.int32 idx) Base.nothing

unwrap :: TTerm Name -> TTerm Term
unwrap = Core.termFunction . Core.functionElimination . Core.eliminationWrap

-- | Maps a string to an encoded variable term.
var :: String -> TTerm Term
var = Core.termVariable . name

varFromName :: Name -> TTerm Term
varFromName (Name n) = Core.termVariable $ TTerm $ Terms.string n

-- | Maps a string to a variable encoded term.
variable :: String -> TTerm a
variable = TTerm . TermVariable . Name

variableFromName :: Name -> TTerm a
variableFromName = TTerm . TermVariable

wrap :: TTerm Name -> TTerm Term -> TTerm Term
wrap name = Core.termWrap . Core.wrappedTerm name
