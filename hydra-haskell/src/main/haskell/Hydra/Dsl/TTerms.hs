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
import qualified Data.Maybe as Y
import Data.Int


infixr 0 >:
(>:) :: String -> TTerm Term -> TTerm Field
n >: d = field n d

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

fold :: TTerm Term -> TTerm Term
fold = Core.termFunction . Core.functionElimination . Core.eliminationList

int16 :: Int16 -> TTerm Term
int16 = int16Term . TTerm . Terms.int16

int16Term :: TTerm Int16 -> TTerm Term
int16Term = Core.termLiteral . Core.literalInteger . Core.integerValueInt16

int32 :: Int -> TTerm Term
int32 = int32Term . TTerm . Terms.int32

int32Term :: TTerm Int -> TTerm Term
int32Term = Core.termLiteral . Core.literalInteger . Core.integerValueInt32

lambda :: String -> TTerm Term -> TTerm Term
lambda var body = Core.termFunction $ Core.functionLambda $ Core.lambda (name var) nothing body

list :: [TTerm Term] -> TTerm Term
list = Core.termList . Base.list

match :: TTerm Name -> TTerm (Maybe Term) -> [TTerm Field] -> TTerm Term
match tname def fields = Core.termFunction $ Core.functionElimination $ Core.eliminationUnion
  $ Core.caseStatement tname def $ Base.list fields

prim :: Name -> TTerm Term
prim = Core.termFunction . Core.functionPrimitive . TTerm . coreEncodeName

project :: TTerm Name -> TTerm Name -> TTerm Term
project tname fname = Core.termFunction $ Core.functionElimination $ Core.eliminationRecord
  $ Core.projection tname fname

record :: TTerm Name -> [TTerm Field] -> TTerm Term
record name fields = Core.termRecord $ Core.record name $ Base.list fields

string :: String -> TTerm Term
string = Core.termLiteral . Core.literalString . TTerm . Terms.string

true :: TTerm Term
true = boolean True

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
