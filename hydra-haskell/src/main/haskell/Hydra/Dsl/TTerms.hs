-- | A domain-specific language for constructing term-encoded Hydra terms in Haskell.

module Hydra.Dsl.TTerms (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTerms,
) where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Core as Core hiding (name)
import Hydra.Dsl.TBase
import qualified Hydra.Dsl.Base as Base

import qualified Data.Map as M
import qualified Data.Maybe as Y
import Data.Int


(@@) :: TTerm Term -> TTerm Term -> TTerm Term
f @@ x = apply f x

apply :: TTerm Term -> TTerm Term -> TTerm Term
apply func arg = termApplication $ application func arg

field :: String -> TTerm Term -> TTerm Field
field s = Core.field (name s)

float32 :: Float -> TTerm Term
float32 = float32Term . TTerm . Terms.float32

float32Term :: TTerm Float -> TTerm Term
float32Term = termLiteral . literalFloat . floatValueFloat32

fold :: TTerm Term -> TTerm Term
fold = termFunction . functionElimination . eliminationList

int16 :: Int16 -> TTerm Term
int16 = int16Term . TTerm . Terms.int16

int16Term :: TTerm Int16 -> TTerm Term
int16Term = termLiteral . literalInteger . integerValueInt16

int32 :: Int -> TTerm Term
int32 = int32Term . TTerm . Terms.int32

int32Term :: TTerm Int -> TTerm Term
int32Term = termLiteral . literalInteger . integerValueInt32

lambda :: String -> TTerm Term -> TTerm Term
lambda var body = termFunction $ functionLambda $ Core.lambda (name var) nothing body

list :: [TTerm Term] -> TTerm Term
list = termList . Base.list

prim :: Name -> TTerm Term
prim = termFunction . functionPrimitive . TTerm . coreEncodeName

project :: TTerm Name -> TTerm Name -> TTerm Term
project tname fname = termFunction $ functionElimination $ eliminationRecord $ projection tname fname

record :: TTerm Name -> [TTerm Field] -> TTerm Term
record name fields = termRecord $ Core.record name $ Base.list fields

string :: String -> TTerm Term
string = termLiteral . literalString . TTerm . Terms.string

-- | Maps a string to an encoded variable term.
var :: String -> TTerm Term
var = termVariable . name

varFromName :: Name -> TTerm Term
varFromName (Name n) = termVariable $ TTerm $ Terms.string n

-- | Maps a string to a variable encoded term.
variable :: String -> TTerm a
variable = TTerm . TermVariable . Name

variableFromName :: Name -> TTerm a
variableFromName = TTerm . TermVariable
