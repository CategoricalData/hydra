-- | A domain-specific language for constructing term-encoded Hydra terms in Haskell.

module Hydra.Dsl.TTerms (
  module Hydra.Dsl.TBase,
  module Hydra.Dsl.TTerms,
) where

import Hydra.Kernel
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Core as Core
import Hydra.Dsl.TBase

import qualified Data.Map as M
import qualified Data.Maybe as Y


field :: String -> TTerm Term -> TTerm Field
field s = Core.field (Core.name $ Name s)

float32 :: Float -> TTerm Term
float32 = float32Term . TTerm . Terms.float32

float32Term :: TTerm Float -> TTerm Term
float32Term = termLiteral . literalFloat . floatValueFloat32

int32 :: Int -> TTerm Term
int32 = int32Term . TTerm . Terms.int32

int32Term :: TTerm Int -> TTerm Term
int32Term = termLiteral . literalInteger . integerValueInt32

project :: TTerm Name -> TTerm Name -> TTerm Term
project tname fname = termFunction $ functionElimination $ eliminationRecord $ projection tname fname

record :: TTerm Name -> [TTerm Field] -> TTerm Term
record name fields = termRecord $ Core.record name $ list fields

string :: String -> TTerm Term
string = termLiteral . literalString . TTerm . Terms.string

var :: String -> TTerm a
var = TTerm . TermVariable . Name

wrappedTerm :: TTerm Name -> TTerm Term -> TTerm WrappedTerm
wrappedTerm typeName object = Base.record _WrappedTerm [
  _WrappedTerm_typeName>>: typeName,
  _WrappedTerm_object>>: object]

wrappedTermTypeName :: TTerm (WrappedTerm -> Name)
wrappedTermTypeName = Base.project _WrappedTerm _WrappedTerm_typeName

wrappedTermObject :: TTerm (WrappedTerm -> Term)
wrappedTermObject = Base.project _WrappedTerm _WrappedTerm_object

wrappedType :: TTerm Name -> TTerm Type -> TTerm WrappedType
wrappedType typeName object = Base.record _WrappedType [
  _WrappedType_typeName>>: typeName,
  _WrappedType_object>>: object]

wrappedTypeTypeName :: TTerm (WrappedType -> Name)
wrappedTypeTypeName = Base.project _WrappedType _WrappedType_typeName

wrappedTypeObject :: TTerm (WrappedType -> Type)
wrappedTypeObject = Base.project _WrappedType _WrappedType_object
