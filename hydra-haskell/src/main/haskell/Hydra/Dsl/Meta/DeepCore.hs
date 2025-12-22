-- | A domain-specific language for constructing Hydra terms in Haskell.
--
-- This DSL provides helpers for constructing "deep" terms - i.e., terms that
-- represent Hydra terms as data (Term values), as opposed to the shallow DSL
-- that works at the Haskell level.
module Hydra.Dsl.Meta.DeepCore where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as P

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


--------------------------------------------------------------------------------
-- Application operators
--------------------------------------------------------------------------------

-- | Apply a function term to an argument term (infix operator, alias)
(@@@) :: TTerm Term -> TTerm Term -> TTerm Term
fun @@@ arg = apply fun arg

-- | Apply a function term to an argument term
apply :: TTerm Term -> TTerm Term -> TTerm Term
apply lhs rhs = Core.termApplication $ Core.application lhs rhs

--------------------------------------------------------------------------------
-- Case statements and pattern matching
--------------------------------------------------------------------------------

cases :: Name -> TTerm Term -> TTerm (Maybe Term) -> [TTerm Field] -> TTerm Term
cases tname arg dflt fields = match tname dflt fields @@@ arg

-- | Create a union case statement with optional default case
match :: Name -> TTerm (Maybe Term) -> [TTerm Field] -> TTerm Term
match tname dflt fields = Core.termFunction $ Core.functionElimination $ Core.eliminationUnion $
  Core.caseStatement
    (Core.nameLift tname)
    dflt
    (P.list fields)

-- | Create a field for case matching
field :: Name -> TTerm Term -> TTerm Field
field fname body = Core.field (Core.nameLift fname) body

--------------------------------------------------------------------------------
-- Functions and lambdas
--------------------------------------------------------------------------------

-- | Create a lambda term with a variable name and body
lambda :: String -> TTerm Term -> TTerm Term
lambda v body = Core.termFunction $ Core.functionLambda $ Core.lambda (Core.name (P.string v)) P.nothing $ body

constant :: TTerm Term -> TTerm Term
constant = lambda ignoredVariable

-- | Create a reference to a primitive function
primitive :: Name -> TTerm Term
primitive name = Core.termFunction $ Core.functionPrimitive $ Core.nameLift name

-- TODO: this is probably equivalent to `primitive`.
-- | Create a reference to a primitive function using encodedName
-- (for primitives that need namespace encoding)
primitiveEncoded :: Name -> TTerm Term
primitiveEncoded name = Core.termFunction $ Core.functionPrimitive $ P.encodedName name

--------------------------------------------------------------------------------
-- Projections and eliminations
--------------------------------------------------------------------------------

-- | Create a record field projection
project :: Name -> Name -> TTerm Term
project tname fname = Core.termFunction $ Core.functionElimination $ Core.eliminationRecord $
  Core.projection (Core.nameLift tname) (Core.nameLift fname)

unwrap :: Name -> TTerm Term
unwrap name = unwrapDynamic (Core.nameLift name)

-- | Create an unwrap elimination for a wrapped type
unwrapDynamic :: TTerm Name -> TTerm Term
unwrapDynamic tname = Core.termFunction $ Core.functionElimination $ Core.eliminationWrap tname

--------------------------------------------------------------------------------
-- Literals and basic terms
--------------------------------------------------------------------------------

-- | Create a string literal term
string :: TTerm String -> TTerm Term
string s = Core.termLiteral $ Core.literalString s

-- | Create a variable reference term
var :: String -> TTerm Term
var s = Core.termVariable $ Core.name $ P.string s

-- | Create a unit term
unit :: TTerm Term
unit = Core.termUnit

--------------------------------------------------------------------------------
-- Compound terms (records, unions, wraps, lists)
--------------------------------------------------------------------------------

-- | Create a union injection term
injection :: Name -> TTerm Field -> TTerm Term
injection tname fld = Core.termUnion $ Core.injection (Core.nameLift tname) fld

-- | Create a record term with a type name and fields
record :: Name -> [TTerm Field] -> TTerm Term
record tname fields = Core.termRecord $ Core.record (Core.nameLift tname) (P.list fields)

-- | Create a list term from a list of terms
list :: TTerm [Term] -> TTerm Term
list terms = Core.termList terms

map :: TTerm (M.Map Term Term) -> TTerm Term
map terms = Core.termMap terms

-- | Create an Either term (left or right)
either :: TTerm (Either Term Term) -> TTerm Term
either e = Core.termEither e

left :: TTerm Term -> TTerm Term
left (TTerm t) = Core.termEither $ TTerm $ TermEither $ Left t

right :: TTerm Term -> TTerm Term
right (TTerm t) = Core.termEither $ TTerm $ TermEither $ Right t

-- | Create a let expression with bindings and a body
-- Each binding is a (name, term) pair
lets :: [(String, TTerm Term)] -> TTerm Term -> TTerm Term
lets bindings body = Core.termLet $ Core.let_
  (P.list [Core.binding (Core.name (P.string n)) t P.nothing | (n, t) <- bindings])
  body

-- | Create a single let binding
letn :: String -> TTerm Term -> TTerm Term -> TTerm Term
letn name value body = lets [(name, value)] body

pair :: TTerm Term -> TTerm Term -> TTerm Term
pair (TTerm first) (TTerm second) = Core.termPair $ TTerm $ TermPair (first, second)

-- | Create a wrap term with a type name and body
wrap :: Name -> TTerm Term -> TTerm Term
wrap tname body = Core.termWrap $ Core.wrappedTerm (Core.nameLift tname) body

-- | Create a wrap term with a dynamic type name (TTerm Name)
wrapDynamic :: TTerm Name -> TTerm Term -> TTerm Term
wrapDynamic tname body = Core.termWrap $ Core.wrappedTerm tname body
