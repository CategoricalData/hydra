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
(@@@) :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
fun @@@ arg = apply fun arg

-- | Apply a function term to an argument term
apply :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
apply lhs rhs = Core.termApplication $ Core.application lhs rhs

--------------------------------------------------------------------------------
-- Case statements and pattern matching
--------------------------------------------------------------------------------

cases :: Name -> TypedTerm Term -> TypedTerm (Maybe Term) -> [TypedTerm CaseAlternative] -> TypedTerm Term
cases tname arg dflt alts = match tname dflt alts @@@ arg

-- | Create a field (e.g. for injection payloads)
field :: Name -> TypedTerm Term -> TypedTerm Field
field fname body = Core.field (Core.nameLift fname) body

-- | Create a case alternative (a variant tag together with its handler) for case matching
caseAlternative :: Name -> TypedTerm Term -> TypedTerm CaseAlternative
caseAlternative aname handler = Core.caseAlternative (Core.nameLift aname) handler

-- | Create a union case statement with optional default case
match :: Name -> TypedTerm (Maybe Term) -> [TypedTerm CaseAlternative] -> TypedTerm Term
match tname dflt alts = Core.termCases $
  Core.caseStatement
    (Core.nameLift tname)
    dflt
    (P.list alts)

--------------------------------------------------------------------------------
-- Functions and lambdas
--------------------------------------------------------------------------------

constant :: TypedTerm Term -> TypedTerm Term
constant = lambda ignoredVariable

-- | Create a lambda term with a variable name and body
lambda :: String -> TypedTerm Term -> TypedTerm Term
lambda v body = Core.termLambda $ Core.lambda (Core.name (P.string v)) P.nothing $ body

-- | Create a lambda term with a dynamic parameter name, optional domain, and body
lambdaTyped :: TypedTerm Name -> TypedTerm (Maybe Type) -> TypedTerm Term -> TypedTerm Term
lambdaTyped name domain body = Core.termLambda $ Core.lambda name domain body

-- | Create a reference to a primitive function.
-- Uses termVariable; the name resolves via graphPrimitives fallthrough.
primitive :: Name -> TypedTerm Term
primitive name = Core.termVariable $ Core.nameLift name

-- TODO: this is probably equivalent to `primitive`.
-- | Create a reference to a primitive function using encodedName
-- (for primitives that need namespace encoding)
primitiveEncoded :: Name -> TypedTerm Term
primitiveEncoded name = Core.termVariable $ P.encodedName name

--------------------------------------------------------------------------------
-- Projections and eliminations
--------------------------------------------------------------------------------

-- | Create a record field projection
project :: Name -> Name -> TypedTerm Term
project tname fname = Core.termProject $
  Core.projection (Core.nameLift tname) (Core.nameLift fname)

unwrap :: Name -> TypedTerm Term
unwrap name = unwrapDynamic (Core.nameLift name)

-- | Create an unwrap elimination for a wrapped type
unwrapDynamic :: TypedTerm Name -> TypedTerm Term
unwrapDynamic tname = Core.termUnwrap tname

--------------------------------------------------------------------------------
-- Literals and basic terms
--------------------------------------------------------------------------------

-- | Create a string literal term
string :: TypedTerm String -> TypedTerm Term
string s = Core.termLiteral $ Core.literalString s

-- | Create a unit term
unit :: TypedTerm Term
unit = Core.termUnit

-- | Create a variable reference term
var :: String -> TypedTerm Term
var s = Core.termVariable $ Core.name $ P.string s

--------------------------------------------------------------------------------
-- Compound terms (records, unions, wraps, lists)
--------------------------------------------------------------------------------

-- | Create an Either term (left or right)
either :: TypedTerm (Either Term Term) -> TypedTerm Term
either e = Core.termEither e

-- | Create a union injection term
injection :: Name -> TypedTerm Field -> TypedTerm Term
injection tname fld = Core.termInject $ Core.injection (Core.nameLift tname) fld

-- | Create a Just term
just :: TypedTerm Term -> TypedTerm Term
just (TypedTerm t) = TypedTerm $ TermMaybe $ Just t

left :: TypedTerm Term -> TypedTerm Term
left (TypedTerm t) = Core.termEither $ TypedTerm $ TermEither $ Left t

-- | Create a single let binding
letn :: String -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
letn name value body = lets [(name, value)] body

-- | Create a let expression with bindings and a body
-- Each binding is a (name, term) pair
lets :: [(String, TypedTerm Term)] -> TypedTerm Term -> TypedTerm Term
lets bindings body = Core.termLet $ Core.let_
  (P.list [Core.binding (Core.name (P.string n)) t P.nothing | (n, t) <- bindings])
  body

-- | Create a list term from a list of terms
list :: TypedTerm [Term] -> TypedTerm Term
list terms = Core.termList terms

map :: TypedTerm (M.Map Term Term) -> TypedTerm Term
map terms = Core.termMap terms

-- | Create a Nothing term
nothing :: TypedTerm Term
nothing = TypedTerm $ TermMaybe Nothing

pair :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
pair (TypedTerm first) (TypedTerm second) = Core.termPair $ TypedTerm $ TermPair (first, second)

-- | Create a record term with a type name and fields
record :: Name -> [TypedTerm Field] -> TypedTerm Term
record tname fields = Core.termRecord $ Core.record (Core.nameLift tname) (P.list fields)

ref :: TypedTermDefinition a -> TypedTerm Term
ref (TypedTermDefinition name _) = Core.termVariable $ Core.nameLift name

right :: TypedTerm Term -> TypedTerm Term
right (TypedTerm t) = Core.termEither $ TypedTerm $ TermEither $ Right t

-- | Create a wrap term with a type name and body
wrap :: Name -> TypedTerm Term -> TypedTerm Term
wrap tname body = Core.termWrap $ Core.wrappedTerm (Core.nameLift tname) body

-- | Create a wrap term with a dynamic type name (TypedTerm Name)
wrapDynamic :: TypedTerm Name -> TypedTerm Term -> TypedTerm Term
wrapDynamic tname body = Core.termWrap $ Core.wrappedTerm tname body
