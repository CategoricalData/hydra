"""Base DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions."""

import hydra.dsl.terms as terms
from hydra.core import Field, Name, TermVariable
from hydra.graph import Element
from hydra.phantoms import TCase, TElement, TField, TTerm

# module Hydra.Dsl.Base (
#   module Hydra.Dsl.Base,
#   module Hydra.Dsl.PhantomLiterals,
#   module Hydra.Dsl.ShorthandTypes,
#   hydraCoreGraph,
# ) where

# import Hydra.Coders
# import Hydra.Core
# import Hydra.Compute
# import Hydra.Graph
# import Hydra.Staging.Annotations
# import Hydra.Phantoms
# import Hydra.Module
# import Hydra.Qnames
# import Hydra.Dsl.PhantomLiterals
# import Hydra.Dsl.ShorthandTypes
# import Hydra.Sources.Tier0.Core
# import qualified Hydra.Dsl.Annotations as Ann
# import qualified Hydra.Dsl.Terms as Terms
# import qualified Hydra.Dsl.Types as Types
# import Hydra.Sources.Libraries

# import Prelude hiding ((++))
# import Data.String(IsString(..))

# import qualified Data.Map as M
# import qualified Data.Set as S


# instance IsString (TTerm a) where fromString = TTerm . Terms.string

# infixr 0 >:
# (>:) :: String -> TTerm a -> Field
# n >: d = Field (Name n) (unTTerm d)

# infixr 0 >>:
# (>>:) :: Name -> TTerm a -> Field
# fname >>: d = Field fname (unTTerm d)

# (<.>) :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
# f <.> g = compose f g

# -- Two alternative symbols for typed term application. TODO: remove one
# (@@) :: TTerm (a -> b) -> TTerm a -> TTerm b
# f @@ x = apply f x
# ($$) :: TTerm (a -> b) -> TTerm a -> TTerm b
# f $$ x = apply f x

# infixr 0 @->
# (@->) :: a -> b -> (a, b)
# x @-> y = (x, y)

# infixr 0 -->
# (-->) :: TCase a -> TTerm (a -> b) -> Field
# c --> t = caseField c t


# annot :: Name -> Maybe Term -> TTerm a -> TTerm a
# annot key mvalue (TTerm term) = TTerm $ Ann.annotateTerm key mvalue term


def apply[T](func: TTerm[T], a: TTerm[T]) -> TTerm[T]:
    """Apply a function to an argument."""
    return TTerm[T](terms.apply(func.value, a.value))


def apply2[T](func: TTerm[T], a: TTerm[T], b: TTerm[T]) -> TTerm[T]:
    """Apply a function to two arguments."""
    return TTerm[T](terms.apply(terms.apply(func.value, a.value), b.value))


def case_field[T](case: TCase[T], func: TTerm[T]) -> Field:
    """Apply a function to a case."""
    return Field(case.value, func.value)


def cases[T](
    name: Name, arg: TTerm[T], dflt: TTerm[T], fields: list[Field]
) -> TTerm[T]:
    """Apply a function to a case."""
    return apply(TTerm[T](terms.match(name, dflt.value, fields)), arg)


def compose[T](lhs: TTerm[T], rhs: TTerm[T]) -> TTerm[T]:
    """Compose two functions."""
    return TTerm[T](terms.compose(lhs.value, rhs.value))


def constant[T](term: TTerm[T]) -> TTerm[T]:
    """Constant function."""
    return TTerm[T](terms.constant(term.value))


# definitionInModule :: Module -> String -> TTerm a -> TElement a
# definitionInModule mod = definitionInNamespace $ moduleNamespace mod

# definitionInNamespace :: Namespace -> String -> TTerm a -> TElement a
# definitionInNamespace ns lname = TElement $ unqualifyName $ QualifiedName (Just ns) lname

# doc :: String -> TTerm a -> TTerm a
# doc s (TTerm term) = TTerm $ setTermDescription (Just s) term

# doc70 :: String -> TTerm a -> TTerm a
# doc70 = doc . wrapLine 70

# doc80 :: String -> TTerm a -> TTerm a
# doc80 = doc . wrapLine 80


def el[T](element: TElement[T]) -> Element:
    """Element."""
    return Element(element.name, element.term.value, None)


def field[T](fname: Name, val: TTerm[T]) -> Field:
    """Field."""
    return Field(fname, val.value)


def first[T]():
    """Construct a first."""
    return TTerm[T](terms.untuple(2, 0, None))


# firstClassType :: TTerm Type -> TTerm Type
# firstClassType typ = annot key_firstClassType (Just $ Terms.boolean True) typ


def fld[T](fname: Name, val: TTerm[T]) -> TField[T]:
    """Construct a field."""
    return TField[T](Field(fname, val.value))


# fold :: TTerm (b -> a -> b) -> TTerm (b -> [a] -> b)
# fold f = (primitive _lists_foldl) @@ f


def identity[T]():
    """Construct an identity."""
    return TTerm[T](terms.identity())


def inject[T](name: Name, fname: Name, term: TTerm[T]) -> TTerm[T]:
    """Construct an inject."""
    return TTerm[T](terms.inject(name, Field(fname, term.value)))


def inject2[T](name: Name, fname: Name):
    """Construct an inject2."""
    return lam(
        "injected_",
        inject(
            name,
            fname,
            TTerm[T](terms.var("injected_")),
        ),
    )


def just[T](term: TTerm[T]):
    """Construct a just."""
    return TTerm[T](terms.just(term.value))


def just_[T]():
    """Construct a just_."""
    return TTerm[T](terms.lam("just_", terms.just(terms.var("just_"))))


def lam[T](v: str, body: TTerm[T]) -> TTerm[T]:
    """Construct a lambda."""
    return TTerm[T](terms.lam(v, body.value))


# lambdas :: [String] -> TTerm x -> TTerm (a -> b)
# lambdas params (TTerm body) = TTerm $ Terms.lambdas params body


# --letTerm :: Var a -> TTerm a -> TTerm b -> TTerm b
# --letTerm (Var k) (TTerm v) (TTerm env) = TTerm $ Terms.letTerm (Name k) v env

# lets :: [Field] -> TTerm a -> TTerm a
# lets fields (TTerm env) = TTerm $ TermLet $ Let (toBinding <$> fields) env
#   where
#      toBinding (Field name value) = Binding name value Nothing

# list :: [TTerm a] -> TTerm [a]
# list els = TTerm $ Terms.list (unTTerm <$> els)

# map :: M.Map (TTerm a) (TTerm b) -> TTerm (M.Map a b)
# map = TTerm . Terms.map . M.fromList . fmap fromTTerm . M.toList
#   where
#     fromTTerm (TTerm k, TTerm v) = (k, v)

# match :: Name -> Maybe (TTerm b) -> [Field] -> TTerm (a -> b)
# match name dflt fields = TTerm $ Terms.match name (unTTerm <$> dflt) fields

# matchData :: Name -> Maybe (TTerm b) -> [(Name, TTerm (x -> b))] -> TTerm (a -> b)
# matchData name dflt pairs = TTerm $ Terms.match name (unTTerm <$> dflt) (toField <$> pairs)
#   where
#     toField (fname, TTerm term) = Field fname term

# matchToEnum :: Name -> Name -> Maybe (TTerm b) -> [(Name, Name)] -> TTerm (a -> b)
# matchToEnum domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
#   where
#     toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

# matchToUnion :: Name -> Name -> Maybe (TTerm b) -> [(Name, Field)] -> TTerm (a -> b)
# matchToUnion domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
#   where
#     toCase (fromName, fld) = (fromName, constant $ TTerm $ Terms.inject codName fld)


def nom[T](name: Name, term: TTerm[T]):
    """Construct a nom."""
    return TTerm[T](terms.wrap(name, term.value))


def nothing[T]():
    """Construct a nothing."""
    return TTerm[T](terms.nothing())


def opt[T](mc: TTerm[T]):
    """Construct an optional."""
    return TTerm[T](terms.optional(mc.value))


def pair[T](a: TTerm[T], b: TTerm[T]):
    """Construct a pair."""
    return TTerm[T](terms.pair(a.value, b.value))


def primitive[T](name: Name):
    """Construct a primitive."""
    return TTerm[T](terms.primitive(name))


def primitive1[A, B](
    prim_name: Name, term: TTerm[A], meaningless: B | None = None
) -> TTerm[B]:
    """Construct a primitive1."""
    return TTerm[B](terms.apply(terms.primitive(prim_name), term.value))


def primitive2[T](prim_name: Name, a: TTerm[T], b: TTerm[T]) -> TTerm[T]:
    """Construct a primitive2."""
    return TTerm[T](
        terms.apply(terms.apply(terms.primitive(prim_name), a.value), b.value)
    )


def primitive3[T](prim_name: Name, a: TTerm[T], b: TTerm[T], c: TTerm[T]) -> TTerm[T]:
    """Construct a primitive3."""
    return TTerm[T](
        terms.apply(
            terms.apply(terms.apply(terms.primitive(prim_name), a.value), b.value),
            c.value,
        )
    )


def project[T](name: Name, fname: Name):
    """Construct a project."""
    return TTerm[T](terms.project(name, fname))


def record[T](name: Name, fields: list[Field]):
    """Construct a record."""
    return TTerm[T](terms.record(name, fields))


def ref[T](name: TElement[T]):
    """Construct a reference."""
    return TTerm[T](TermVariable(name.name))


def second[T]():
    """Construct a second."""
    return TTerm[T](terms.untuple(2, 1, None))


# set :: [TTerm a] -> TTerm (S.Set a)
# set = TTerm . Terms.set . S.fromList . fmap unTTerm


def unit[T]():
    """Construct a unit."""
    return TTerm[T](terms.unit())


def unit_variant[T](name: Name, fname: Name):
    """Construct a unit variant."""
    return TTerm[T](terms.inject(name, Field(fname, terms.unit())))


def untuple[T](arity: int, idx: int):
    """Construct a tuple."""
    return TTerm[T](terms.untuple(arity, idx, None))


def unwrap[T](name: Name):
    """Unwrap a term."""
    return TTerm[T](terms.unwrap(name))


def var[T](v: str):
    """Construct a variable."""
    return TTerm[T](terms.var(v))


def variant[T](name: Name, fname: Name, term: TTerm[T]) -> TTerm[T]:
    """Construct a variant."""
    return TTerm[T](terms.inject(name, Field(fname, term.value)))


# withTypeClasses :: M.Map Name (S.Set TypeClass) -> TTerm a -> TTerm a
# withTypeClasses classes (TTerm term) = TTerm $ setTypeClasses classes term


def wrap[T](name: Name, term: TTerm[T]) -> TTerm[T]:
    """Wrap a term."""
    return TTerm[T](terms.wrap(name, term.value))
