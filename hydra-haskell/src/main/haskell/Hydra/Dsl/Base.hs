-- | Base DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.

module Hydra.Dsl.Base (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.PhantomLiterals,
  hydraCore,
) where

import Hydra.Kernel
import Hydra.Dsl.PhantomLiterals
import Hydra.Sources.Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Lib.Strings as Strings

import Prelude hiding ((++))
-- import Data.String(IsString(..))

import qualified Data.Map as M
import qualified Data.Set as S


-- instance IsString (Datum String) where fromString = Datum . Terms.string

el :: Definition a -> Element Kv
el (Definition name (Datum term)) = Element name (epsilonEncodeType dummyType) term
  where
    dummyType = TypeRecord (RowType (Name "PreInferencePlaceholder") Nothing [])

infixr 0 >:
(>:) :: String -> Datum a -> Fld a
n >: d = Fld $ Field (FieldName n) (unDatum d)

(<.>) :: Datum (b -> c) -> Datum (a -> b) -> Datum (a -> c)
f <.> g = compose f g

($$) :: Datum (a -> b) -> Datum a -> Datum b
f $$ x = apply f x

(@@) :: Datum (a -> b) -> Datum a -> Datum b
f @@ x = apply f x

infixr 0 @->
(@->) :: a -> b -> (a, b)
x @-> y = (x, y)

infixr 0 -->
(-->) :: Case a -> Datum (a -> b) -> Field Kv
c --> t = caseField c t

(++) :: Datum String -> Datum String -> Datum String
l ++ r = Strings.cat @@ list [l, r]

apply :: Datum (a -> b) -> Datum a -> Datum b
apply (Datum lhs) (Datum rhs) = Datum $ Terms.apply lhs rhs

apply2 :: Datum (a -> b -> c) -> Datum a -> Datum b -> Datum c
apply2 (Datum f) (Datum a1) (Datum a2) = Datum $ Terms.apply (Terms.apply f a1) a2

caseField :: Case a -> Datum (a -> b) -> Field Kv
caseField (Case fname) (Datum f) = Field fname f

compose :: Datum (b -> c) -> Datum (a -> b) -> Datum (a -> c)
compose (Datum f) (Datum g) = Datum $ Terms.compose f g

constant :: Datum a -> Datum (b -> a)
constant (Datum term) = Datum $ Terms.constant term

delta :: Datum (Reference a -> a)
delta = Datum Terms.delta

doc :: String -> Datum a -> Datum a
doc s (Datum term) = Datum $ setTermDescription hydraCore (Just s) term

element :: Definition a -> Datum (Reference a)
element (Definition name _) = Datum $ TermElement name

field :: FieldName -> Datum a -> Field Kv
field fname (Datum val) = Field fname val

fld :: FieldName -> Datum a -> Fld Kv
fld fname (Datum val) = Fld $ Field fname val

function :: Type Kv -> Type Kv -> Datum a -> Datum a
function dom cod = typed (Types.function dom cod)

functionN :: [Type Kv] -> Type Kv -> Datum a -> Datum a
functionN doms cod = typed $ Types.functionN doms cod

-- id :: Datum (a -> a)
-- id = Datum Terms.id

inject :: Name -> FieldName -> Datum a -> Datum b
inject name fname (Datum term) = Datum $ Terms.inject name (Field fname term)

inject2 :: Name -> FieldName -> Datum (a -> b)
inject2 name fname = lambda "x2" $ typed (Types.wrap name) $ inject name fname $ var "x2"

just :: Datum x -> Datum (Maybe x)
just (Datum term) = Datum $ Terms.just term

lambda :: String -> Datum x -> Datum (a -> b)
lambda v (Datum body) = Datum $ Terms.lambda v body

--letTerm :: Var a -> Datum a -> Datum b -> Datum b
--letTerm (Var k) (Datum v) (Datum env) = Datum $ Terms.letTerm (Name k) v env

list :: [Datum a] -> Datum [a]
list els = Datum $ Terms.list (unDatum <$> els)

map :: M.Map (Datum a) (Datum b) -> Datum (M.Map a b)
map = Datum . Terms.map . M.fromList . fmap fromDatum . M.toList
  where
    fromDatum (Datum k, Datum v) = (k, v)

match :: Name -> Maybe (Term Kv) -> [Field Kv] -> Datum (u -> b)
match name dflt fields = Datum $ Terms.cases name dflt fields

matchData :: Name -> Maybe (Datum b) -> [(FieldName, Datum (x -> b))] -> Datum (a -> b)
matchData name dflt pairs = Datum $ Terms.cases name (unDatum <$> dflt) (toField <$> pairs)
  where
    toField (fname, Datum term) = Field fname term

matchOpt :: Datum b -> Datum (a -> b) -> Datum (Maybe a -> b)
matchOpt (Datum n) (Datum j) = Datum $ Terms.matchOpt n j

matchToEnum :: Name -> Name -> Maybe (Datum b) -> [(FieldName, FieldName)] -> Datum (a -> b)
matchToEnum domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> Maybe (Datum b) -> [(FieldName, Field Kv)] -> Datum (a -> b)
matchToUnion domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ Datum $ Terms.inject codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Datum a -> Datum b
nom name (Datum term) = Datum $ Terms.wrap name term

nothing :: Datum x
nothing = Datum $ Terms.nothing

opt :: Maybe (Datum a) -> Datum (Maybe a)
opt mc = Datum $ Terms.optional (unDatum <$> mc)

primitive :: Name -> Datum a
primitive = Datum . Terms.primitive

project :: Name -> FieldName -> Datum (a -> b)
project name fname = Datum $ Terms.project name fname

record :: Name -> [Fld a] -> Datum a
record name fields = Datum $ Terms.record name (unFld <$> fields)

ref :: Definition a -> Datum a
ref (Definition name _) = Datum (Terms.apply Terms.delta $ TermElement name)

set :: S.Set (Datum a) -> Datum (S.Set a)
set = Datum . Terms.set . S.fromList . fmap unDatum . S.toList

typed :: Type Kv -> Datum a -> Datum a
typed t (Datum term) = Datum $ setTermType hydraCore (Just t) term

unit :: Datum a
unit = Datum Terms.unit

unitVariant :: Name -> FieldName -> Datum a
unitVariant name fname = typed (Types.wrap name) $ Datum $ Terms.inject name $ Field fname Terms.unit

unwrap :: Name -> Datum (a -> b)
unwrap = Datum . Terms.unwrap

var :: String -> Datum a
var v = Datum $ Terms.var v

variant :: Name -> FieldName -> Datum a -> Datum b
variant name fname (Datum term) = typed (Types.wrap name) $ Datum $ Terms.inject name $ Field fname term

with :: Datum a -> [Fld a] -> Datum a
(Datum env) `with` bindings = Datum $ TermLet $ Let (M.fromList $ toPair <$> bindings) env
  where
     toPair (Fld (Field name value)) = (Name $ unFieldName name, value)

wrap :: Name -> Datum a -> Datum b
wrap name (Datum term) = Datum $ Terms.wrap name term
