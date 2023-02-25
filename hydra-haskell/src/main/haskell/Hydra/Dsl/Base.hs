-- | Base DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.

module Hydra.Dsl.Base (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.PhantomLiterals,
  Standard.coreContext,
) where

import Hydra.Kernel
import Hydra.Kv
import Hydra.CoreEncoding
import Hydra.Dsl.PhantomLiterals
import qualified Hydra.Dsl.Standard as Standard
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Core
import Hydra.Types.Inference
import qualified Hydra.Dsl.Lib.Strings as Strings

import Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S


el :: Definition a -> Element Kv
el (Definition name (Datum term)) = Element name (encodeType dummyType) term
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
compose (Datum f) (Datum g) = Datum $ Terms.lambda "x1" $ Terms.apply f (Terms.apply g $ Terms.variable "x1")

constant :: Datum a -> Datum (b -> a)
constant (Datum term) = Datum $ Terms.lambda "_" term

denom :: Name -> Datum (a -> b)
denom = Datum . Terms.unwrap

delta :: Datum (Reference a -> a)
delta = Datum Terms.delta

doc :: String -> Datum a -> Datum a
doc s (Datum term) = Datum $ setTermDescription Standard.coreContext (Just s) term

element :: Definition a -> Datum (Reference a)
element (Definition name _) = Datum $ Terms.element name

field :: FieldName -> Datum a -> Field Kv
field fname (Datum val) = Field fname val

function :: Type Kv -> Type Kv -> Datum a -> Datum a
function dom cod = typed (Types.function dom cod)

functionN :: [Type Kv] -> Type Kv -> Datum a -> Datum a
functionN doms cod = typed $ Types.functionN doms cod

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

matchData :: Name -> [(FieldName, Datum (x -> b))] -> Datum (a -> b)
matchData name pairs = Datum $ Terms.cases name (toField <$> pairs)
  where
    toField (fname, Datum term) = Field fname term

matchOpt :: Datum b -> Datum (a -> b) -> Datum (Maybe a -> b)
matchOpt (Datum n) (Datum j) = Datum $ Terms.matchOptional n j

match :: Name -> Type Kv -> [Field Kv] -> Datum (u -> b)
match name cod fields = function (Types.wrap name) cod $ Datum $ Terms.cases name fields

matchToEnum :: Name -> Name -> [(FieldName, FieldName)] -> Datum (a -> b)
matchToEnum domName codName pairs = matchData domName (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> [(FieldName, Field Kv)] -> Datum (a -> b)
matchToUnion domName codName pairs = matchData domName (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ Datum $ Terms.inject codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Datum a -> Datum b
nom name (Datum term) = Datum $ Terms.wrap name term

opt :: Maybe (Datum a) -> Datum (Maybe a)
opt mc = Datum $ Terms.optional (unDatum <$> mc)

primitive :: Name -> Datum a
primitive = Datum . Terms.primitive

project :: Name -> Type Kv -> FieldName -> Datum (a -> b)
project name cod fname = Datum $ Terms.projection name fname

record :: Name -> [Fld a] -> Datum a
record name fields = Datum $ Terms.record name (unFld <$> fields)

ref :: Definition a -> Datum a
ref (Definition name _) = Datum (Terms.apply Terms.delta $ Terms.element name) 

set :: S.Set (Datum a) -> Datum (S.Set a)
set = Datum . Terms.set . S.fromList . fmap unDatum . S.toList

typed :: Type Kv -> Datum a -> Datum a
typed t (Datum term) = Datum $ setTermType Standard.coreContext (Just t) term

union :: Name -> FieldName -> Datum a -> Datum b
union name fname (Datum term) = Datum $ Terms.inject name (Field fname term)

union2 :: Name -> FieldName -> Datum (a -> b)
union2 name fname = lambda "x2" $ typed (Types.wrap name) $ union name fname $ var "x2"

unit :: Datum a
unit = Datum Terms.unit

unitVariant :: Name -> FieldName -> Datum a
unitVariant name fname = typed (Types.wrap name) $ Datum $ Terms.inject name $ Field fname Terms.unit

var :: String -> Datum a
var v = Datum $ Terms.variable v

variant :: Name -> FieldName -> Datum a -> Datum b
variant name fname (Datum term) = typed (Types.wrap name) $ Datum $ Terms.inject name $ Field fname term
