-- | Base DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.

module Hydra.Dsl.Base (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.PhantomLiterals,
  module Hydra.Dsl.ShorthandTypes,
  hydraCore,
) where

import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Annotations
import Hydra.Phantoms
import Hydra.Module
import qualified Hydra.Tier1 as Tier1
import Hydra.Dsl.PhantomLiterals
import Hydra.Dsl.ShorthandTypes
import Hydra.Sources.Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Lib.Lists as Lists
import Hydra.Sources.Libraries

import Prelude hiding ((++))
import Data.String(IsString(..))

import qualified Data.Map as M
import qualified Data.Set as S


instance IsString (Datum a) where fromString = Datum . Terms.string

infixr 0 >:
(>:) :: String -> Datum a -> Field
n >: d = Field (Name n) (unDatum d)

infixr 0 >>:
(>>:) :: Name -> Datum a -> Field
fname >>: d = Field fname (unDatum d)

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
(-->) :: Case a -> Datum (a -> b) -> Field
c --> t = caseField c t

apply :: Datum (a -> b) -> Datum a -> Datum b
apply (Datum lhs) (Datum rhs) = Datum $ Terms.apply lhs rhs

apply2 :: Datum (a -> b -> c) -> Datum a -> Datum b -> Datum c
apply2 (Datum f) (Datum a1) (Datum a2) = Datum $ Terms.apply (Terms.apply f a1) a2

caseField :: Case a -> Datum (a -> b) -> Field
caseField (Case fname) (Datum f) = Field fname f

compose :: Datum (b -> c) -> Datum (a -> b) -> Datum (a -> c)
compose (Datum f) (Datum g) = Datum $ Terms.compose f g

constant :: Datum a -> Datum (b -> a)
constant (Datum term) = Datum $ Terms.constant term

definitionInModule :: Module -> String -> Datum a -> Definition a
definitionInModule mod lname = Definition $ Tier1.unqualifyName $ QualifiedName (Just $ moduleNamespace mod) lname

doc :: String -> Datum a -> Datum a
doc s (Datum term) = Datum $ setTermDescription (Just s) term

doc70 :: String -> Datum a -> Datum a
doc70 = doc . wrapLine 70

doc80 :: String -> Datum a -> Datum a
doc80 = doc . wrapLine 80

el :: Definition a -> Element
el (Definition name (Datum term)) = Element name term

field :: Name -> Datum a -> Field
field fname (Datum val) = Field fname val

first :: Datum ((a, b) -> a)
first = Datum $ Terms.untuple 2 0

fld :: Name -> Datum a -> Fld a
fld fname (Datum val) = Fld $ Field fname val

fold :: Datum (b -> a -> b) -> Datum (b -> [a] -> b)
fold f = Lists.foldl @@ f

function :: Type -> Type -> Datum a -> Datum a
function dom cod = typed (Types.function dom cod)

functionN :: [Type] -> Datum a -> Datum a
functionN ts = typed $ Types.functionN ts

functionNWithClasses :: [Type] -> M.Map Name (S.Set TypeClass) -> Datum a -> Datum a
functionNWithClasses ts classes = typed $ setTypeClasses classes (Types.functionN ts)

functionWithClasses :: Type -> Type -> M.Map Name (S.Set TypeClass) -> Datum a -> Datum a
functionWithClasses dom cod classes = typed $ setTypeClasses classes (Types.function dom cod)

-- Note: Haskell has trouble type-checking this construction if the convenience functions from Base are used
ifElse :: Datum Bool -> Datum a -> Datum a -> Datum a
ifElse (Datum cond) (Datum ifTrue) (Datum ifFalse) = Datum $
  Terms.apply (Terms.apply (Terms.apply (Terms.primitive _logic_ifElse) ifTrue) ifFalse) cond

ifOpt :: Datum (Maybe a) -> Datum b -> Datum (a -> b) -> Datum b
ifOpt m n j = matchOpt n j @@ m

identity :: Datum (a -> a)
identity = Datum Terms.identity

inject :: Name -> Name -> Datum a -> Datum b
inject name fname (Datum term) = Datum $ Terms.inject name (Field fname term)

inject2 :: Name -> Name -> Datum (a -> b)
inject2 name fname = lambda "x2" $ inject name fname $ var "x2"

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

match :: Name -> Maybe (Datum b) -> [Field] -> Datum (u -> b)
match name dflt fields = Datum $ Terms.match name (unDatum <$> dflt) fields

matchData :: Name -> Maybe (Datum b) -> [(Name, Datum (x -> b))] -> Datum (a -> b)
matchData name dflt pairs = Datum $ Terms.match name (unDatum <$> dflt) (toField <$> pairs)
  where
    toField (fname, Datum term) = Field fname term

matchOpt :: Datum b -> Datum (a -> b) -> Datum (Maybe a -> b)
matchOpt (Datum n) (Datum j) = Datum $ Terms.matchOpt n j

matchToEnum :: Name -> Name -> Maybe (Datum b) -> [(Name, Name)] -> Datum (a -> b)
matchToEnum domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> Maybe (Datum b) -> [(Name, Field)] -> Datum (a -> b)
matchToUnion domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ Datum $ Terms.inject codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Datum a -> Datum b
nom name (Datum term) = Datum $ Terms.wrap name term

nothing :: Datum x
nothing = Datum Terms.nothing

opt :: Maybe (Datum a) -> Datum (Maybe a)
opt mc = Datum $ Terms.optional (unDatum <$> mc)

pair :: (Datum a, Datum b) -> Datum (a, b)
pair (Datum l, Datum r) = Datum $ Terms.pair l r

primitive :: Name -> Datum a
primitive = Datum . Terms.primitive

project :: Name -> Name -> Datum (a -> b)
project name fname = Datum $ Terms.project name fname

record :: Name -> [Field] -> Datum a
record name fields = Datum $ Terms.record name fields

ref :: Definition a -> Datum a
ref (Definition name _) = Datum (TermVariable name)

second :: Datum ((a, b) -> b)
second = Datum $ Terms.untuple 2 1

set :: S.Set (Datum a) -> Datum (S.Set a)
set = Datum . Terms.set . S.fromList . fmap unDatum . S.toList

typed :: Type -> Datum a -> Datum a
typed typ (Datum term) = Datum $ setTermType (Just typ) term

unit :: Datum a
unit = Datum Terms.unit

unitVariant :: Name -> Name -> Datum a
unitVariant name fname = Datum $ Terms.inject name $ Field fname Terms.unit

unwrap :: Name -> Datum (a -> b)
unwrap = Datum . Terms.unwrap

var :: String -> Datum a
var v = Datum $ Terms.var v

variant :: Name -> Name -> Datum a -> Datum b
variant name fname (Datum term) = Datum $ Terms.inject name $ Field fname term

with :: Datum a -> [Field] -> Datum a
(Datum env) `with` fields = Datum $ TermLet $ Let (toBinding <$> fields) env
  where
     toBinding (Field name value) = LetBinding name value Nothing

wrap :: Name -> Datum a -> Datum b
wrap name (Datum term) = Datum $ Terms.wrap name term
