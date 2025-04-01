-- | Base DSL which makes use of phantom types. Use this DSL for defining programs as opposed to data type definitions.

module Hydra.Dsl.Base (
  module Hydra.Dsl.Base,
  module Hydra.Dsl.PhantomLiterals,
  module Hydra.Dsl.ShorthandTypes,
  hydraCoreGraph,
) where

import Hydra.Coders
import Hydra.Core
import Hydra.Compute
import Hydra.Graph
import Hydra.Staging.Annotations
import Hydra.Phantoms
import Hydra.Module
import Hydra.Qnames
import Hydra.Dsl.PhantomLiterals
import Hydra.Dsl.ShorthandTypes
import Hydra.Sources.Tier0.Core
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Libraries

import Prelude hiding ((++))
import Data.String(IsString(..))

import qualified Data.Map as M
import qualified Data.Set as S


instance IsString (TTerm a) where fromString = TTerm . Terms.string

infixr 0 >:
(>:) :: String -> TTerm a -> Field
n >: d = Field (Name n) (unTTerm d)

infixr 0 >>:
(>>:) :: Name -> TTerm a -> Field
fname >>: d = Field fname (unTTerm d)

(<.>) :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
f <.> g = compose f g

-- Two alternative symbols for typed term application. TODO: remove one
(@@) :: TTerm (a -> b) -> TTerm a -> TTerm b
f @@ x = apply f x
($$) :: TTerm (a -> b) -> TTerm a -> TTerm b
f $$ x = apply f x

infixr 0 @->
(@->) :: a -> b -> (a, b)
x @-> y = (x, y)

infixr 0 -->
(-->) :: TCase a -> TTerm (a -> b) -> Field
c --> t = caseField c t

annot :: Name -> Maybe Term -> TTerm a -> TTerm a
annot key mvalue (TTerm term) = TTerm $ Ann.annotateTerm key mvalue term

apply :: TTerm (a -> b) -> TTerm a -> TTerm b
apply (TTerm lhs) (TTerm rhs) = TTerm $ Terms.apply lhs rhs

apply2 :: TTerm (a -> b -> c) -> TTerm a -> TTerm b -> TTerm c
apply2 (TTerm f) (TTerm a1) (TTerm a2) = TTerm $ Terms.apply (Terms.apply f a1) a2

caseField :: TCase a -> TTerm (a -> b) -> Field
caseField (TCase fname) (TTerm f) = Field fname f

--cases :: Name -> TTerm a -> Maybe (TTerm b) -> [Field] -> TTerm b
cases name arg dflt fields = (match name dflt fields) @@ arg

compose :: TTerm (b -> c) -> TTerm (a -> b) -> TTerm (a -> c)
compose (TTerm f) (TTerm g) = TTerm $ Terms.compose f g

constant :: TTerm a -> TTerm (b -> a)
constant (TTerm term) = TTerm $ Terms.constant term

definitionInModule :: Module -> String -> TTerm a -> TElement a
definitionInModule mod = definitionInNamespace $ moduleNamespace mod

definitionInNamespace :: Namespace -> String -> TTerm a -> TElement a
definitionInNamespace ns lname = TElement $ unqualifyName $ QualifiedName (Just ns) lname

doc :: String -> TTerm a -> TTerm a
doc s (TTerm term) = TTerm $ setTermDescription (Just s) term

doc70 :: String -> TTerm a -> TTerm a
doc70 = doc . wrapLine 70

doc80 :: String -> TTerm a -> TTerm a
doc80 = doc . wrapLine 80

el :: TElement a -> Element
el (TElement name (TTerm term)) = Element name term Nothing

field :: Name -> TTerm a -> Field
field fname (TTerm val) = Field fname val

first :: TTerm ((a, b) -> a)
first = TTerm $ Terms.untuple 2 0 Nothing

firstClassType :: TTerm Type -> TTerm Type
firstClassType typ = annot key_firstClassType (Just $ Terms.boolean True) typ

fld :: Name -> TTerm a -> TField a
fld fname (TTerm val) = TField $ Field fname val

fold :: TTerm (b -> a -> b) -> TTerm (b -> [a] -> b)
fold f = (primitive _lists_foldl) @@ f

identity :: TTerm (a -> a)
identity = TTerm Terms.identity

inject :: Name -> Name -> TTerm a -> TTerm b
inject name fname (TTerm term) = TTerm $ Terms.inject name (Field fname term)

inject2 :: Name -> Name -> TTerm (a -> b)
inject2 name fname = lambda "injected_" $ inject name fname $ var "injected_"

just :: TTerm a -> TTerm (Maybe a)
just (TTerm term) = TTerm $ Terms.just term

just_ :: TTerm (a -> Maybe a)
just_ = TTerm $ Terms.lambda "just_" $ Terms.just $ Terms.var "just_"

lambda :: String -> TTerm x -> TTerm (a -> b)
lambda v (TTerm body) = TTerm $ Terms.lambda v body

lambdas :: [String] -> TTerm x -> TTerm (a -> b)
lambdas params (TTerm body) = TTerm $ Terms.lambdas params body

--letTerm :: Var a -> TTerm a -> TTerm b -> TTerm b
--letTerm (Var k) (TTerm v) (TTerm env) = TTerm $ Terms.letTerm (Name k) v env

lets :: [Field] -> TTerm a -> TTerm a
lets fields (TTerm env) = TTerm $ TermLet $ Let (toBinding <$> fields) env
  where
     toBinding (Field name value) = LetBinding name value Nothing

list :: [TTerm a] -> TTerm [a]
list els = TTerm $ Terms.list (unTTerm <$> els)

map :: M.Map (TTerm a) (TTerm b) -> TTerm (M.Map a b)
map = TTerm . Terms.map . M.fromList . fmap fromTTerm . M.toList
  where
    fromTTerm (TTerm k, TTerm v) = (k, v)

match :: Name -> Maybe (TTerm b) -> [Field] -> TTerm (a -> b)
match name dflt fields = TTerm $ Terms.match name (unTTerm <$> dflt) fields

matchData :: Name -> Maybe (TTerm b) -> [(Name, TTerm (x -> b))] -> TTerm (a -> b)
matchData name dflt pairs = TTerm $ Terms.match name (unTTerm <$> dflt) (toField <$> pairs)
  where
    toField (fname, TTerm term) = Field fname term

matchToEnum :: Name -> Name -> Maybe (TTerm b) -> [(Name, Name)] -> TTerm (a -> b)
matchToEnum domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> Maybe (TTerm b) -> [(Name, Field)] -> TTerm (a -> b)
matchToUnion domName codName dflt pairs = matchData domName dflt (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ TTerm $ Terms.inject codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> TTerm a -> TTerm b
nom name (TTerm term) = TTerm $ Terms.wrap name term

nothing :: TTerm (Maybe a)
nothing = TTerm Terms.nothing

opt :: Maybe (TTerm a) -> TTerm (Maybe a)
opt mc = TTerm $ Terms.optional (unTTerm <$> mc)

pair :: (TTerm a) -> (TTerm b) -> TTerm (a, b)
pair (TTerm l) (TTerm r) = TTerm $ Terms.pair l r

primitive :: Name -> TTerm a
primitive = TTerm . Terms.primitive

primitive1 :: Name -> TTerm a -> TTerm b
primitive1 primName (TTerm a) = TTerm $ Terms.primitive primName Terms.@@ a

primitive2 :: Name -> TTerm a -> TTerm b -> TTerm c
primitive2 primName (TTerm a) (TTerm b) = TTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b

primitive3 :: Name -> TTerm a -> TTerm b -> TTerm c -> TTerm d
primitive3 primName (TTerm a) (TTerm b) (TTerm c) = TTerm $ Terms.primitive primName Terms.@@ a Terms.@@ b Terms.@@ c

project :: Name -> Name -> TTerm (a -> b)
project name fname = TTerm $ Terms.project name fname

record :: Name -> [Field] -> TTerm a
record name fields = TTerm $ Terms.record name fields

ref :: TElement a -> TTerm a
ref (TElement name _) = TTerm (TermVariable name)

second :: TTerm ((a, b) -> b)
second = TTerm $ Terms.untuple 2 1 Nothing

set :: [TTerm a] -> TTerm (S.Set a)
set = TTerm . Terms.set . S.fromList . fmap unTTerm

unit :: TTerm a
unit = TTerm Terms.unit

unitVariant :: Name -> Name -> TTerm a
unitVariant name fname = TTerm $ Terms.inject name $ Field fname Terms.unit

untuple :: Int -> Int -> TTerm (a -> b)
untuple arity idx = TTerm $ Terms.untuple arity idx Nothing

unwrap :: Name -> TTerm (a -> b)
unwrap = TTerm . Terms.unwrap

var :: String -> TTerm a
var v = TTerm $ Terms.var v

variant :: Name -> Name -> TTerm a -> TTerm b
variant name fname (TTerm term) = TTerm $ Terms.inject name $ Field fname term

withTypeClasses :: M.Map Name (S.Set TypeClass) -> TTerm a -> TTerm a
withTypeClasses classes (TTerm term) = TTerm $ setTypeClasses classes term

wrap :: Name -> TTerm a -> TTerm b
wrap name (TTerm term) = TTerm $ Terms.wrap name term
