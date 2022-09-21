module Hydra.Impl.Haskell.Dsl.Base (
  module Hydra.Impl.Haskell.Dsl.Base,
  module Hydra.Impl.Haskell.Dsl.PhantomLiterals,
  module Hydra.Phantoms,
  Standard.coreContext,
) where

import Hydra.Common
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.PhantomLiterals
import Hydra.Meta
import Hydra.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import Hydra.Graph
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Types.Inference
import qualified Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings
import Hydra.Monads

import Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S


el :: Definition a -> GraphFlow Meta (Element Meta)
el (Definition name (Datum term0)) = do
    pushTrc $ "construct element " <> unName name
    term1 <- annotateTermWithTypes term0
    t <- findType term1
    let schemaTerm = encodeType t
    return $ Element name schemaTerm term1
  where
    findType term = do
      cx <- getState
      mt <- annotationClassTermType (contextAnnotations cx) term
      case mt of
        Just t -> return t
        Nothing -> fail "expected a type annotation"

--el :: Definition a -> GraphFlow Meta (Element Meta)
--el (Definition name (Datum term)) = do
--    pushTrc $ "infer type of " <> unName name
--    t <- findType
--    let schemaTerm = encodeType t
--    return $ Element name schemaTerm term
--  where
--    findType = do
--      cx <- getState
--      mt <- annotationClassTermType (contextAnnotations cx) term
--      case mt of
--        Just t -> return t
--        Nothing -> typeSchemeType . snd <$> inferType term

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
(-->) :: Case a -> Datum (a -> b) -> Field Meta
c --> t = caseField c t

(++) :: Datum String -> Datum String -> Datum String
l ++ r = Strings.cat @@ list [l, r]

apply :: Datum (a -> b) -> Datum a -> Datum b
apply (Datum lhs) (Datum rhs) = Datum $ Terms.apply lhs rhs

apply2 :: Datum (a -> b -> c) -> Datum a -> Datum b -> Datum c
apply2 (Datum f) (Datum a1) (Datum a2) = Datum $ Terms.apply (Terms.apply f a1) a2

caseField :: Case a -> Datum (a -> b) -> Field Meta -- Datum (u -> b)
caseField (Case fname) (Datum f) = Field fname $ Terms.lambda "x" $ Terms.apply f (Terms.variable "x")

compareTo :: Datum a -> Datum (a -> Bool)
compareTo (Datum term) = Datum $ Terms.compareTo term

compose :: Datum (b -> c) -> Datum (a -> b) -> Datum (a -> c)
compose (Datum f) (Datum g) = Datum $ Terms.lambda "x" $ Terms.apply f (Terms.apply g $ Terms.variable "x")

constant :: Datum a -> Datum (b -> a)
constant (Datum term) = Datum $ Terms.lambda "_" term

denom :: Name -> Datum (a -> b)
denom = Datum . Terms.eliminateNominal

delta :: Datum (Reference a -> a)
delta = Datum Terms.delta

doc :: String -> Datum a -> Datum a
doc s (Datum term) = Datum $ setTermDescription Standard.coreContext (Just s) term

element :: Definition a -> Datum (Reference a)
element (Definition name _) = Datum $ Terms.element name

field :: FieldName -> Datum a -> Field Meta
field fname (Datum val) = Field fname val

function :: Type Meta -> Type Meta -> Datum a -> Datum a
function dom cod = typed (Types.function dom cod)

lambda :: String -> Datum x -> Datum (a -> b)
lambda v (Datum body) = Datum $ Terms.lambda v body

--letTerm :: Var a -> Datum a -> Datum b -> Datum b
--letTerm (Var k) (Datum v) (Datum env) = Datum $ Terms.letTerm (Variable k) v env

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

match :: Name -> Type Meta -> [Field Meta] -> Datum (u -> b)
match name cod fields = function (Types.nominal name) cod $ Datum $ Terms.cases name fields

matchToEnum :: Name -> Name -> [(FieldName, FieldName)] -> Datum (a -> b)
matchToEnum domName codName pairs = matchData domName (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> [(FieldName, Field Meta)] -> Datum (a -> b)
matchToUnion domName codName pairs = matchData domName (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName, constant $ Datum $ Terms.union codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Datum a -> Datum b
nom name (Datum term) = Datum $ Terms.nominal name term

opt :: Maybe (Datum a) -> Datum (Maybe a)
opt mc = Datum $ Terms.optional (unDatum <$> mc)

primitive :: Name -> Datum a
primitive = Datum . Terms.primitive

project :: Name -> Type Meta -> FieldName -> Datum (a -> b)
project name cod fname = Datum $ Terms.projection name fname

record :: Name -> [Field Meta] -> Datum a
record name fields = Datum $ Terms.record name fields

ref :: Definition a -> Datum a
ref e = delta @@ element e

set :: S.Set (Datum a) -> Datum (S.Set a)
set = Datum . Terms.set . S.fromList . fmap unDatum . S.toList

typed :: Type Meta -> Datum a -> Datum a
typed t (Datum term) = Datum $ setTermType Standard.coreContext (Just t) term

union :: Name -> FieldName -> Datum a -> Datum b
union name fname (Datum term) = Datum $ Terms.union name (Field fname term)

union2 :: Name -> FieldName -> Datum (a -> b)
union2 name fname = lambda "x" $ typed (Types.nominal name) $ union name fname $ var "x"

unit :: Datum a
unit = Datum Terms.unit

unitVariant :: Name -> FieldName -> Datum a
unitVariant name fname = typed (Types.nominal name) $ Datum $ Terms.union name $ Field fname Terms.unit

var :: String -> Datum a
var v = Datum $ Terms.variable v

variant :: Name -> FieldName -> Datum a -> Datum b
variant name fname (Datum term) = typed (Types.nominal name) $ Datum $ Terms.union name $ Field fname term
