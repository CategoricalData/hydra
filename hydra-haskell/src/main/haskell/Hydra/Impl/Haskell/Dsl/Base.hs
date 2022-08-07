module Hydra.Impl.Haskell.Dsl.Base (
  module Hydra.Impl.Haskell.Dsl.Base,
  module Hydra.Impl.Haskell.Dsl.Literals,
  module Hydra.Impl.Haskell.Dsl.Phantoms,
  Standard.standardContext,
) where

import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Literals
import Hydra.Meta
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Graph as Graph
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Core
import qualified Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings

import Prelude hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


el :: Element a -> Context Meta -> Result (Graph.Element Meta)
el (Element name (Data term)) cx = do
    dt <- declaredType
    t <- Y.maybe (fail $ "untyped term in " <> unName name <> ": " <> show term) return dt
    let t = Y.fromMaybe Types.unit dt
    let schemaTerm = encodeType cx t
    return $ Graph.Element name schemaTerm term
  where
    cx = Standard.standardContext
    declaredType = annotationClassTermType (contextAnnotations cx) cx term
--    findType = do
--      mt <- declaredType
--      Y.maybe (typeSchemeType . snd <$> inferType cx term) pure mt

graph :: Graph.GraphName -> [Context Meta -> Result (Graph.Element Meta)] -> Result (Graph.Graph Meta)
graph gname cons = do
    elements <- mapM (\f -> f cx) cons
    return $ Graph.Graph gname elements terms schemaGraph
  where
    cx = Standard.standardContext
    terms = const True
    schemaGraph = hydraCoreName


(<.>) :: Data (b -> c) -> Data (a -> b) -> Data (a -> c)
f <.> g = compose f g

($$) :: Data (a -> b) -> Data a -> Data b
f $$ x = apply f x

(@@) :: Data (a -> b) -> Data a -> Data b
f @@ x = apply f x
--Data lhs @@ Data rhs = Data $ Terms.apply lhs rhs

infixr 0 @->
(@->) :: a -> b -> (a, b)
x @-> y = (x, y)

infixr 0 -->
(-->) :: Case a -> Data (a -> b) -> Field Meta
c --> t = caseField c t

(++) :: Data String -> Data String -> Data String
l ++ r = Strings.cat @@ list [l, r]

apply :: Data (a -> b) -> Data a -> Data b
apply (Data lhs) (Data rhs) = Data $ Terms.apply lhs rhs

apply2 :: Data (a -> b -> c) -> Data a -> Data b -> Data c
apply2 (Data f) (Data a1) (Data a2) = Data $ Terms.apply (Terms.apply f a1) a2

caseField :: Case a -> Data (a -> b) -> Field Meta -- Data (u -> b)
caseField (Case fname) (Data f) = Field fname $ Terms.lambda "x" $ Terms.apply f (Terms.variable "x")

compareTo :: Data a -> Data (a -> Bool)
compareTo (Data term) = Data $ Terms.compareTo term

compose :: Data (b -> c) -> Data (a -> b) -> Data (a -> c)
compose (Data f) (Data g) = Data $ Terms.lambda "x" $ Terms.apply f (Terms.apply g $ Terms.variable "x")

constant :: Data a -> Data (b -> a)
constant (Data term) = Data $ Terms.lambda "_" term

denom :: Name -> Data (a -> b)
denom = Data . Terms.eliminateNominal

delta :: Data (Ref a -> a)
delta = Data Terms.delta

doc :: String -> Data a -> Data a
doc s (Data term) = Data $ setTermDescription Standard.standardContext (Just s) term

element :: Element a -> Data (Ref a)
element (Element name _) = Data $ Terms.element name

field :: FieldName -> Data a -> Field Meta
field fname (Data val) = Field fname val

function :: Type Meta -> Type Meta -> Data a -> Data a
function dom cod = typed (Types.function dom cod)

lambda :: String -> Data x -> Data (a -> b)
lambda v (Data body) = Data $ Terms.lambda v body

--letTerm :: Var a -> Data a -> Data b -> Data b
--letTerm (Var k) (Data v) (Data env) = Data $ Terms.letTerm (Variable k) v env

list :: [Data a] -> Data [a]
list els = Data $ Terms.list (unData <$> els)

map :: M.Map (Data a) (Data b) -> Data (M.Map a b)
map = Data . Terms.map . M.fromList . fmap fromData . M.toList
  where
    fromData (Data k, Data v) = (k, v)

matchData :: Name -> Type Meta -> [(FieldName, Data (x -> b))] -> Data (a -> b)
matchData name cod pairs = typed (Types.function (Types.nominal name) cod) $ Data $ Terms.cases name (toField <$> pairs)
  where
    toField (fname, Data term) = Field fname term

matchOpt :: Data b -> Data (a -> b) -> Data (Maybe a -> b)
matchOpt (Data n) (Data j) = Data $ Terms.matchOptional n j

match :: Name -> Type Meta -> [Field Meta] -> Data (u -> b)
match name cod fields = function (Types.nominal name) cod $ Data $ Terms.cases name fields

matchToEnum :: Name -> Name -> [(FieldName, FieldName)] -> Data (a -> b)
matchToEnum name codName pairs = matchData name (Types.nominal codName) (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Name -> Name -> [(FieldName, Field Meta)] -> Data (a -> b)
matchToUnion name codName pairs = matchData name (Types.nominal codName) (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName,
      constant $ typed (Types.nominal codName) $ Data $ Terms.union codName fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Data a -> Data b
nom name (Data term) = Data $ Terms.nominal name term

opt :: Maybe (Data a) -> Data (Maybe a)
opt mc = Data $ Terms.optional (unData <$> mc)

primitive :: Name -> Data a
primitive = Data . Terms.primitive

project :: Name -> Type Meta -> FieldName -> Data (a -> b)
project name cod fname = function (Types.nominal name) cod $
  Data $ Terms.projection name fname

record :: Name -> [Field Meta] -> Data a
record name fields = Data $ Terms.record name fields

ref :: Element a -> Data a
ref e = delta @@ element e

--ref :: Element a -> Data (Ref a)
--ref (Element name _) = Data $ Terms.element name

set :: S.Set (Data a) -> Data (S.Set a)
set = Data . Terms.set . S.fromList . fmap unData . S.toList

-- TODO: type abstraction

-- TODO: type application

typed :: Type Meta -> Data a -> Data a
typed t (Data term) = Data $ Standard.typed t term

union :: Name -> FieldName -> Data a -> Data b
union name fname (Data term) = Data $ Terms.union name (Field fname term)

union2 :: Name -> FieldName -> Data (a -> b)
union2 name fname = lambda "x" $ typed (Types.nominal name) $ union name fname $ var "x"

unit :: Data a
unit = Data Terms.unit

unitVariant :: Name -> FieldName -> Data a
unitVariant name fname = typed (Types.nominal name) $ Data $ Terms.union name $ Field fname Terms.unit

var :: String -> Data a
var v = Data $ Terms.variable v

variant :: Name -> FieldName -> Data a -> Data b
variant name fname (Data term) = typed (Types.nominal name) $ Data $ Terms.union name $ Field fname term
