module Hydra.Impl.Haskell.Dsl.Base (
  module Hydra.Impl.Haskell.Dsl.Base,
  module Hydra.Impl.Haskell.Dsl.Literals,
  module Hydra.Impl.Haskell.Dsl.Phantoms,
  standardContext,
) where

import Hydra.Common
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Literals
import Hydra.Impl.Haskell.Meta
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Graph as Graph
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Core
import qualified Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y


el :: El a -> Context Meta -> Result (Graph.Element Meta)
el (El name (Trm term)) cx = do
    --t <- typeSchemeType . snd <$> inferType cx term
    mt <- contextTypeOf cx $ termMeta term
    t <- Y.maybe (fail $ "untyped term in " ++ unName name ++ ": " ++ show term) return mt
    let schemaTerm = encodeType cx t
    return $ Graph.Element name schemaTerm term
    
graph :: Graph.GraphName -> [Context Meta -> Result (Graph.Element Meta)] -> Result (Graph.Graph Meta)
graph gname cons = do
    elements <- mapM (\f -> f cx) cons
    return $ Graph.Graph gname elements terms schemaGraph
  where
    cx = standardContext
    terms = const True
    schemaGraph = hydraCoreName


(@.) :: Trm (b -> c) -> Trm (a -> b) -> Trm (a -> c)
f @. g = compose f g

(.$) :: Trm (a -> b) -> Trm a -> Trm b
f .$ x = apply f x

infixr 0 @->
(@->) :: a -> b -> (a, b)
x @-> y = (x, y)

infixr 0 .->
(.->) :: Case a -> Trm (a -> b) -> Field Meta
c .-> t = caseField c t

(.++) :: Trm String -> Trm String -> Trm String
l .++ r = apply Strings.cat $ list [l, r]

apply :: Trm (a -> b) -> Trm a -> Trm b
apply (Trm lhs) (Trm rhs) = Trm $ Terms.apply lhs rhs

apply2 :: Trm (a -> b -> c) -> Trm a -> Trm b -> Trm c
apply2 (Trm f) (Trm a1) (Trm a2) = Trm $ Terms.apply (Terms.apply f a1) a2

at :: El a -> Trm a
at e = apply delta (element e)

caseField :: Case a -> Trm (a -> b) -> Field Meta -- Trm (u -> b)
caseField (Case fname) (Trm f) = Field fname $ Terms.lambda "x" $ Terms.apply f (Terms.variable "x")

compareTo :: Trm a -> Trm (a -> Bool)
compareTo (Trm term) = Trm $ Terms.compareTo term

compose :: Trm (b -> c) -> Trm (a -> b) -> Trm (a -> c)
compose (Trm f) (Trm g) = Trm $ Terms.lambda "x" $ Terms.apply f (Terms.apply g $ Terms.variable "x")

constant :: Trm a -> Trm (b -> a)
constant = lambda "_"

denom :: Name -> Trm (a -> b)
denom = Trm . Terms.eliminateNominal

delta :: Trm (Ref a -> a)
delta = Trm Terms.delta

doc :: String -> Trm a -> Trm a
doc s (Trm (Term expr meta)) = Trm $ Term expr $ setDescription (Just s) meta

element :: El a -> Trm (Ref a)
element (El name _) = Trm $ Terms.element name

field :: FieldName -> Trm a -> Field Meta
field fname (Trm val) = Field fname val

lambda :: String -> Trm x -> Trm (a -> b)
lambda v (Trm body) = Trm $ Terms.lambda v body

letTerm :: Var a -> Trm a -> Trm b -> Trm b
letTerm (Var k) (Trm v) (Trm env) = Trm $ Terms.letTerm (Variable k) v env

list :: [Trm a] -> Trm [a]
list els = Trm $ Terms.list (unTrm <$> els)

map :: M.Map (Trm a) (Trm b) -> Trm (M.Map a b)
map = Trm . Terms.map . M.fromList . fmap fromTrm . M.toList
  where
    fromTrm (Trm k, Trm v) = (k, v)

match :: Type Meta -> Type Meta -> [(FieldName, Trm (x -> b))] -> Trm (a -> b)
match dom cod pairs = typed (Types.function dom cod) $ Trm $ Terms.cases (toField <$> pairs)
  where
    toField (fname, Trm term) = Field fname term

matchOpt :: Trm b -> Trm (a -> b) -> Trm (Maybe a -> b)
matchOpt (Trm n) (Trm j) = Trm $ Terms.matchOptional n j

matchSafe :: Type Meta -> Type Meta -> [Field Meta] -> Trm (u -> b)
matchSafe dom cod fields = typed (Types.function dom cod) $ Trm $ Terms.cases fields

matchToEnum :: Type Meta -> Name -> [(FieldName, FieldName)] -> Trm (a -> b)
matchToEnum dom codName pairs =   match dom (Types.nominal codName) (toCase <$> pairs)
  where
    toCase (fromName, toName) = (fromName, constant $ unitVariant codName toName)

matchToUnion :: Type Meta -> Name -> [(FieldName, Field Meta)] -> Trm (a -> b)
matchToUnion dom codName pairs = match dom (Types.nominal codName) (toCase <$> pairs)
  where
    toCase (fromName, fld) = (fromName,
      constant $ typed (Types.nominal codName) $ Trm $ Terms.union fld)

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Name -> Trm a -> Trm b
nom name (Trm term) = Trm $ Terms.nominal name term

opt :: Maybe (Trm a) -> Trm (Maybe a)
opt mc = Trm $ Terms.optional (unTrm <$> mc)

project :: Type Meta -> Type Meta -> FieldName -> Trm (a -> b)
project dom cod fname = typed (Types.function dom cod) $
  Trm $ Terms.projection fname

record :: [Field Meta] -> Trm a
record = Trm . Terms.record

ref :: El a -> Trm (Ref a)
ref (El name _) = Trm $ Terms.element name

set :: S.Set (Trm a) -> Trm (S.Set a)
set = Trm . Terms.set . S.fromList . fmap unTrm . S.toList

-- TODO: type abstraction

-- TODO: type application

typed :: Type Meta -> Trm a -> Trm a
typed t (Trm (Term d m)) = Trm $ Term d (setType standardContext (Just t) m)

union :: FieldName -> Trm a -> Trm b
union fname (Trm term) = Trm $ Terms.union (Field fname term)

union2 :: Name -> FieldName -> Trm (a -> b)
union2 name fname = lambda "x" $ typed (Types.nominal name) $ union fname $ var "x"

unit :: Trm a
unit = Trm Terms.unitTerm

unitVariant :: Name -> FieldName -> Trm a
unitVariant name fname = typed (Types.nominal name) $ Trm $ Terms.union $ Field fname Terms.unitTerm

var :: String -> Trm a
var v = Trm $ Terms.variable v

variant :: Name -> FieldName -> Trm a -> Trm b
variant name fname (Trm term) = typed (Types.nominal name) $ Trm $ Terms.union $ Field fname term
