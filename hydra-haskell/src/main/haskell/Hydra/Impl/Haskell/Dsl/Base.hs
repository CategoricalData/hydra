module Hydra.Impl.Haskell.Dsl.Base (
  module Hydra.Impl.Haskell.Dsl.Base,
  module Hydra.Impl.Haskell.Dsl.Lib.Lists,
  module Hydra.Impl.Haskell.Dsl.Lib.Literals,
  module Hydra.Impl.Haskell.Dsl.Lib.Math,
  module Hydra.Impl.Haskell.Dsl.Lib.Sets,
  module Hydra.Impl.Haskell.Dsl.Lib.Strings,
  module Hydra.Impl.Haskell.Dsl.Literals,
  module Hydra.Impl.Haskell.Dsl.Phantoms,
) where

import Hydra.Common
import Hydra.Core
import Hydra.CoreEncoding
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Literals

import Hydra.Impl.Haskell.Dsl.Lib.Lists
import Hydra.Impl.Haskell.Dsl.Lib.Literals
import Hydra.Impl.Haskell.Dsl.Lib.Math
import Hydra.Impl.Haskell.Dsl.Lib.Sets
import Hydra.Impl.Haskell.Dsl.Lib.Strings

import Hydra.Impl.Haskell.Dsl.Phantoms
import Hydra.Types.Inference
import qualified Hydra.Graph as Graph
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms

import Data.Map as M
import Data.Set as S


toElement :: Context Meta -> El a -> Result (Graph.Element Meta)
toElement cx (El mod local (Trm term)) = do
    t <- typeSchemeType . snd <$> inferType cx term
    let schemaTerm = encodeType cx t
    return $ Graph.Element name schemaTerm term
  where
    name = fromQname mod local


apply :: Trm (a -> b) -> Trm a -> Trm b
apply (Trm lhs) (Trm rhs) = Trm $ Terms.apply lhs rhs

apply2 :: Trm (a -> b -> c) -> Trm a -> Trm b -> Trm c
apply2 (Trm f) (Trm a1) (Trm a2) = Trm $ Terms.apply (Terms.apply f a1) a2

compareTo :: Trm a -> Trm (a -> Bool)
compareTo (Trm term) = Trm $ Terms.compareTo term

denom :: Graph.GraphName -> String -> Trm a
denom gname local = Trm $ Terms.eliminateNominal (fromQname gname local)

deref :: Trm (Ref a -> a)
deref = Trm Terms.delta

field :: FieldName -> Trm a -> Fld
field fname (Trm term) = Fld $ Field fname term

lambda :: Var a -> Trm x -> Trm (a -> b)
lambda (Var v) (Trm body) = Trm $ Terms.lambda v body

letTerm :: Var a -> Trm a -> Trm b -> Trm b
letTerm (Var k) (Trm v) (Trm env) = Trm $ Terms.letData (Variable k) v env

list :: [Trm a] -> Trm [a]
list els = Trm $ Terms.list (unTrm <$> els)

map :: Map (Trm a) (Trm b) -> Trm (Map a b)
map = Trm . Terms.map . M.fromList . fmap fromTrm . M.toList
  where
    fromTrm (Trm k, Trm v) = (k, v)

match :: [(FieldName, Trm (Any -> a))] -> Trm (Union -> a)
match pairs = Trm $ Terms.cases (toField <$> pairs)
  where
    toField (fname, Trm term) = Field fname term

matchOpt :: Trm b -> Trm (a -> b) -> Trm (Maybe a -> b)
matchOpt (Trm n) (Trm j) = Trm $ Terms.matchOptional n j

-- Note: the phantom types provide no guarantee of type safety in this case
nom :: Graph.GraphName -> String -> Trm a -> Trm b
nom gname local (Trm term) = Trm $ Terms.nominal (fromQname gname local) term

opt :: Maybe (Trm a) -> Trm (Maybe a)
opt mc = Trm $ Terms.optional (unTrm <$> mc)

project :: FieldName -> Trm (Record -> a)
project fname = Trm $ Terms.projection fname

record :: [Fld] -> Trm Record
record flds = Trm $ Terms.record (unFld <$> flds)

ref :: El a -> Trm (Ref a)
ref (El gname local _) = Trm $ Terms.element $ fromQname gname local

set :: Set (Trm a) -> Trm (Set a)
set = Trm . Terms.set . S.fromList . fmap unTrm . S.toList

-- TODO: type abstraction

-- TODO: type application

union :: Fld -> Trm Union
union (Fld field) = Trm $ Terms.union field

unit :: Trm Record
unit = Trm Terms.unitData

var :: Var a -> Trm a
var (Var v) = Trm $ Terms.variable v
