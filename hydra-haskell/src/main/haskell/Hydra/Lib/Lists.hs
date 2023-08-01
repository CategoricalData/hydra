-- | Haskell implementations of hydra/lib/lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Expect as Expect
import qualified Hydra.Dsl.Terms as Terms
import qualified Data.List as L
import qualified Hydra.Dsl.Terms as Terms


apply :: [x -> y] -> [x] -> [y]
apply = (<*>)

applyRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
applyRaw funs' args' = do
    funs <- Expect.list Prelude.pure funs'
    args <- Expect.list Prelude.pure args'
    return $ Terms.list $ L.concat (helper args <$> funs)
  where
    helper args f = Terms.apply f <$> args

bind :: [x] -> (x -> [y]) -> [y]
bind = (>>=)

bindRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
bindRaw args' fun = do
    args <- Expect.list Prelude.pure args'
    return $ Terms.apply (Terms.primitive $ Name "hydra/lib/lists.concat") (Terms.list $ Terms.apply fun <$> args)

concat :: [[x]] -> [x]
concat = L.concat

concat2 :: [x] -> [x] -> [x]
concat2 l1 l2 = l1 ++ l2

cons :: x -> [x] -> [x]
cons = (:)

head :: [x] -> x
head = L.head

intercalate :: [x] -> [[x]] -> [x]
intercalate = L.intercalate

intersperse :: x -> [x] -> [x]
intersperse = L.intersperse

last :: [x] -> x
last = L.last

length :: [x] -> Int
length = L.length

map :: (x -> y) -> [x] -> [y]
map = fmap

mapRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
mapRaw fun args' = do
    args <- Expect.list Prelude.pure args'
    return $ Terms.list (Terms.apply fun <$> args)

pure :: x -> [x]
pure e = [e]

reverse :: [x] -> [x]
reverse = L.reverse

tail :: [x] -> [x]
tail = L.tail
