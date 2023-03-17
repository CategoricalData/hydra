-- | Haskell implementations of hydra/lib/lists primitives

module Hydra.Lib.Lists where

import Hydra.Compute
import Hydra.Core
import Hydra.Graph
import qualified Hydra.Dsl.Terms as Terms
import qualified Data.List as L
import qualified Hydra.Dsl.Terms as Terms


apply :: [a -> b] -> [a] -> [b]
apply = (<*>)

applyRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
applyRaw funs' args' = do
    funs <- Terms.expectList Prelude.pure funs'
    args <- Terms.expectList Prelude.pure args'
    return $ Terms.list $ L.concat (helper args <$> funs)
  where
    helper args f = Terms.apply f <$> args

bind :: [a] -> (a -> [b]) -> [b]
bind = (>>=)

bindRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
bindRaw args' fun = do
    args <- Terms.expectList Prelude.pure args'
    return $ Terms.apply (Terms.primitive $ Name "hydra/lib/lists.concat") (Terms.list $ Terms.apply fun <$> args)

concat :: [[a]] -> [a]
concat = L.concat

head :: [a] -> a
head = L.head

intercalate :: [a] -> [[a]] -> [a]
intercalate = L.intercalate

intersperse :: a -> [a] -> [a]
intersperse = L.intersperse

last :: [a] -> a
last = L.last

length :: [a] -> Int
length = L.length

map :: (a -> b) -> [a] -> [b]
map = fmap

mapRaw :: Show a => Term a -> Term a -> Flow (Graph a) (Term a)
mapRaw fun args' = do
    args <- Terms.expectList Prelude.pure args'
    return $ Terms.list (Terms.apply fun <$> args)

pure :: a -> [a]
pure x = [x]
