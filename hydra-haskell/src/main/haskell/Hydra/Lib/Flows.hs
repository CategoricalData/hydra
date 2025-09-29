-- | Haskell implementations of hydra.lib.flows primitives. These are simply wrappers around hydra.flows functions.

module Hydra.Lib.Flows where

import Hydra.Compute
import qualified Hydra.Monads as Monads

import qualified Control.Monad as CM
import qualified Data.Map as M
import qualified Data.Set as S


-- Haskell-specific helpers

instance Functor (Flow s) where
  fmap = CM.liftM
instance Applicative (Flow s) where
  pure = Monads.pure
  (<*>) = CM.ap
instance Monad (Flow s) where
  (>>=) = Monads.bind
instance MonadFail (Flow s) where
  fail = Monads.fail

-- Primitive functions

apply :: Flow s (x -> y) -> Flow s x -> Flow s y
apply = (<*>)

bind :: Flow s x -> (x -> Flow s y) -> Flow s y
bind = Monads.bind

fail :: String -> Flow s x
fail = Monads.fail

foldl :: (a -> b -> Flow s a) -> a -> [b] -> Flow s a
foldl = CM.foldM

map :: (x -> y) -> Flow s x -> Flow s y
map = Monads.map

mapElems :: Ord k => (v1 -> Flow s v2) -> M.Map k v1 -> Flow s (M.Map k v2)
mapElems f m = M.fromList <$> (CM.mapM (\(k, v) -> (,) <$> Monads.pure k <*> f v) $ M.toList m)

mapKeys :: Ord k2 => (k1 -> Flow s k2) -> M.Map k1 v -> Flow s (M.Map k2 v)
mapKeys f m = M.fromList <$> (CM.mapM (\(k, v) -> (,) <$> f k <*> Monads.pure v) $ M.toList m)

mapList :: (x -> Flow s y) -> [x] -> Flow s [y]
mapList = CM.mapM

mapOptional :: (x -> Flow s y) -> Maybe x -> Flow s (Maybe y)
mapOptional = traverse

mapSet :: Ord y => (x -> Flow s y) -> S.Set x -> Flow s (S.Set y)
mapSet f xs = S.fromList <$> (CM.mapM f $ S.toList xs)

pure :: x -> Flow s x
pure = Monads.pure

sequence :: [Flow s x] -> Flow s [x]
sequence = CM.sequence
