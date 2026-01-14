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

-- | Apply a function in a Flow to a value in a Flow (applicative).
apply :: Flow s (x -> y) -> Flow s x -> Flow s y
apply = (<*>)

-- | Monadic bind for Flow.
bind :: Flow s x -> (x -> Flow s y) -> Flow s y
bind = Monads.bind

-- | Create a failed Flow with an error message.
fail :: String -> Flow s x
fail = Monads.fail

-- | Fold over a list with a monadic function.
foldl :: (a -> b -> Flow s a) -> a -> [b] -> Flow s a
foldl = CM.foldM

-- | Map a function over a Flow (functor).
map :: (x -> y) -> Flow s x -> Flow s y
map = Monads.map

-- | Map a monadic function over the values of a map.
mapElems :: Ord k => (v1 -> Flow s v2) -> M.Map k v1 -> Flow s (M.Map k v2)
mapElems f m = M.fromList <$> (CM.mapM (\(k, v) -> (,) <$> Monads.pure k <*> f v) $ M.toList m)

-- | Map a monadic function over the keys of a map.
mapKeys :: Ord k2 => (k1 -> Flow s k2) -> M.Map k1 v -> Flow s (M.Map k2 v)
mapKeys f m = M.fromList <$> (CM.mapM (\(k, v) -> (,) <$> f k <*> Monads.pure v) $ M.toList m)

-- | Map a monadic function over a list.
mapList :: (x -> Flow s y) -> [x] -> Flow s [y]
mapList = CM.mapM

-- | Map a monadic function over an optional value.
mapMaybe :: (x -> Flow s y) -> Maybe x -> Flow s (Maybe y)
mapMaybe = traverse

-- | Map a monadic function over a set.
mapSet :: Ord y => (x -> Flow s y) -> S.Set x -> Flow s (S.Set y)
mapSet f xs = S.fromList <$> (CM.mapM f $ S.toList xs)

-- | Lift a value into a Flow.
pure :: x -> Flow s x
pure = Monads.pure

-- | Sequence a list of Flows into a Flow of a list.
sequence :: [Flow s x] -> Flow s [x]
sequence = CM.sequence
