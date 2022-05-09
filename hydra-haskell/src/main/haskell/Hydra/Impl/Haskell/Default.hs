module Hydra.Impl.Haskell.Default where

import Hydra.Core

import qualified Data.Map as M


class Default a where dflt :: a
instance Default () where dflt = ()
instance Default [a] where dflt = []
instance Default Int where dflt = 0
instance Default Meta where dflt = Meta M.empty
