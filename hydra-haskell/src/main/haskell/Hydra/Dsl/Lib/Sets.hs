module Hydra.Dsl.Lib.Sets where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Base

import Data.Set


contains :: TTerm a -> TTerm (Set a) -> TTerm Bool
contains = primitive2 _sets_contains

difference :: TTerm (Set a) -> TTerm (Set a) -> TTerm (Set a)
difference = primitive2 _sets_difference

empty :: TTerm (Set a)
empty = primitive _sets_empty

fromList :: TTerm [a] -> TTerm (Set a)
fromList = primitive1 _sets_fromList

insert :: TTerm a -> TTerm (Set a) -> TTerm (Set a)
insert = primitive2 _sets_insert

intersection :: TTerm (Set a) -> TTerm (Set a) -> TTerm (Set a)
intersection = primitive2 _sets_intersection

isEmpty :: TTerm (Set a) -> TTerm Bool
isEmpty = primitive1 _sets_isEmpty

map :: TTerm (a -> b) -> TTerm (Set a) -> TTerm (Set b)
map = primitive2 _sets_map

remove :: TTerm a -> TTerm (Set a) -> TTerm (Set a)
remove = primitive2 _sets_remove

singleton :: TTerm a -> TTerm (Set a)
singleton = primitive1 _sets_singleton

size :: TTerm (Set a) -> TTerm Int
size = primitive1 _sets_size

toList :: TTerm (Set a) -> TTerm [a]
toList = primitive1 _sets_toList

union :: TTerm (Set a) -> TTerm (Set a) -> TTerm (Set a)
union = primitive2 _sets_union
