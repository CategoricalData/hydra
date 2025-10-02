-- | Phantom-typed term DSL for the hydra.lib.sets library

module Hydra.Dsl.Lib.Sets where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms

import Data.Set


delete :: TTerm a -> TTerm (Set a) -> TTerm (Set a)
delete = primitive2 _sets_delete

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

map :: TTerm (a -> b) -> TTerm (Set a) -> TTerm (Set b)
map = primitive2 _sets_map

member :: TTerm a -> TTerm (Set a) -> TTerm Bool
member = primitive2 _sets_member

null :: TTerm (Set a) -> TTerm Bool
null = primitive1 _sets_null

singleton :: TTerm a -> TTerm (Set a)
singleton = primitive1 _sets_singleton

size :: TTerm (Set a) -> TTerm Int
size = primitive1 _sets_size

toList :: TTerm (Set a) -> TTerm [a]
toList = primitive1 _sets_toList

union :: TTerm (Set a) -> TTerm (Set a) -> TTerm (Set a)
union = primitive2 _sets_union

unions :: TTerm [Set a] -> TTerm (Set a)
unions = primitive1 _sets_unions
