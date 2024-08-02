module Hydra.Dsl.Lib.Sets where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Set


contains :: TTerm (a -> Set a -> Bool)
contains = TTerm $ Terms.primitive _sets_contains

difference :: TTerm (Set a -> Set a -> Set a)
difference = TTerm $ Terms.primitive _sets_difference

empty :: TTerm (Set a)
empty = TTerm $ Terms.primitive _sets_empty

fromList :: TTerm ([a] -> Set a)
fromList = TTerm $ Terms.primitive _sets_fromList

insert :: TTerm (a -> Set a -> Set a)
insert = TTerm $ Terms.primitive _sets_insert

intersection :: TTerm (Set a -> Set a -> Set a)
intersection = TTerm $ Terms.primitive _sets_intersection

isEmpty :: TTerm (Set a -> Bool)
isEmpty = TTerm $ Terms.primitive _sets_isEmpty

map :: TTerm ((a -> b) -> Set a -> Set b)
map = TTerm $ Terms.primitive _sets_map

remove :: TTerm (a -> Set a -> Set a)
remove = TTerm $ Terms.primitive _sets_remove

singleton :: TTerm (a -> Set a)
singleton = TTerm $ Terms.primitive _sets_singleton

size :: TTerm (Set a -> Int)
size = TTerm $ Terms.primitive _sets_size

toList :: TTerm (Set a -> [a])
toList = TTerm $ Terms.primitive _sets_toList

union :: TTerm (Set a -> Set a -> Set a)
union = TTerm $ Terms.primitive _sets_union
