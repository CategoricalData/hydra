module Hydra.Dsl.Lib.Sets where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Set


contains :: Datum (a -> Set a -> Bool)
contains = Datum $ Terms.primitive _sets_contains

difference :: Datum (Set a -> Set a -> Set a)
difference = Datum $ Terms.primitive _sets_difference

empty :: Datum (Set a)
empty = Datum $ Terms.primitive _sets_empty

fromList :: Datum ([a] -> Set a)
fromList = Datum $ Terms.primitive _sets_fromList

insert :: Datum (a -> Set a -> Set a)
insert = Datum $ Terms.primitive _sets_insert

intersection :: Datum (Set a -> Set a -> Set a)
intersection = Datum $ Terms.primitive _sets_intersection

isEmpty :: Datum (Set a -> Bool)
isEmpty = Datum $ Terms.primitive _sets_isEmpty

map :: Datum ((a -> b) -> Set a -> Set b)
map = Datum $ Terms.primitive _sets_map

remove :: Datum (a -> Set a -> Set a)
remove = Datum $ Terms.primitive _sets_remove

singleton :: Datum (a -> Set a)
singleton = Datum $ Terms.primitive _sets_singleton

size :: Datum (Set a -> Int)
size = Datum $ Terms.primitive _sets_size

toList :: Datum (Set a -> [a])
toList = Datum $ Terms.primitive _sets_toList

union :: Datum (Set a -> Set a -> Set a)
union = Datum $ Terms.primitive _sets_union
