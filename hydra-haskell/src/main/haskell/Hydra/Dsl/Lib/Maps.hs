module Hydra.Dsl.Lib.Maps where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms

import Data.Map


empty :: Datum (Map k v)
empty = Datum $ Terms.primitive _maps_empty

fromList :: Datum ([(k, v)] -> Map k v)
fromList = Datum $ Terms.primitive _maps_fromList

insert :: Datum (k -> v -> Map k v -> Map k v)
insert = Datum $ Terms.primitive _maps_insert

isEmpty :: Datum (Map k v -> Bool)
isEmpty = Datum $ Terms.primitive _maps_isEmpty

lookup :: Datum (k -> Map k v -> Maybe v)
lookup = Datum $ Terms.primitive _maps_lookup

map :: Datum ((v1 -> v2) -> Map k v1 -> Map k v2)
map = Datum $ Terms.primitive _maps_map

remove :: Datum (k -> Map k v -> Map k v)
remove = Datum $ Terms.primitive _maps_remove

singleton :: Datum (k -> v -> Map k v)
singleton = Datum $ Terms.primitive _maps_singleton

size :: Datum (Map k v -> Int)
size = Datum $ Terms.primitive _maps_size

toList :: Datum (Map k v -> [(k, v)])
toList = Datum $ Terms.primitive _maps_toList
