module Hydra.Dsl.Lib.Lists where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


concat :: Datum ([[a]] -> [a])
concat = Datum $ Terms.primitive _lists_concat

concat2 :: Datum ([a] -> [a] -> [a])
concat2 = Datum $ Terms.primitive _lists_concat2

cons :: Datum (a -> [a] -> [a])
cons = Datum $ Terms.primitive _lists_cons

head :: Datum ([a] -> a)
head = Datum $ Terms.primitive _lists_head

intercalate :: Datum ([a] -> [[a]] -> [a])
intercalate = Datum $ Terms.primitive _lists_intercalate

intersperse :: Datum ([a] -> a -> [a])
intersperse = Datum $ Terms.primitive _lists_intersperse

last :: Datum ([a] -> a)
last = Datum $ Terms.primitive _lists_last

length :: Datum ([a] -> Int)
length = Datum $ Terms.primitive _lists_length

map :: Datum ((a -> b) -> [a] -> [b])
map = Datum $ Terms.primitive _lists_map

nub :: Eq a => Datum ([a] -> [a])
nub = Datum $ Terms.primitive _lists_nub

null :: Datum ([a] -> Bool)
null = Datum $ Terms.primitive _lists_null

pure :: Datum (a -> [a])
pure = Datum $ Terms.primitive _lists_pure

reverse :: Datum ([a] -> [a])
reverse = Datum $ Terms.primitive _lists_reverse

safeHead :: Datum ([a] -> Maybe a)
safeHead = Datum $ Terms.primitive _lists_safeHead

tail :: Datum ([a] -> [a])
tail = Datum $ Terms.primitive _lists_tail
