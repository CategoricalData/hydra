module Hydra.Dsl.Lib.Lists where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Dsl.Phantoms


apply :: TTerm [a -> b] -> TTerm [a] -> TTerm [b]
apply = primitive2 _lists_apply

at :: TTerm Int -> TTerm [a] -> TTerm a
at = primitive2 _lists_at

bind :: TTerm [a] -> TTerm (a -> [b]) -> TTerm [b]
bind = primitive2 _lists_bind

concat :: TTerm [[a]] -> TTerm [a]
concat = primitive1 _lists_concat

concat2 :: TTerm [a] -> TTerm [a] -> TTerm [a]
concat2 = primitive2 _lists_concat2

cons :: TTerm a -> TTerm [a] -> TTerm [a]
cons = primitive2 _lists_cons

elem :: Eq a => TTerm a -> TTerm [a] -> TTerm Bool
elem = primitive2 _lists_elem

filter :: TTerm (a -> Bool) -> TTerm [a] -> TTerm [a]
filter = primitive2 _lists_filter

foldl :: TTerm (b -> a -> b) -> TTerm b -> TTerm [a] -> TTerm b
foldl = primitive3 _lists_foldl

head :: TTerm [a] -> TTerm a
head = primitive1 _lists_head

intercalate :: TTerm [a] -> TTerm [[a]] -> TTerm [a]
intercalate = primitive2 _lists_intercalate

intersperse :: TTerm [a] -> TTerm a -> TTerm [a]
intersperse = primitive2 _lists_intersperse

last :: TTerm [a] -> TTerm a
last = primitive1 _lists_last

length :: TTerm [a] -> TTerm Int
length = primitive1 _lists_length

map :: TTerm (a -> b) -> TTerm [a] -> TTerm [b]
map = primitive2 _lists_map

nub :: Eq a => TTerm [a] -> TTerm [a]
nub = primitive1 _lists_nub

null :: TTerm [a] -> TTerm Bool
null = primitive1 _lists_null

pure :: TTerm a -> TTerm [a]
pure = primitive1 _lists_pure

reverse :: TTerm [a] -> TTerm [a]
reverse = primitive1 _lists_reverse

safeHead :: TTerm [a] -> TTerm (Maybe a)
safeHead = primitive1 _lists_safeHead

tail :: TTerm [a] -> TTerm [a]
tail = primitive1 _lists_tail

take :: TTerm Int -> TTerm [a] -> TTerm [a]
take = primitive2 _lists_take

zip :: TTerm [a] -> TTerm [b] -> TTerm [(a, b)]
zip = primitive2 _lists_zip

zipWith :: TTerm (a -> b -> c) -> TTerm [a] -> TTerm [b] -> TTerm [c]
zipWith = primitive3 _lists_zipWith
