-- | Phantom-typed term DSL for the hydra.lib.lists library

module Hydra.Dsl.Lib.Lists where

import Hydra.Phantoms
import Hydra.Dsl.Meta.Phantoms
import qualified Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries


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

drop :: TTerm Int -> TTerm [a] -> TTerm [a]
drop = primitive2 _lists_drop

dropWhile :: TTerm (a -> Bool) -> TTerm [a] -> TTerm [a]
dropWhile = primitive2 _lists_dropWhile

elem :: Eq a => TTerm a -> TTerm [a] -> TTerm Bool
elem = primitive2 _lists_elem

filter :: TTerm (a -> Bool) -> TTerm [a] -> TTerm [a]
filter = primitive2 _lists_filter

foldl :: TTerm (b -> a -> b) -> TTerm b -> TTerm [a] -> TTerm b
foldl = primitive3 _lists_foldl

group :: Eq a => TTerm [a] -> TTerm [[a]]
group = primitive1 _lists_group

head :: TTerm [a] -> TTerm a
head = primitive1 _lists_head

init :: TTerm [a] -> TTerm [a]
init = primitive1 _lists_init

intercalate :: TTerm [a] -> TTerm [[a]] -> TTerm [a]
intercalate = primitive2 _lists_intercalate

intersperse :: TTerm a -> TTerm [a] -> TTerm [a]
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

replicate :: TTerm Int -> TTerm a -> TTerm [a]
replicate = primitive2 _lists_replicate

reverse :: TTerm [a] -> TTerm [a]
reverse = primitive1 _lists_reverse

safeHead :: TTerm [a] -> TTerm (Maybe a)
safeHead = primitive1 _lists_safeHead

singleton :: TTerm a -> TTerm [a]
singleton = primitive1 _lists_singleton

sort :: TTerm [a] -> TTerm [a]
sort = primitive1 _lists_sort

sortOn :: TTerm (a -> b) -> TTerm [a] -> TTerm [a]
sortOn = primitive2 _lists_sortOn

span :: TTerm (a -> Bool) -> TTerm [a] -> TTerm ([a], [a])
span = primitive2 _lists_span

tail :: TTerm [a] -> TTerm [a]
tail = primitive1 _lists_tail

take :: TTerm Int -> TTerm [a] -> TTerm [a]
take = primitive2 _lists_take

transpose :: TTerm [[a]] -> TTerm [[a]]
transpose = primitive1 _lists_transpose

zip :: TTerm [a] -> TTerm [b] -> TTerm [(a, b)]
zip = primitive2 _lists_zip

zipWith :: TTerm (a -> b -> c) -> TTerm [a] -> TTerm [b] -> TTerm [c]
zipWith = primitive3 _lists_zipWith
