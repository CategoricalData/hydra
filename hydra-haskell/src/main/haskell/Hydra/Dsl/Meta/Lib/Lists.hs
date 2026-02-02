-- | Phantom-typed term DSL for the hydra.lib.lists library

{-# LANGUAGE FlexibleContexts #-}

module Hydra.Dsl.Meta.Lib.Lists where

import Hydra.Phantoms
import Hydra.Dsl.AsTerm
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

filter :: AsTerm t [a] => TTerm (a -> Bool) -> t -> TTerm [a]
filter p xs = primitive2 _lists_filter p (asTerm xs)

find :: TTerm (a -> Bool) -> TTerm [a] -> TTerm (Maybe a)
find = primitive2 _lists_find

foldl :: AsTerm f (b -> a -> b) => f -> TTerm b -> TTerm [a] -> TTerm b
foldl f = primitive3 _lists_foldl (asTerm f)

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

-- | Map a function over a list
-- Accepts TTerm or TBinding for both arguments (via AsTerm)
map :: (AsTerm f (a -> b), AsTerm t [a]) => f -> t -> TTerm [b]
map f l = primitive2 _lists_map (asTerm f) (asTerm l)

nub :: Eq a => TTerm [a] -> TTerm [a]
nub = primitive1 _lists_nub

null :: TTerm [a] -> TTerm Bool
null = primitive1 _lists_null

partition :: TTerm (a -> Bool) -> TTerm [a] -> TTerm ([a], [a])
partition = primitive2 _lists_partition

pure :: TTerm a -> TTerm [a]
pure = primitive1 _lists_pure

replicate :: AsTerm t a => TTerm Int -> t -> TTerm [a]
replicate n x = primitive2 _lists_replicate n (asTerm x)

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
