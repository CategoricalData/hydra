module Hydra.Dsl.Lib.Lists where

import Hydra.Phantoms
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Terms as Terms


apply :: TTerm ([a -> b] -> [a] -> [b])
apply = TTerm $ Terms.primitive _lists_apply

at :: TTerm (Int -> [a] -> a)
at = TTerm $ Terms.primitive _lists_at

bind :: TTerm ([a] -> (a -> [b]) -> [b])
bind = TTerm $ Terms.primitive _lists_bind

concat :: TTerm ([[a]] -> [a])
concat = TTerm $ Terms.primitive _lists_concat

concat2 :: TTerm ([a] -> [a] -> [a])
concat2 = TTerm $ Terms.primitive _lists_concat2

cons :: TTerm (a -> [a] -> [a])
cons = TTerm $ Terms.primitive _lists_cons

filter :: TTerm ((a -> Bool) -> [a] -> [a])
filter = TTerm $ Terms.primitive _lists_filter

foldl :: TTerm ((b -> a -> b) -> b -> [a] -> b)
foldl = TTerm $ Terms.primitive _lists_foldl

head :: TTerm ([a] -> a)
head = TTerm $ Terms.primitive _lists_head

intercalate :: TTerm ([a] -> [[a]] -> [a])
intercalate = TTerm $ Terms.primitive _lists_intercalate

intersperse :: TTerm ([a] -> a -> [a])
intersperse = TTerm $ Terms.primitive _lists_intersperse

last :: TTerm ([a] -> a)
last = TTerm $ Terms.primitive _lists_last

length :: TTerm ([a] -> Int)
length = TTerm $ Terms.primitive _lists_length

map :: TTerm ((a -> b) -> [a] -> [b])
map = TTerm $ Terms.primitive _lists_map

nub :: Eq a => TTerm ([a] -> [a])
nub = TTerm $ Terms.primitive _lists_nub

null :: TTerm ([a] -> Bool)
null = TTerm $ Terms.primitive _lists_null

pure :: TTerm (a -> [a])
pure = TTerm $ Terms.primitive _lists_pure

reverse :: TTerm ([a] -> [a])
reverse = TTerm $ Terms.primitive _lists_reverse

safeHead :: TTerm ([a] -> Maybe a)
safeHead = TTerm $ Terms.primitive _lists_safeHead

tail :: TTerm ([a] -> [a])
tail = TTerm $ Terms.primitive _lists_tail
