module Hydra.Impl.Haskell.Dsl.Lib.Lists where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


concat :: Data ([a] -> a)
concat = Data $ Terms.primitive _lists_concat

head :: Data ([a] -> a)
head = Data $ Terms.primitive _lists_head

intercalate :: Data ([a] -> [a] -> [a])
intercalate = Data $ Terms.primitive _lists_intercalate

intersperse :: Data ([a] -> a -> [a])
intersperse = Data $ Terms.primitive _lists_intersperse

last :: Data ([a] -> a)
last = Data $ Terms.primitive _lists_last

length :: Data ([a] -> Int)
length = Data $ Terms.primitive _lists_length

--map :: Data ((a -> b) -> [a] -> [b])
--map = Data $ Terms.primitive _lists_map
