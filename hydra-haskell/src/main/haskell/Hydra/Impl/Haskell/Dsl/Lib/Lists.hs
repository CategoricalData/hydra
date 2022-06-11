module Hydra.Impl.Haskell.Dsl.Lib.Lists where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


concat :: Trm ([a] -> a)
concat = Trm $ Terms.primitive _lists_concat

head :: Trm ([a] -> a)
head = Trm $ Terms.primitive _lists_head

intercalate :: Trm ([a] -> [a] -> [a])
intercalate = Trm $ Terms.primitive _lists_intercalate

intersperse :: Trm ([a] -> a -> [a])
intersperse = Trm $ Terms.primitive _lists_intersperse

last :: Trm ([a] -> a)
last = Trm $ Terms.primitive _lists_last

length :: Trm ([a] -> Int)
length = Trm $ Terms.primitive _lists_length

--map :: Trm ((a -> b) -> [a] -> [b])
--map = Trm $ Terms.primitive _lists_map
