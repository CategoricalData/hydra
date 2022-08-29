module Hydra.Impl.Haskell.Dsl.Lib.Lists where

import Hydra.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


concat :: Datum ([a] -> a)
concat = Datum $ Terms.primitive _lists_concat

head :: Datum ([a] -> a)
head = Datum $ Terms.primitive _lists_head

intercalate :: Datum ([a] -> [a] -> [a])
intercalate = Datum $ Terms.primitive _lists_intercalate

intersperse :: Datum ([a] -> a -> [a])
intersperse = Datum $ Terms.primitive _lists_intersperse

last :: Datum ([a] -> a)
last = Datum $ Terms.primitive _lists_last

length :: Datum ([a] -> Int)
length = Datum $ Terms.primitive _lists_length

--map :: Datum ((a -> b) -> [a] -> [b])
--map = Datum $ Terms.primitive _lists_map
