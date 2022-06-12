module Hydra.Impl.Haskell.Dsl.Lib.Math where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


add :: Data (Int -> Int -> Int)
add = Data $ Terms.primitive _math_add

div :: Data (Int -> Int -> Int)
div = Data $ Terms.primitive _math_div

mod :: Data (Int -> Int -> Int)
mod = Data $ Terms.primitive _math_mod

mul :: Data (Int -> Int -> Int)
mul = Data $ Terms.primitive _math_mul

neg :: Data (Int -> Int)
neg = Data $ Terms.primitive _math_neg

rem :: Data (Int -> Int -> Int)
rem = Data $ Terms.primitive _math_rem

sub :: Data (Int -> Int -> Int)
sub = Data $ Terms.primitive _math_sub
