module Hydra.Impl.Haskell.Dsl.Lib.Math where

import Hydra.Impl.Haskell.Dsl.Phantoms
import qualified Hydra.Impl.Haskell.Dsl.Terms as Terms
import Hydra.Impl.Haskell.Sources.Libraries


add :: Trm (Int -> Int -> Int)
add = Trm $ Terms.primitive _math_add

div :: Trm (Int -> Int -> Int)
div = Trm $ Terms.primitive _math_div

mod :: Trm (Int -> Int -> Int)
mod = Trm $ Terms.primitive _math_mod

mul :: Trm (Int -> Int -> Int)
mul = Trm $ Terms.primitive _math_mul

neg :: Trm (Int -> Int)
neg = Trm $ Terms.primitive _math_neg

rem :: Trm (Int -> Int -> Int)
rem = Trm $ Terms.primitive _math_rem

sub :: Trm (Int -> Int -> Int)
sub = Trm $ Terms.primitive _math_sub
