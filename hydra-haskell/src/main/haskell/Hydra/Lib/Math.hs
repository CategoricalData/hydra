-- | Haskell implementations of hydra.lib.math primitives

module Hydra.Lib.Math where

import Prelude (Num, Ord, Integral, Enum, Bool, Double, Int, Integer, Float, (.), ($), (+), (-), (*))
import qualified Prelude


abs :: Num a => a -> a
abs = Prelude.abs

acos :: Double -> Double
acos = Prelude.acos

acosh :: Double -> Double
acosh = Prelude.acosh

asin :: Double -> Double
asin = Prelude.asin

asinh :: Double -> Double
asinh = Prelude.asinh

atan :: Double -> Double
atan = Prelude.atan

atan2 :: Double -> Double -> Double
atan2 = Prelude.atan2

atanh :: Double -> Double
atanh = Prelude.atanh

ceiling :: Double -> Integer
ceiling = Prelude.ceiling

cos :: Double -> Double
cos = Prelude.cos

cosh :: Double -> Double
cosh = Prelude.cosh

add :: Num a => a -> a -> a
add x y = x + y

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
div :: Integral a => a -> a -> a
div = Prelude.div

e :: Double
e = Prelude.exp 1.0

even :: Integral a => a -> Bool
even = Prelude.even

exp :: Double -> Double
exp = Prelude.exp

floor :: Double -> Integer
floor = Prelude.floor

log :: Double -> Double
log = Prelude.log

logBase :: Double -> Double -> Double
logBase = Prelude.logBase

max :: Ord a => a -> a -> a
max = Prelude.max

min :: Ord a => a -> a -> a
min = Prelude.min

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
mod :: Integral a => a -> a -> a
mod = Prelude.mod

mul :: Num a => a -> a -> a
mul x y = x * y

negate :: Num a => a -> a
negate = Prelude.negate

odd :: Integral a => a -> Bool
odd = Prelude.odd

pi :: Double
pi = Prelude.pi

pow :: Double -> Double -> Double
pow = (Prelude.**)

pred :: Enum a => a -> a
pred = Prelude.pred

range :: Enum a => a -> a -> [a]
range start end = [start .. end]

-- TODO: partial function. See https://github.com/CategoricalData/hydra/issues/201
rem :: Integral a => a -> a -> a
rem = Prelude.rem

round :: Double -> Integer
round = Prelude.round

signum :: Num a => a -> a
signum = Prelude.signum

sin :: Double -> Double
sin = Prelude.sin

sinh :: Double -> Double
sinh = Prelude.sinh

sqrt :: Double -> Double
sqrt = Prelude.sqrt

sub :: Num a => a -> a -> a
sub x y = x - y

succ :: Enum a => a -> a
succ = Prelude.succ

tan :: Double -> Double
tan = Prelude.tan

tanh :: Double -> Double
tanh = Prelude.tanh

truncate :: Double -> Integer
truncate = Prelude.truncate