-- | Deep term DSL for the hydra.lib.math library.
--
-- These helpers construct Term-valued representations of math primitive
-- applications (TTerm Term), as opposed to the shallow DSL in
-- Hydra.Dsl.Meta.Lib.Math which works at the Haskell value level (TTerm Double, etc.).

module Hydra.Dsl.Deep.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Phantoms (encodedName)
import Hydra.Sources.Libraries


-- | Apply a unary math primitive to a deep term
apply1 :: Name -> TTerm Term -> TTerm Term
apply1 name arg = Core.termApplication $ Core.application (Core.termVariable $ encodedName name) arg

-- | Apply a binary math primitive to two deep terms
apply2 :: Name -> TTerm Term -> TTerm Term -> TTerm Term
apply2 name a b = Core.termApplication $ Core.application
  (Core.termApplication $ Core.application (Core.termVariable $ encodedName name) a)
  b

-- | Reference a math primitive as a deep term (for use as a first-class value)
ref :: Name -> TTerm Term
ref name = Core.termVariable $ encodedName name


-- Unary functions

abs :: TTerm Term -> TTerm Term
abs = apply1 _math_abs

acos :: TTerm Term -> TTerm Term
acos = apply1 _math_acos

acosh :: TTerm Term -> TTerm Term
acosh = apply1 _math_acosh

asin :: TTerm Term -> TTerm Term
asin = apply1 _math_asin

asinh :: TTerm Term -> TTerm Term
asinh = apply1 _math_asinh

atan :: TTerm Term -> TTerm Term
atan = apply1 _math_atan

atanh :: TTerm Term -> TTerm Term
atanh = apply1 _math_atanh

ceiling :: TTerm Term -> TTerm Term
ceiling = apply1 _math_ceiling

cos :: TTerm Term -> TTerm Term
cos = apply1 _math_cos

cosh :: TTerm Term -> TTerm Term
cosh = apply1 _math_cosh

exp :: TTerm Term -> TTerm Term
exp = apply1 _math_exp

floor :: TTerm Term -> TTerm Term
floor = apply1 _math_floor

log :: TTerm Term -> TTerm Term
log = apply1 _math_log

negate :: TTerm Term -> TTerm Term
negate = apply1 _math_negate

negateFloat64 :: TTerm Term -> TTerm Term
negateFloat64 = apply1 _math_negateFloat64

round :: TTerm Term -> TTerm Term
round = apply1 _math_round

signum :: TTerm Term -> TTerm Term
signum = apply1 _math_signum

sin :: TTerm Term -> TTerm Term
sin = apply1 _math_sin

sinh :: TTerm Term -> TTerm Term
sinh = apply1 _math_sinh

sqrt :: TTerm Term -> TTerm Term
sqrt = apply1 _math_sqrt

tan :: TTerm Term -> TTerm Term
tan = apply1 _math_tan

tanh :: TTerm Term -> TTerm Term
tanh = apply1 _math_tanh

truncate :: TTerm Term -> TTerm Term
truncate = apply1 _math_truncate


-- Binary functions

add :: TTerm Term -> TTerm Term -> TTerm Term
add = apply2 _math_add

addFloat64 :: TTerm Term -> TTerm Term -> TTerm Term
addFloat64 = apply2 _math_addFloat64

atan2 :: TTerm Term -> TTerm Term -> TTerm Term
atan2 = apply2 _math_atan2

logBase :: TTerm Term -> TTerm Term -> TTerm Term
logBase = apply2 _math_logBase

mul :: TTerm Term -> TTerm Term -> TTerm Term
mul = apply2 _math_mul

mulFloat64 :: TTerm Term -> TTerm Term -> TTerm Term
mulFloat64 = apply2 _math_mulFloat64

pow :: TTerm Term -> TTerm Term -> TTerm Term
pow = apply2 _math_pow

sub :: TTerm Term -> TTerm Term -> TTerm Term
sub = apply2 _math_sub

subFloat64 :: TTerm Term -> TTerm Term -> TTerm Term
subFloat64 = apply2 _math_subFloat64
