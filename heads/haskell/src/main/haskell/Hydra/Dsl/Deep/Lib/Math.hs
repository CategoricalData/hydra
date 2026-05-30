-- | Deep term DSL for the hydra.lib.math library.
--
-- These helpers construct Term-valued representations of math primitive
-- applications (TypedTerm Term), as opposed to the shallow DSL in
-- Hydra.Dsl.Meta.Lib.Math which works at the Haskell value level (TypedTerm Double, etc.).

module Hydra.Dsl.Deep.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Phantoms (encodedName)
import Hydra.Sources.Libraries


-- | Apply a unary math primitive to a deep term
apply1 :: Name -> TypedTerm Term -> TypedTerm Term
apply1 name arg = Core.termApplication $ Core.application (Core.termVariable $ encodedName name) arg

-- | Apply a binary math primitive to two deep terms
apply2 :: Name -> TypedTerm Term -> TypedTerm Term -> TypedTerm Term
apply2 name a b = Core.termApplication $ Core.application
  (Core.termApplication $ Core.application (Core.termVariable $ encodedName name) a)
  b

-- | Reference a math primitive as a deep term (for use as a first-class value)
ref :: Name -> TypedTerm Term
ref name = Core.termVariable $ encodedName name


-- Unary functions

abs :: TypedTerm Term -> TypedTerm Term
abs = apply1 _math_abs

acos :: TypedTerm Term -> TypedTerm Term
acos = apply1 _math_acos

acosh :: TypedTerm Term -> TypedTerm Term
acosh = apply1 _math_acosh

asin :: TypedTerm Term -> TypedTerm Term
asin = apply1 _math_asin

asinh :: TypedTerm Term -> TypedTerm Term
asinh = apply1 _math_asinh

atan :: TypedTerm Term -> TypedTerm Term
atan = apply1 _math_atan

atanh :: TypedTerm Term -> TypedTerm Term
atanh = apply1 _math_atanh

ceiling :: TypedTerm Term -> TypedTerm Term
ceiling = apply1 _math_ceiling

cos :: TypedTerm Term -> TypedTerm Term
cos = apply1 _math_cos

cosh :: TypedTerm Term -> TypedTerm Term
cosh = apply1 _math_cosh

exp :: TypedTerm Term -> TypedTerm Term
exp = apply1 _math_exp

floor :: TypedTerm Term -> TypedTerm Term
floor = apply1 _math_floor

log :: TypedTerm Term -> TypedTerm Term
log = apply1 _math_log

negate :: TypedTerm Term -> TypedTerm Term
negate = apply1 _math_negate

negateFloat64 :: TypedTerm Term -> TypedTerm Term
negateFloat64 = apply1 _math_negateFloat64

round :: TypedTerm Term -> TypedTerm Term
round = apply1 _math_round

signum :: TypedTerm Term -> TypedTerm Term
signum = apply1 _math_signum

sin :: TypedTerm Term -> TypedTerm Term
sin = apply1 _math_sin

sinh :: TypedTerm Term -> TypedTerm Term
sinh = apply1 _math_sinh

sqrt :: TypedTerm Term -> TypedTerm Term
sqrt = apply1 _math_sqrt

tan :: TypedTerm Term -> TypedTerm Term
tan = apply1 _math_tan

tanh :: TypedTerm Term -> TypedTerm Term
tanh = apply1 _math_tanh

truncate :: TypedTerm Term -> TypedTerm Term
truncate = apply1 _math_truncate


-- Binary functions

add :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
add = apply2 _math_add

addFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
addFloat64 = apply2 _math_addFloat64

atan2 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
atan2 = apply2 _math_atan2

logBase :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
logBase = apply2 _math_logBase

mul :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
mul = apply2 _math_mul

mulFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
mulFloat64 = apply2 _math_mulFloat64

pow :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
pow = apply2 _math_pow

sub :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
sub = apply2 _math_sub

subFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
subFloat64 = apply2 _math_subFloat64
