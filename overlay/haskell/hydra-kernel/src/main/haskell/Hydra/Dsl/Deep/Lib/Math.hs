-- | Deep term DSL for the hydra.lib.math library.
--
-- These helpers construct Term-valued representations of math primitive
-- applications (TypedTerm Term), as opposed to the shallow DSL in
-- Hydra.Dsl.Meta.Lib.Math which works at the Haskell value level (TypedTerm Double, etc.).

module Hydra.Dsl.Deep.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Dsl.Meta.Core as Core
import Hydra.Dsl.Meta.Phantoms (encodedName)
import qualified Hydra.Dsl.Prims as Prims
import qualified Hydra.Lib.Math as DefMath


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
abs = apply1 (Prims.primName DefMath.abs)

acos :: TypedTerm Term -> TypedTerm Term
acos = apply1 (Prims.primName DefMath.acos)

acosh :: TypedTerm Term -> TypedTerm Term
acosh = apply1 (Prims.primName DefMath.acosh)

asin :: TypedTerm Term -> TypedTerm Term
asin = apply1 (Prims.primName DefMath.asin)

asinh :: TypedTerm Term -> TypedTerm Term
asinh = apply1 (Prims.primName DefMath.asinh)

atan :: TypedTerm Term -> TypedTerm Term
atan = apply1 (Prims.primName DefMath.atan)

atanh :: TypedTerm Term -> TypedTerm Term
atanh = apply1 (Prims.primName DefMath.atanh)

ceiling :: TypedTerm Term -> TypedTerm Term
ceiling = apply1 (Prims.primName DefMath.ceiling)

cos :: TypedTerm Term -> TypedTerm Term
cos = apply1 (Prims.primName DefMath.cos)

cosh :: TypedTerm Term -> TypedTerm Term
cosh = apply1 (Prims.primName DefMath.cosh)

exp :: TypedTerm Term -> TypedTerm Term
exp = apply1 (Prims.primName DefMath.exp)

floor :: TypedTerm Term -> TypedTerm Term
floor = apply1 (Prims.primName DefMath.floor)

log :: TypedTerm Term -> TypedTerm Term
log = apply1 (Prims.primName DefMath.log)

negate :: TypedTerm Term -> TypedTerm Term
negate = apply1 (Prims.primName DefMath.negate)

negateFloat64 :: TypedTerm Term -> TypedTerm Term
negateFloat64 = apply1 (Prims.primName DefMath.negateFloat64)

round :: TypedTerm Term -> TypedTerm Term
round = apply1 (Prims.primName DefMath.round)

signum :: TypedTerm Term -> TypedTerm Term
signum = apply1 (Prims.primName DefMath.signum)

sin :: TypedTerm Term -> TypedTerm Term
sin = apply1 (Prims.primName DefMath.sin)

sinh :: TypedTerm Term -> TypedTerm Term
sinh = apply1 (Prims.primName DefMath.sinh)

sqrt :: TypedTerm Term -> TypedTerm Term
sqrt = apply1 (Prims.primName DefMath.sqrt)

tan :: TypedTerm Term -> TypedTerm Term
tan = apply1 (Prims.primName DefMath.tan)

tanh :: TypedTerm Term -> TypedTerm Term
tanh = apply1 (Prims.primName DefMath.tanh)

truncate :: TypedTerm Term -> TypedTerm Term
truncate = apply1 (Prims.primName DefMath.truncate)


-- Binary functions

add :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
add = apply2 (Prims.primName DefMath.add)

addFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
addFloat64 = apply2 (Prims.primName DefMath.addFloat64)

atan2 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
atan2 = apply2 (Prims.primName DefMath.atan2)

logBase :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
logBase = apply2 (Prims.primName DefMath.logBase)

mul :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
mul = apply2 (Prims.primName DefMath.mul)

mulFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
mulFloat64 = apply2 (Prims.primName DefMath.mulFloat64)

pow :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
pow = apply2 (Prims.primName DefMath.pow)

sub :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
sub = apply2 (Prims.primName DefMath.sub)

subFloat64 :: TypedTerm Term -> TypedTerm Term -> TypedTerm Term
subFloat64 = apply2 (Prims.primName DefMath.subFloat64)
