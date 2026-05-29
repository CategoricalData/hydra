-- | Primitive declarations for the hydra.lib.math namespace.

module Hydra.Sources.Kernel.Lib.Math where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Logic    as Logic
import qualified Hydra.Dsl.Meta.Lib.Math     as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes   as Maybes
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                    as I


ns :: ModuleName
ns = ModuleName "hydra.lib.math"

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModuleName ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.math namespace."}
  where
    definitions = [
      primNoDef "abs"       "The absolute value of an integer." int32To Nothing,
      primNoDef "acos"      "The arc cosine of a floating-point number." f64To Nothing,
      primNoDef "acosh"     "The hyperbolic arc cosine of a floating-point number." f64To Nothing,
      primNoDef "add"       "Integer addition." int32To2 Nothing,
      primNoDef "addFloat64" "Floating-point addition." f64To2 Nothing,
      primNoDef "asin"      "The arc sine of a floating-point number." f64To Nothing,
      primNoDef "asinh"     "The hyperbolic arc sine of a floating-point number." f64To Nothing,
      primNoDef "atan"      "The arc tangent of a floating-point number." f64To Nothing,
      primNoDef "atan2"     "The two-argument arc tangent (atan2)." f64To2 Nothing,
      primNoDef "atanh"     "The hyperbolic arc tangent of a floating-point number." f64To Nothing,
      primNoDef "ceiling"   "The smallest integer greater than or equal to the argument, as a float." f64To Nothing,
      primNoDef "cos"       "The cosine of a floating-point number." f64To Nothing,
      primNoDef "cosh"      "The hyperbolic cosine of a floating-point number." f64To Nothing,
      primNoDef "e"         "Euler's constant (the base of the natural logarithm)." f64Const Nothing,
      toPrimitive "Test whether an integer is even." int32ToBool Nothing even_,
      primNoDef "exp"       "The exponential function." f64To Nothing,
      primNoDef "floor"     "The largest integer less than or equal to the argument, as a float." f64To Nothing,
      primNoDef "log"       "The natural logarithm." f64To Nothing,
      primNoDef "logBase"   "Logarithm of the second argument in the base of the first." f64To2 Nothing,
      primNoDef "max"       "The maximum of two integers." int32To2 Nothing,
      primNoDef "maybeDiv"  "Integer division, or Nothing if dividing by zero." int32To2Maybe Nothing,
      primNoDef "maybeMod"  "Integer modulus, or Nothing if dividing by zero." int32To2Maybe Nothing,
      primNoDef "maybePred" "The predecessor of an integer, or Nothing on underflow." int32ToMaybe Nothing,
      primNoDef "maybeRem"  "Integer remainder, or Nothing if dividing by zero." int32To2Maybe Nothing,
      primNoDef "maybeSucc" "The successor of an integer, or Nothing on overflow." int32ToMaybe Nothing,
      primNoDef "min"       "The minimum of two integers." int32To2 Nothing,
      primNoDef "mul"       "Integer multiplication." int32To2 Nothing,
      primNoDef "mulFloat64" "Floating-point multiplication." f64To2 Nothing,
      primNoDef "negate"    "Negate an integer." int32To Nothing,
      primNoDef "negateFloat64" "Negate a floating-point number." f64To Nothing,
      toPrimitive "Test whether an integer is odd." int32ToBool Nothing odd_,
      primNoDef "pi"        "The mathematical constant pi." f64Const Nothing,
      primNoDef "pow"       "Raise the first argument to the power of the second." f64To2 Nothing,
      primNoDef "range"     "Construct the inclusive integer range from the first to the second argument." int32To2List Nothing,
      primNoDef "round"     "Round a floating-point number to the nearest integer-valued float." f64To Nothing,
      primNoDef "roundFloat32" "Round a Float32 to the given number of decimal places." int32F32ToF32 Nothing,
      primNoDef "roundFloat64" "Round a Float64 to the given number of decimal places." int32F64ToF64 Nothing,
      primNoDef "signum"    "Return the sign of an integer as -1, 0, or 1." int32To Nothing,
      primNoDef "sin"       "The sine of a floating-point number." f64To Nothing,
      primNoDef "sinh"      "The hyperbolic sine of a floating-point number." f64To Nothing,
      primNoDef "sqrt"      "The non-negative square root of a floating-point number." f64To Nothing,
      primNoDef "sub"       "Integer subtraction." int32To2 Nothing,
      primNoDef "subFloat64" "Floating-point subtraction." f64To2 Nothing,
      primNoDef "tan"       "The tangent of a floating-point number." f64To Nothing,
      primNoDef "tanh"      "The hyperbolic tangent of a floating-point number." f64To Nothing,
      primNoDef "truncate"  "Truncate a floating-point number toward zero, as a float." f64To Nothing]

primNoDef :: String -> String -> TermSignature -> Maybe String -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Shared monomorphic signatures.

f64Const :: TermSignature
f64Const = sig $ TypeScheme [] Types.float64 Nothing

f64To :: TermSignature
f64To = sig $ TypeScheme [] (Types.float64 Types.~> Types.float64) Nothing

f64To2 :: TermSignature
f64To2 = sig $ TypeScheme [] (Types.float64 Types.~> Types.float64 Types.~> Types.float64) Nothing

int32F32ToF32 :: TermSignature
int32F32ToF32 = sig $ TypeScheme [] (Types.int32 Types.~> Types.float32 Types.~> Types.float32) Nothing

int32F64ToF64 :: TermSignature
int32F64ToF64 = sig $ TypeScheme [] (Types.int32 Types.~> Types.float64 Types.~> Types.float64) Nothing

int32To :: TermSignature
int32To = sig $ TypeScheme [] (Types.int32 Types.~> Types.int32) Nothing

int32To2 :: TermSignature
int32To2 = sig $ TypeScheme [] (Types.int32 Types.~> Types.int32 Types.~> Types.int32) Nothing

int32To2List :: TermSignature
int32To2List = sig $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.list Types.int32) Nothing

int32To2Maybe :: TermSignature
int32To2Maybe = sig $ TypeScheme []
  (Types.int32 Types.~> Types.int32 Types.~> Types.optional Types.int32) Nothing

int32ToBool :: TermSignature
int32ToBool = sig $ TypeScheme [] (Types.int32 Types.~> Types.boolean) Nothing

int32ToMaybe :: TermSignature
int32ToMaybe = sig $ TypeScheme [] (Types.int32 Types.~> Types.optional Types.int32) Nothing

-- Default implementations.

-- even x = equal (fromMaybe 0 (maybeMod x 2)) 0
even_ :: TTermDefinition (I.Int32 -> Bool)
even_ = define "even" $
  doc "Test whether an integer is even, defined via maybeMod and equality." $
  "x" ~> Equality.equal
    (Maybes.fromMaybe (int32 0) (Math.maybeMod (var "x") (int32 2)))
    (int32 0)

-- odd x = not (even x)
odd_ :: TTermDefinition (I.Int32 -> Bool)
odd_ = define "odd" $
  doc "Test whether an integer is odd, defined as the negation of even." $
  "x" ~> Logic.not (Math.even (var "x"))
