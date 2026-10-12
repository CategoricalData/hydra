module Hydra.Sources.Test.Lib.Functions where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Functions as DefFunctions
import qualified Hydra.Core.Lib.Math as DefMath
import qualified Hydra.Core.Lib.Ordering as DefOrdering


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.functions"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model", ModuleName "hydra.core.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.test.testGraph", ModuleName "hydra.core.testing", ModuleName "hydra.core.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.functions primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.core.lib.functions primitives

-- Note: hydra.core.lib.functions.absurd (void -> x) is deliberately untested in the common suite.
-- void is uninhabited: no well-typed term can construct a value of type void to pass as its
-- argument, so there is no valid input for a primCase to exercise. Its native implementations
-- are still shipped across every host, and its type-correctness is exercised indirectly by any
-- consumer that eliminates a void-containing type.
allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.functions primitives" $
    supergroup "hydra.core.lib.functions primitives" [
      functionsIdentity,
      functionsConst,
      functionsCompose,
      functionsFlip]

functionsIdentity :: TypedTerm TestGroup
functionsIdentity = supergroup "identity" [
  subgroup "returnsArgumentUnchanged" [
    test "integer" 42 42]]
  where
    test name x result = primCaseWithProvisions name ["returnsArgumentUnchanged"] DefFunctions.identity [int32 x] (int32 result)

-- const :: x -> y -> x (#749): had zero test coverage prior to this addition. Cases
-- confirm both argument positions independently, since a monomorphization bug could
-- pass one and fail the other (e.g. if the second, ignored argument's type leaked in).
functionsConst :: TypedTerm TestGroup
functionsConst = supergroup "const" [
  subgroup "returnsFirstArgumentIgnoringSecond" [
    test "returns first argument, ignoring an int second argument"
      (Terms.primitive DefFunctions.const @@ int32 5 @@ string "ignored") (int32 5),
    test "returns first argument, ignoring a string second argument"
      (Terms.primitive DefFunctions.const @@ string "a" @@ int32 99) (string "a")]]
  where
    test name input result = evalCaseWithProvisions name ["returnsFirstArgumentIgnoringSecond"] DefFunctions.const input result

-- compose :: (b -> c) -> (a -> b) -> a -> c, where compose(g, f, x) = g(f(x)) (#775): had zero
-- test coverage prior to this addition. Uses two distinct, order-sensitive unary primitives
-- (negate and an increment via math.add) so a swapped-argument bug (f and g transposed) would
-- produce a different, detectable result.
functionsCompose :: TypedTerm TestGroup
functionsCompose = subgroup "compose" [
  -- compose(negate, increment, 5) = negate(increment(5)) = negate(6) = -6
  test "compose(negate, increment, 5) = negate(increment(5))"
    (Terms.primitive DefFunctions.compose
      @@ Terms.primitive DefMath.negate
      @@ (Terms.lambda "x" (Terms.primitive DefMath.add @@ Terms.var "x" @@ int32 1))
      @@ int32 5)
    (int32 (-6))]
  where
    test name input result = evalCase name input result

-- flip :: (a -> b -> c) -> a -> b -> c, where flip(f, x, y) = f(y, x) (#749): had zero
-- test coverage prior to this addition. Uses ordering.gt (an existing, order-sensitive,
-- non-class-constrained binary primitive) as the function argument to prove flip
-- actually swaps argument order, rather than merely passing its two value arguments
-- through unchanged.
functionsFlip :: TypedTerm TestGroup
functionsFlip = supergroup "flip" [
  subgroup "swapsArgumentOrder" [
    -- flip(f, x, y) = f(y, x): flip(gt, 5, 3) = gt(3, 5) = false
    test "flip(gt, 5, 3) = gt(3, 5)"
      (Terms.primitive DefFunctions.flip @@ Terms.primitive DefOrdering.gt @@ int32 5 @@ int32 3) false,
    -- flip(gt, 3, 5) = gt(5, 3) = true
    test "flip(gt, 3, 5) = gt(5, 3)"
      (Terms.primitive DefFunctions.flip @@ Terms.primitive DefOrdering.gt @@ int32 3 @@ int32 5) true]]
  where
    test name input result = evalCaseWithProvisions name ["swapsArgumentOrder"] DefFunctions.flip input result
