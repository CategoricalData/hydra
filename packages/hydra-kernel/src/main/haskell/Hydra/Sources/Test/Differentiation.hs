-- | Test cases for automatic differentiation
module Hydra.Sources.Test.Differentiation where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import           Hydra.Dsl.Meta.Phantoms      as Phantoms hiding ((++))
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Sources.Kernel.Terms.Differentiation as Diff
import qualified Hydra.Sources.Kernel.Terms.Variables as Variables
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore
import qualified Hydra.Dsl.Meta.Lib.Eithers as Eithers
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Strings  as Strings
import qualified Hydra.Dsl.Meta.Terms         as Terms
import qualified Hydra.Lib.Math              as Math


ns :: Namespace
ns = Namespace "hydra.test.differentiation"

module_ :: Module
module_ = Module {
            moduleNamespace = ns,
            moduleDefinitions = definitions,
            moduleTermDependencies = [Diff.ns, Variables.ns, ShowCore.ns],
            moduleTypeDependencies = kernelTypesNamespaces,
            moduleDescription = (Just "Test cases for automatic differentiation")}
  where
    definitions = [Phantoms.toDefinition allTests]

define :: String -> TTerm a -> TTermDefinition a
define = definitionInModule module_

allTests :: TTermDefinition TestGroup
allTests = define "allTests" $
    doc "Test cases for automatic differentiation" $
    supergroup "differentiation" [
      basicDiffGroup,
      primitiveDiffGroup,
      binaryDiffGroup,
      chainRuleGroup,
      structuralDiffGroup,
      evalDiffGroup,
      gradientCheckGroup,
      gradientGroup]

-- ============================================================
-- Deep/meta-level helpers (for structural tests that compare term shapes)
-- ============================================================

showTerm :: TTerm Term -> TTerm String
showTerm t = ShowCore.term @@ t

mkName :: String -> TTerm Name
mkName s = Core.name (string s)

mkVar :: String -> TTerm Term
mkVar s = Core.termVariable $ mkName s

f64 :: Double -> TTerm Term
f64 d = Core.termLiteral $ Core.literalFloat $ Core.floatValueFloat64 (float64 d)

mkLambda :: String -> TTerm Term -> TTerm Term
mkLambda param body = Core.termLambda $ Core.lambda (mkName param) nothing body

-- Deep-level primitive application (for structural test expectations)
prim1 :: Name -> TTerm Term -> TTerm Term
prim1 name arg = Core.termApplication $ Core.application (Core.termVariable $ encodedName name) arg

prim2 :: Name -> TTerm Term -> TTerm Term -> TTerm Term
prim2 name a b = Core.termApplication $ Core.application
  (Core.termApplication $ Core.application (Core.termVariable $ encodedName name) a)
  b

diffCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
diffCase cname input expected =
  universalCase cname
    (showTerm (Diff.differentiateTerm @@ mkName "x" @@ input))
    (showTerm expected)

diffFnCase :: String -> TTerm Term -> TTerm Term -> TTerm TestCaseWithMetadata
diffFnCase cname input expected =
  universalCase cname
    (showTerm (Diff.differentiateFunction @@ input))
    (showTerm expected)

-- ============================================================
-- Shallow-level helpers (for eval tests that need reduceTerm)
-- ============================================================

-- Coerce phantom types
retype :: TTerm a -> TTerm b
retype (TTerm t) = TTerm t

-- Evaluate a derivative: differentiate, substitute x=xVal, reduce
evalDiffCase :: String -> TTerm Term -> Double -> Double -> TTerm TestCaseWithMetadata
evalDiffCase cname expr xVal expected = roundedEvalDiffCase cname expr xVal expected

-- Rounded eval case: round both sides to 12 significant digits for cross-platform portability
roundedEvalDiffCase :: String -> TTerm Term -> Double -> Double -> TTerm TestCaseWithMetadata
roundedEvalDiffCase cname expr xVal expected = evalCase cname input output
  where
    digits = 12
    deriv = Diff.differentiateTerm @@ mkName "x" @@ expr
    substituted = Variables.replaceFreeTermVariable @@ mkName "x" @@ f64 xVal @@ deriv
    input = prim2 _math_roundFloat64
      (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 (int32 digits))
      (retype substituted)
    output = Terms.float64 (Math.roundFloat64 digits expected)

-- ============================================================
-- Structural tests
-- ============================================================

basicDiffGroup :: TTerm TestGroup
basicDiffGroup = subgroup "basic rules" [
    diffCase "variable wrt itself"
      (mkVar "x")
      (f64 1.0),

    diffCase "variable wrt different variable"
      (mkVar "y")
      (f64 0.0),

    diffCase "float64 literal"
      (f64 42.0)
      (f64 0.0),

    diffCase "zero literal"
      (f64 0.0)
      (f64 0.0),

    diffFnCase "identity function"
      (mkLambda "x" (mkVar "x"))
      (mkLambda "x" (f64 1.0)),

    diffFnCase "constant function"
      (mkLambda "x" (f64 5.0))
      (mkLambda "x" (f64 0.0)),

    diffCase "lambda binding different variable"
      (mkLambda "y" (mkVar "x"))
      (mkLambda "y" (f64 1.0)),

    diffCase "lambda shadowing differentiation variable"
      (mkLambda "x" (mkVar "x"))
      (mkLambda "x" (f64 0.0))]

primitiveDiffGroup :: TTerm TestGroup
primitiveDiffGroup = subgroup "unary primitives" [
    diffCase "sin of variable"
      (prim1 _math_sin (mkVar "x"))
      (prim2 _math_mulFloat64
        (Core.termApplication $ Core.application (Core.termVariable $ encodedName _math_cos) (mkVar "x"))
        (f64 1.0)),

    diffCase "exp of variable"
      (prim1 _math_exp (mkVar "x"))
      (prim2 _math_mulFloat64
        (Core.termApplication $ Core.application (Core.termVariable $ encodedName _math_exp) (mkVar "x"))
        (f64 1.0)),

    diffCase "sin of different variable"
      (prim1 _math_sin (mkVar "y"))
      (prim2 _math_mulFloat64
        (Core.termApplication $ Core.application (Core.termVariable $ encodedName _math_cos) (mkVar "y"))
        (f64 0.0))]

-- Binary tests: input uses Int32 names (add/mul/sub), output uses Float64 names (addFloat64 etc.)
binaryDiffGroup :: TTerm TestGroup
binaryDiffGroup = subgroup "binary primitives" [
    diffCase "add variable to itself"
      (prim2 _math_add (mkVar "x") (mkVar "x"))
      (prim2 _math_addFloat64 (f64 1.0) (f64 1.0)),

    diffCase "multiply variable by itself"
      (prim2 _math_mul (mkVar "x") (mkVar "x"))
      (prim2 _math_addFloat64
        (prim2 _math_mulFloat64 (mkVar "x") (f64 1.0))
        (prim2 _math_mulFloat64 (mkVar "x") (f64 1.0))),

    diffCase "subtract constant from variable"
      (prim2 _math_sub (mkVar "x") (f64 5.0))
      (prim2 _math_subFloat64 (f64 1.0) (f64 0.0))]

chainRuleGroup :: TTerm TestGroup
chainRuleGroup = subgroup "chain rule" [
    diffCase "nested sin(cos(x))"
      (prim1 _math_sin (prim1 _math_cos (mkVar "x")))
      (prim2 _math_mulFloat64
        (Core.termApplication $ Core.application
          (Core.termVariable $ encodedName _math_cos)
          (prim1 _math_cos (mkVar "x")))
        (prim2 _math_mulFloat64
          (Core.termApplication $ Core.application
            (mkLambda "_x" (prim1 _math_negateFloat64 (prim1 _math_sin (mkVar "_x"))))
            (mkVar "x"))
          (f64 1.0)))]

structuralDiffGroup :: TTerm TestGroup
structuralDiffGroup = subgroup "structural" [
    diffCase "list of terms"
      (Core.termList $ list [mkVar "x", f64 5.0])
      (Core.termList $ list [f64 1.0, f64 0.0]),

    diffCase "pair of terms"
      (Core.termPair $ pair (mkVar "x") (f64 5.0))
      (Core.termPair $ pair (f64 1.0) (f64 0.0)),

    diffCase "unit term"
      Core.termUnit
      (f64 0.0)]

-- ============================================================
-- Evaluation tests (shallow level — terms actually get reduced)
-- ============================================================

roundDigits :: Int
roundDigits = 10

evalDiffGroup :: TTerm TestGroup
evalDiffGroup = subgroup "evaluate derivatives" [
    evalDiffCase "d/dx(x) at 3.0"
      (mkVar "x")
      3.0 1.0,

    evalDiffCase "d/dx(42.0) at 3.0"
      (f64 42.0)
      3.0 0.0,

    evalDiffCase "d/dx(x+x) at 3.0"
      (prim2 _math_add (mkVar "x") (mkVar "x"))
      3.0 2.0,

    evalDiffCase "d/dx(x*x) at 3.0"
      (prim2 _math_mul (mkVar "x") (mkVar "x"))
      3.0 6.0,

    evalDiffCase "d/dx(x*x) at 5.0"
      (prim2 _math_mul (mkVar "x") (mkVar "x"))
      5.0 10.0,

    evalDiffCase "d/dx(x-5) at 7.0"
      (prim2 _math_sub (mkVar "x") (f64 5.0))
      7.0 1.0,

    evalDiffCase "d/dx(sin(x)) at 0.0"
      (prim1 _math_sin (mkVar "x"))
      0.0 1.0,

    evalDiffCase "d/dx(sin(x)) at pi/2"
      (prim1 _math_sin (mkVar "x"))
      (pi / 2) (cos (pi / 2)),

    evalDiffCase "d/dx(cos(x)) at 1.0"
      (prim1 _math_cos (mkVar "x"))
      1.0 (-(sin 1.0)),

    evalDiffCase "d/dx(cos(x)) at pi"
      (prim1 _math_cos (mkVar "x"))
      pi (-(sin pi)),

    evalDiffCase "d/dx(exp(x)) at 0.0"
      (prim1 _math_exp (mkVar "x"))
      0.0 1.0,

    evalDiffCase "d/dx(exp(x)) at 1.0"
      (prim1 _math_exp (mkVar "x"))
      1.0 (exp 1.0),

    evalDiffCase "d/dx(log(x)) at 1.0"
      (prim1 _math_log (mkVar "x"))
      1.0 1.0,

    evalDiffCase "d/dx(log(x)) at e"
      (prim1 _math_log (mkVar "x"))
      (exp 1.0) (1.0 / exp 1.0),

    evalDiffCase "d/dx(sqrt(x)) at 4.0"
      (prim1 _math_sqrt (mkVar "x"))
      4.0 0.25,

    evalDiffCase "d/dx(x^3) at 2.0"
      (prim2 _math_pow (mkVar "x") (f64 3.0))
      2.0 12.0,

    evalDiffCase "d/dx(sin(cos(x))) at 0.5"
      (prim1 _math_sin (prim1 _math_cos (mkVar "x")))
      0.5 (cos (cos 0.5) * (-(sin 0.5))),

    evalDiffCase "d/dx(sin(cos(x))) at 1.0"
      (prim1 _math_sin (prim1 _math_cos (mkVar "x")))
      1.0 (cos (cos 1.0) * (-(sin 1.0))),

    evalDiffCase "d/dx(exp(sin(x))) at 0.0"
      (prim1 _math_exp (prim1 _math_sin (mkVar "x")))
      0.0 1.0,

    evalDiffCase "d/dx(x*sin(x)) at pi"
      (prim2 _math_mul (mkVar "x") (prim1 _math_sin (mkVar "x")))
      pi (sin pi + pi * cos pi)]

-- ============================================================
-- Gradient check: AD vs finite differences
-- ============================================================

gradientCheckGroup :: TTerm TestGroup
gradientCheckGroup = subgroup "gradient check" [
    gradCheck "x^2" (\x -> x * x) (prim2 _math_mul (mkVar "x") (mkVar "x")) 2.0,
    gradCheck "sin(x)" sin (prim1 _math_sin (mkVar "x")) 1.0,
    gradCheck "exp(x)" exp (prim1 _math_exp (mkVar "x")) 0.5,
    gradCheck "x^3" (\x -> x ** 3) (prim2 _math_pow (mkVar "x") (f64 3.0)) 1.5,
    gradCheck "log(x)" log (prim1 _math_log (mkVar "x")) 2.0,
    gradCheck "cos(x)" cos (prim1 _math_cos (mkVar "x")) 1.0,
    gradCheck "sin(cos(x))" (\x -> sin (cos x)) (prim1 _math_sin (prim1 _math_cos (mkVar "x"))) 1.0,
    gradCheck "x*sin(x)" (\x -> x * sin x) (prim2 _math_mul (mkVar "x") (prim1 _math_sin (mkVar "x"))) 2.0]
  where
    h :: Double
    h = 1.0e-6

    gradDigits :: Int
    gradDigits = 5

    gradCheck :: String -> (Double -> Double) -> TTerm Term -> Double -> TTerm TestCaseWithMetadata
    gradCheck cname hsFn expr xVal = evalCase cname adResult fdResult
      where
        deriv = Diff.differentiateTerm @@ mkName "x" @@ expr
        adSubst = Variables.replaceFreeTermVariable @@ mkName "x" @@ f64 xVal @@ deriv
        -- Round the AD result for comparison with finite differences
        adResult = prim2 _math_roundFloat64
          (Core.termLiteral $ Core.literalInteger $ Core.integerValueInt32 (int32 gradDigits))
          (retype adSubst)

        fdResult = Terms.float64 (Math.roundFloat64 gradDigits fdVal)
        fdVal = (hsFn (xVal + h) - hsFn (xVal - h)) / (2.0 * h)

-- ============================================================
-- Gradient tests (multi-variable partial derivatives)
-- ============================================================

gradCase :: String -> TTerm Term -> [String] -> TTerm Term -> TTerm TestCaseWithMetadata
gradCase cname input vars expected =
  universalCase cname
    (showTerm (Diff.gradient @@ mkName "Gradient" @@ list (fmap mkName vars) @@ input))
    (showTerm expected)

mkRecord :: String -> [(String, TTerm Term)] -> TTerm Term
mkRecord tname fields = Core.termRecord $ Core.record
  (mkName tname)
  (list [Core.field (mkName fname) val | (fname, val) <- fields])

gradientGroup :: TTerm TestGroup
gradientGroup = subgroup "gradient" [
    -- gradient of x+y w.r.t. [x, y] => {x: 1+0, y: 0+1}
    gradCase "add two variables"
      (prim2 _math_addFloat64 (mkVar "x") (mkVar "y"))
      ["x", "y"]
      (mkRecord "Gradient"
        [ ("x", prim2 _math_addFloat64 (f64 1.0) (f64 0.0))
        , ("y", prim2 _math_addFloat64 (f64 0.0) (f64 1.0))
        ]),

    -- gradient of x*y w.r.t. [x, y] => {x: x*0+y*1, y: x*1+y*0}
    gradCase "product of two variables"
      (prim2 _math_mulFloat64 (mkVar "x") (mkVar "y"))
      ["x", "y"]
      (mkRecord "Gradient"
        [ ("x", prim2 _math_addFloat64
            (prim2 _math_mulFloat64 (mkVar "x") (f64 0.0))
            (prim2 _math_mulFloat64 (mkVar "y") (f64 1.0)))
        , ("y", prim2 _math_addFloat64
            (prim2 _math_mulFloat64 (mkVar "x") (f64 1.0))
            (prim2 _math_mulFloat64 (mkVar "y") (f64 0.0)))
        ]),

    -- gradient of x w.r.t. [x, y] => {x: 1.0, y: 0.0}
    gradCase "single variable partial"
      (mkVar "x")
      ["x", "y"]
      (mkRecord "Gradient" [("x", f64 1.0), ("y", f64 0.0)]),

    -- gradient of constant w.r.t. [x, y] => {x: 0.0, y: 0.0}
    gradCase "constant"
      (f64 7.0)
      ["x", "y"]
      (mkRecord "Gradient" [("x", f64 0.0), ("y", f64 0.0)])]
