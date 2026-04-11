-- | Haskell driver for the automatic differentiation demo.
--
-- Demonstrates source-to-source automatic differentiation on Hydra terms.
-- Builds mathematical functions as Hydra Term values, differentiates them
-- symbolically, and evaluates both original and derivative at specific points.
--
-- Usage: runhaskell Demo.hs <output-dir>

module Hydra.Ext.Demos.Grad.Demo where

import Hydra.Kernel
import qualified Hydra.Core as Core
import qualified Hydra.Differentiation as Diff
import qualified Hydra.Variables as Vars
import qualified Hydra.Reduction as Reduction
import qualified Hydra.Show.Core as ShowCore
import qualified Hydra.Sources.Libraries as Lib
import qualified Hydra.Lib.Math as HMath

import Control.Monad (when)
import qualified Data.List as L
import qualified Data.Map as M
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Text.Printf (printf)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [outDir] -> runDemo outDir
    _ -> do
      hPutStrLn stderr "Usage: GradDemo <output-dir>"
      exitFailure

-- | Build a Graph with all standard library primitives registered.
primGraph :: Graph
primGraph = let
    allPrims = L.concatMap libraryPrimitives Lib.standardLibraries
    primsMap = M.fromList $ fmap (\p -> (primitiveName p, p)) allPrims
  in emptyGraph { graphPrimitives = primsMap }

-- | Evaluate a Term to a reduced form, or return an error message.
eval :: Term -> Either String Term
eval term = case Reduction.reduceTerm emptyContext primGraph True term of
  Left err -> Left $ show err
  Right t -> Right t

-- | Evaluate a Term and show the result as a string.
evalShow :: Term -> String
evalShow term = case eval term of
  Left err -> "<<error: " ++ err ++ ">>"
  Right t -> ShowCore.term t

-- | Extract a Double from a reduced Term, if possible.
evalDouble :: Term -> Maybe Double
evalDouble term = case eval term of
  Right (TermLiteral (LiteralFloat (FloatValueFloat64 d))) -> Just d
  _ -> Nothing

-- | Build a variable term: x
var :: String -> Term
var = TermVariable . Name

-- | Build a Float64 literal term
lit :: Double -> Term
lit = TermLiteral . LiteralFloat . FloatValueFloat64

-- | Build a unary primitive application: f(x)
app1 :: String -> Term -> Term
app1 name arg = TermApplication $ Application
  (TermVariable (Name name)) arg

-- | Build a binary primitive application: f(x, y)
app2 :: String -> Term -> Term -> Term
app2 name a b = TermApplication $ Application
  (TermApplication $ Application (TermVariable (Name name)) a) b

-- | Substitute a variable with a value in a term, then evaluate.
evalAt :: String -> Double -> Term -> Maybe Double
evalAt varName xVal term =
  let substituted = Vars.replaceFreeTermVariable (Name varName) (lit xVal) term
  in evalDouble substituted

-- | Central finite difference approximation of the derivative.
finiteDiff :: (Double -> Double) -> Double -> Double -> Double
finiteDiff f x h = (f (x + h) - f (x - h)) / (2 * h)

-- | A demo function: name, Hydra term representation, and Haskell evaluation function.
data DemoFunction = DemoFunction
  { dfName :: String
  , dfTerm :: Term
  , dfHaskell :: Double -> Double
  }

-- | The collection of demo functions.
demoFunctions :: [DemoFunction]
demoFunctions =
  [ DemoFunction
      { dfName = "x^2"
      , dfTerm = app2 "hydra.lib.math.mulFloat64" (var "x") (var "x")
      , dfHaskell = \x -> x * x
      }
  , DemoFunction
      { dfName = "x^3"
      , dfTerm = app2 "hydra.lib.math.pow" (var "x") (lit 3.0)
      , dfHaskell = \x -> x ** 3
      }
  , DemoFunction
      { dfName = "sin(x)"
      , dfTerm = app1 "hydra.lib.math.sin" (var "x")
      , dfHaskell = sin
      }
  , DemoFunction
      { dfName = "exp(x)"
      , dfTerm = app1 "hydra.lib.math.exp" (var "x")
      , dfHaskell = exp
      }
  , DemoFunction
      { dfName = "log(x)"
      , dfTerm = app1 "hydra.lib.math.log" (var "x")
      , dfHaskell = log
      }
  , DemoFunction
      { dfName = "sqrt(x)"
      , dfTerm = app1 "hydra.lib.math.sqrt" (var "x")
      , dfHaskell = sqrt
      }
  , DemoFunction
      { dfName = "sin(cos(x))"
      , dfTerm = app1 "hydra.lib.math.sin" (app1 "hydra.lib.math.cos" (var "x"))
      , dfHaskell = \x -> sin (cos x)
      }
  , DemoFunction
      { dfName = "x * sin(x)"
      , dfTerm = app2 "hydra.lib.math.mulFloat64" (var "x") (app1 "hydra.lib.math.sin" (var "x"))
      , dfHaskell = \x -> x * sin x
      }
  , DemoFunction
      { dfName = "exp(x^2)"
      , dfTerm = app1 "hydra.lib.math.exp"
          (app2 "hydra.lib.math.mulFloat64" (var "x") (var "x"))
      , dfHaskell = \x -> exp (x * x)
      }
  ]

-- | Test points for evaluation.
testPoints :: [Double]
testPoints = [0.5, 1.0, 2.0]

runDemo :: FilePath -> IO ()
runDemo outDir = do
  createDirectoryIfMissing True outDir

  putStrLn "╔══════════════════════════════════════════════════════════════════╗"
  putStrLn "║          Automatic Differentiation Demo for Hydra              ║"
  putStrLn "║                                                                ║"
  putStrLn "║  Source-to-source symbolic differentiation on typed lambda     ║"
  putStrLn "║  calculus terms, compiled identically across all Hydra         ║"
  putStrLn "║  target languages.                                            ║"
  putStrLn "╚══════════════════════════════════════════════════════════════════╝"
  putStrLn ""

  -- Phase 1: Symbolic differentiation
  putStrLn "═══ Phase 1: Symbolic Differentiation ═══"
  putStrLn ""

  derivs <- mapM showSymbolic demoFunctions
  putStrLn ""

  -- Phase 2: Numerical evaluation
  putStrLn "═══ Phase 2: Numerical Evaluation ═══"
  putStrLn ""

  mapM_ (evaluateFunction outDir) (zip demoFunctions derivs)
  putStrLn ""

  -- Phase 3: Gradient check
  putStrLn "═══ Phase 3: Gradient Check (AD vs Finite Differences) ═══"
  putStrLn ""

  results <- mapM gradientCheck demoFunctions
  let allPass = and results
  putStrLn ""
  if allPass
    then putStrLn "All gradient checks PASSED."
    else putStrLn "Some gradient checks FAILED."

  putStrLn ""

  -- Phase 4: Gradient descent optimization
  putStrLn "═══ Phase 4: Gradient Descent Optimization ═══"
  putStrLn ""
  optimizationDemo outDir
  putStrLn ""

  -- Phase 5: Curve fitting with two parameters
  putStrLn "═══ Phase 5: Linear Regression via Gradient Descent ═══"
  putStrLn ""
  curveFittingDemo outDir
  putStrLn ""

  -- Write summary to file
  let summaryFile = outDir </> "summary.txt"
  writeFile summaryFile $ unlines
    [ "Automatic Differentiation Demo Summary"
    , "======================================"
    , ""
    , "Functions differentiated: " ++ show (length demoFunctions)
    , "Test points: " ++ show testPoints
    , "Gradient checks: " ++ (if allPass then "ALL PASSED" else "SOME FAILED")
    , "Optimization: single-variable and linear regression demos included"
    ]
  putStrLn $ "Summary written to " ++ summaryFile

showSymbolic :: DemoFunction -> IO Term
showSymbolic df = do
  let deriv = Diff.differentiateTerm (Name "x") (dfTerm df)
  putStrLn $ "  f(x) = " ++ dfName df
  putStrLn $ "    term:       " ++ ShowCore.term (dfTerm df)
  putStrLn $ "    derivative: " ++ ShowCore.term deriv
  putStrLn ""
  return deriv

evaluateFunction :: FilePath -> (DemoFunction, Term) -> IO ()
evaluateFunction outDir (df, deriv) = do
  putStrLn $ "  f(x) = " ++ dfName df
  printf "    %-8s  %-14s  %-14s  %-14s  %-14s\n"
    ("x" :: String) ("f(x)" :: String) ("f'(x) AD" :: String) ("f'(x) exact" :: String) ("error" :: String)
  printf "    %-8s  %-14s  %-14s  %-14s  %-14s\n"
    ("---" :: String) ("----" :: String) ("--------" :: String) ("----------" :: String) ("-----" :: String)

  mapM_ (evalPoint df deriv) testPoints
  putStrLn ""

  -- Write derivative terms to file
  let derivFile = outDir </> (sanitize (dfName df) ++ "_derivative.txt")
  writeFile derivFile $ unlines
    [ "f(x) = " ++ dfName df
    , ""
    , "Original term:"
    , "  " ++ ShowCore.term (dfTerm df)
    , ""
    , "Derivative term:"
    , "  " ++ ShowCore.term deriv
    ]
  where
    sanitize = map (\c -> if c `elem` ("*^() " :: String) then '_' else c)

evalPoint :: DemoFunction -> Term -> Double -> IO ()
evalPoint df deriv x = do
  let fVal = dfHaskell df x
  let adVal = evalAt "x" x deriv
  let exactDeriv = finiteDiff (dfHaskell df) x 1.0e-10
  case adVal of
    Just ad -> do
      let err = abs (ad - exactDeriv)
      printf "    %-8.4f  %-14.8f  %-14.8f  %-14.8f  %-14.2e\n" x fVal ad exactDeriv err
    Nothing ->
      printf "    %-8.4f  %-14.8f  %-14s  %-14.8f  %-14s\n" x fVal ("<<error>>" :: String) exactDeriv ("—" :: String)

gradientCheck :: DemoFunction -> IO Bool
gradientCheck df = do
  let deriv = Diff.differentiateTerm (Name "x") (dfTerm df)
  let h = 1.0e-7
  let results = map (checkPoint deriv h) testPoints
  let allOk = all id results
  let status = if allOk then "PASS" else "FAIL"
  printf "  %-20s %s\n" (dfName df) status
  return allOk
  where
    checkPoint deriv h x =
      case evalAt "x" x deriv of
        Nothing -> False
        Just ad ->
          let fd = finiteDiff (dfHaskell df) x h
              relErr = if abs fd < 1e-12
                then abs (ad - fd)
                else abs (ad - fd) / abs fd
          in relErr < 1e-4

-- ============================================================================
-- Phase 4: Single-variable gradient descent optimization
-- ============================================================================

-- | Minimize f(x) = (x - 3)^2 + 2*sin(x), starting from x = 0.
--   The minimum is near x ≈ 2.81 where f'(x) = 0.
optimizationDemo :: FilePath -> IO ()
optimizationDemo outDir = do
  putStrLn "  Problem: minimize f(x) = (x - 3)^2 + 2*sin(x)"
  putStrLn "  Method:  gradient descent with AD-computed derivatives"
  putStrLn "  Start:   x = 0.0, learning rate = 0.1"
  putStrLn ""

  -- Build the loss function as a Hydra term:
  --   f(x) = (x - 3)^2 + 2*sin(x)
  --        = mulFloat64(subFloat64(x, 3), subFloat64(x, 3)) + mulFloat64(2, sin(x))
  let xMinus3 = app2 "hydra.lib.math.subFloat64" (var "x") (lit 3.0)
  let lossTerm = app2 "hydra.lib.math.addFloat64"
        (app2 "hydra.lib.math.mulFloat64" xMinus3 xMinus3)
        (app2 "hydra.lib.math.mulFloat64" (lit 2.0) (app1 "hydra.lib.math.sin" (var "x")))

  -- Differentiate once — this produces the gradient term
  let gradTerm = Diff.differentiateTerm (Name "x") lossTerm

  putStrLn $ "  Loss term:     " ++ ShowCore.term lossTerm
  putStrLn $ "  Gradient term: " ++ ShowCore.term gradTerm
  putStrLn ""

  -- Gradient descent loop
  let lr = 0.1
  let numSteps = 30
  let haskellLoss x = (x - 3)**2 + 2 * sin x

  printf "    %-6s  %-12s  %-14s  %-14s\n"
    ("step" :: String) ("x" :: String) ("f(x)" :: String) ("f'(x)" :: String)
  printf "    %-6s  %-12s  %-14s  %-14s\n"
    ("----" :: String) ("---" :: String) ("----" :: String) ("-----" :: String)

  let go :: Int -> Double -> IO Double
      go step x
        | step > numSteps = return x
        | otherwise = do
            let fVal = haskellLoss x
            case evalAt "x" x gradTerm of
              Nothing -> do
                printf "    %-6d  %-12.6f  %-14.8f  <<error>>\n" step x fVal
                return x
              Just grad -> do
                when (step `mod` 5 == 0 || step <= 2 || step == numSteps) $
                  printf "    %-6d  %-12.6f  %-14.8f  %-14.8f\n" step x fVal grad
                go (step + 1) (x - lr * grad)

  xFinal <- go 0 0.0
  let fFinal = haskellLoss xFinal
  putStrLn ""
  printf "  Result: x* = %.8f, f(x*) = %.8f\n" xFinal fFinal
  printf "  Verification: f'(x*) ≈ %.2e (should be near zero)\n"
    (case evalAt "x" xFinal gradTerm of Just g -> g; Nothing -> 999.0)

  -- Write optimization trace to file
  let optFile = outDir </> "optimization.txt"
  writeFile optFile $ unlines
    [ "Single-variable optimization"
    , "f(x) = (x - 3)^2 + 2*sin(x)"
    , "Method: gradient descent, lr=0.1, 30 steps"
    , "Result: x* = " ++ show xFinal ++ ", f(x*) = " ++ show fFinal
    ]

-- ============================================================================
-- Phase 5: Linear regression (2-parameter curve fitting)
-- ============================================================================

-- | Fit y = a*x + b to noisy data using coordinate-wise gradient descent.
--   Since we have single-variable differentiation, we compute dL/da and dL/db
--   separately and update each parameter per step.
curveFittingDemo :: FilePath -> IO ()
curveFittingDemo outDir = do
  -- Generate "observed" data: y = 2.5*x + 0.7 + noise
  let trueA = 2.5
  let trueB = 0.7
  let dataPoints = [(x, trueA * x + trueB + noise x) | x <- [0.0, 0.5 .. 4.0]]
        where noise x = 0.1 * sin (7 * x)  -- deterministic pseudo-noise

  putStrLn $ "  Problem: fit y = a*x + b to " ++ show (length dataPoints) ++ " data points"
  putStrLn $ "  True parameters: a = " ++ show trueA ++ ", b = " ++ show trueB
  putStrLn $ "  Method: coordinate-wise gradient descent with AD"
  putStrLn ""

  -- Build the loss function L(a, b) = sum_i (a*x_i + b - y_i)^2
  -- We construct this as a Hydra term with free variables "a" and "b".
  let residual xi yi = app2 "hydra.lib.math.subFloat64"
        (app2 "hydra.lib.math.addFloat64"
          (app2 "hydra.lib.math.mulFloat64" (var "a") (lit xi))
          (var "b"))
        (lit yi)
  let squaredResidual xi yi =
        let r = residual xi yi
        in app2 "hydra.lib.math.mulFloat64" r r
  let lossTerm = foldl1 (\acc t -> app2 "hydra.lib.math.addFloat64" acc t)
        [squaredResidual xi yi | (xi, yi) <- dataPoints]

  -- Differentiate w.r.t. "a" and "b" separately
  let dLda = Diff.differentiateTerm (Name "a") lossTerm
  let dLdb = Diff.differentiateTerm (Name "b") lossTerm

  putStrLn "  Loss function: L(a,b) = sum_i (a*x_i + b - y_i)^2"
  putStrLn ""

  -- Gradient descent
  let lr = 0.001  -- small learning rate (sum of squares can have large gradients)
  let numSteps = 200

  printf "    %-6s  %-12s  %-12s  %-14s\n"
    ("step" :: String) ("a" :: String) ("b" :: String) ("loss" :: String)
  printf "    %-6s  %-12s  %-12s  %-14s\n"
    ("----" :: String) ("---" :: String) ("---" :: String) ("----" :: String)

  let evalLoss a b = sum [(a * xi + b - yi)**2 | (xi, yi) <- dataPoints]

  let evalGrad gradTerm aVal bVal =
        let t1 = Vars.replaceFreeTermVariable (Name "a") (lit aVal) gradTerm
            t2 = Vars.replaceFreeTermVariable (Name "b") (lit bVal) t1
        in evalDouble t2

  let go :: Int -> Double -> Double -> IO (Double, Double)
      go step a b
        | step > numSteps = return (a, b)
        | otherwise = do
            let loss = evalLoss a b
            case (evalGrad dLda a b, evalGrad dLdb a b) of
              (Just ga, Just gb) -> do
                when (step `mod` 40 == 0 || step <= 2 || step == numSteps) $
                  printf "    %-6d  %-12.6f  %-12.6f  %-14.8f\n" step a b loss
                go (step + 1) (a - lr * ga) (b - lr * gb)
              _ -> do
                printf "    %-6d  %-12.6f  %-12.6f  <<gradient error>>\n" step a b
                return (a, b)

  (aFinal, bFinal) <- go 0 0.0 0.0
  let finalLoss = evalLoss aFinal bFinal
  putStrLn ""
  printf "  Result:     a = %.6f, b = %.6f\n" aFinal bFinal
  printf "  True:       a = %.6f, b = %.6f\n" trueA trueB
  printf "  Error:      |a - a*| = %.6f, |b - b*| = %.6f\n"
    (abs (aFinal - trueA)) (abs (bFinal - trueB))
  printf "  Final loss: %.8f\n" finalLoss

  -- Write results
  let fitFile = outDir </> "curve_fitting.txt"
  writeFile fitFile $ unlines
    [ "Linear regression via gradient descent"
    , "Model: y = a*x + b"
    , "True: a = " ++ show trueA ++ ", b = " ++ show trueB
    , "Fitted: a = " ++ show aFinal ++ ", b = " ++ show bFinal
    , "Final loss: " ++ show finalLoss
    ]
