{-# LANGUAGE BangPatterns #-}

-- | Cross-host inference benchmark — Haskell runner.
--
-- Loads the synthetic 'hydra.bench.inferenceScaling' workload from the kernel
-- JSON, takes prefixes of the chained walker definitions, and times
-- 'CodeGeneration.inferModulesGiven' on each prefix. Emits a JSON array
-- describing @{host, n, elapsedSeconds, ok}@ per prefix size.
--
-- Usage:
--
-- @
--   stack exec bench-inference -- [--sizes 10,25,50,100] [--out path/to/result.json]
-- @
--
-- This executable is invoked by @bin/run-inference-bench.sh@, which also
-- dispatches to per-host runners and aggregates results.
module Main where

import Hydra.Kernel
import Hydra.Generation (showError)
import qualified Hydra.Codegen as CodeGeneration
import qualified Hydra.Sources.All as All
import qualified Hydra.Sources.Ext as Ext
import Hydra.Dsl.Bootstrap (bootstrapGraph, unqualifiedDep)

import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Time.Clock         as Clock
import qualified System.Environment      as Env
import qualified System.IO               as IO
import qualified System.Exit             as Exit
import           Text.Printf             (printf, hPrintf)

defaultBenchNamespace :: ModuleName
defaultBenchNamespace = ModuleName "hydra.bench.linearChain"

-- | Default prefix sizes: 0 (empty-target baseline), 10, 25, 50, 100.
defaultSizes :: [Int]
defaultSizes = [0, 10, 25, 50, 100]

-- | Parse a comma-separated list of integers.
parseSizes :: String -> [Int]
parseSizes s = L.sort $ L.nub
  [ read t :: Int | t <- splitOn ',' s, not (null t) ]
  where
    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn c xs = let (h, t) = break (== c) xs
                   in h : case t of
                            [] -> []
                            (_:r) -> splitOn c r

-- | Parse argv into (sizes, namespace, optional output path).
parseArgs :: [String] -> ([Int], ModuleName, Maybe FilePath)
parseArgs = go defaultSizes defaultBenchNamespace Nothing
  where
    go sz nsv out [] = (sz, nsv, out)
    go _  nsv out ("--sizes":s:rest)     = go (parseSizes s) nsv out rest
    go sz _   out ("--namespace":n:rest) = go sz (ModuleName n) out rest
    go sz nsv _   ("--out":p:rest)       = go sz nsv (Just p) rest
    go sz nsv out (_:rest)               = go sz nsv out rest

-- | Build a synthetic module containing the first n walker defs of the bench.
--
-- The defs are renamed into a private namespace ("z.bench.scaling"). The
-- bench module remains in the universe so 'walker(k-1)' lookups still
-- resolve; the synthetic module is what we actually time.
makeSyntheticModule :: Module -> Int -> Module
makeSyntheticModule benchMod n =
    Module {
        moduleName = targetNs,
        moduleDefinitions = renamed,
        moduleDependencies = unqualifiedDep (moduleName benchMod) : moduleDependencies benchMod,
        moduleMetadata = moduleMetadata benchMod
      }
  where
    targetNs = ModuleName "z.bench.scaling"
    -- Definitions come in declaration order: walker0, walker1, ...
    take' = take n (moduleDefinitions benchMod)
    renamed = [ renameDef d | d <- take' ]
    renameDef d = case d of
      DefinitionTerm td ->
        let local = lastDot (unName (termDefinitionName td))
            newName = Name (unModuleName targetNs ++ "." ++ local)
        in DefinitionTerm (td { termDefinitionName = newName })
      other -> other  -- keep type defs etc as-is (bench has no type defs)
    lastDot s = case L.elemIndices '.' s of
      [] -> s
      ixs -> drop (last ixs + 1) s

-- | Wallclock time a single inference run.
timeInference :: [Module] -> Module -> IO (Double, Bool, String)
timeInference universe target = do
    t0 <- Clock.getCurrentTime
    let result = CodeGeneration.inferModulesGiven
                   (InferenceContext { inferenceContextFreshTypeVariableCount = 0, inferenceContextTrace = [] })
                   bootstrapGraph
                   (universe ++ [target])
                   [target]
    let !ok = case result of
                Right _ -> True
                Left _ -> False
    let !err = case result of
                 Right _ -> ""
                 Left e -> take 200 (showError e)
    t1 <- Clock.getCurrentTime
    let elapsed = realToFrac (Clock.diffUTCTime t1 t0) :: Double
    return (elapsed, ok, err)

-- | Format one result as a JSON object.
resultJson :: String -> String -> Int -> Double -> Bool -> String -> String
resultJson host nsv n elapsed ok err =
    "  {\n"
    ++ "    \"host\": \"" ++ host ++ "\",\n"
    ++ "    \"namespace\": \"" ++ nsv ++ "\",\n"
    ++ "    \"n\": " ++ show n ++ ",\n"
    ++ "    \"elapsed_seconds\": " ++ printf "%.6f" elapsed ++ ",\n"
    ++ "    \"ok\": " ++ (if ok then "true" else "false") ++ ",\n"
    ++ "    \"error\": " ++ (if ok then "null" else "\"" ++ escape err ++ "\"") ++ "\n"
    ++ "  }"
  where
    escape = concatMap (\c -> case c of '"' -> "\\\""; '\\' -> "\\\\"; '\n' -> "\\n"; _ -> [c])

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    args <- Env.getArgs
    let (sizes, benchNs, mOut) = parseArgs args
    let host = "haskell"

    -- Universe: kernel mainModules + the hydra-bench package (linked at build time).
    -- hydra-bench is a separate package whose modules are not part of the standard
    -- sync pipeline; running this exec is the canonical way to exercise them.
    let universe = All.mainModules ++ Ext.hydraBenchModules
    hPrintf IO.stderr "Universe: %d modules\n" (length universe)

    -- Find the bench module.
    case L.find (\m -> moduleName m == benchNs) universe of
      Nothing -> do
        hPrintf IO.stderr "ERROR: bench module %s not found in linked hydra-bench package.\n" (unModuleName benchNs)
        Exit.exitWith (Exit.ExitFailure 2)
      Just benchMod -> do
        let avail = length (moduleDefinitions benchMod)
        hPrintf IO.stderr "Bench workload %s: %d definitions available\n" (unModuleName benchNs) avail

        -- Run inference at each requested size.
        results <- L.foldl' (\acc n -> acc >>= \prev -> do
            if n > avail
              then do
                hPrintf IO.stderr "  skipping n=%d (only %d defs available)\n" n avail
                return prev
              else do
                let target = makeSyntheticModule benchMod n
                (elapsed, ok, err) <- timeInference universe target
                let status = if ok then "OK" else "FAIL: " ++ err
                hPrintf IO.stderr "  n=%3d: %6.2fs %s\n" n elapsed status
                return (prev ++ [(n, elapsed, ok, err)])
            ) (return []) sizes

        -- Emit JSON.
        let jsonBody = "[\n" ++ L.intercalate ",\n"
                         [ resultJson host (unModuleName benchNs) n elapsed ok err
                         | (n, elapsed, ok, err) <- results ]
                       ++ "\n]\n"
        case mOut of
          Just path -> do
            writeFile path jsonBody
            hPrintf IO.stderr "Wrote %s\n" path
          Nothing -> putStr jsonBody

        let allOk = all (\(_, _, ok, _) -> ok) results
        if allOk
          then Exit.exitSuccess
          else Exit.exitWith (Exit.ExitFailure 1)
