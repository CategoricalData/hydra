{-# LANGUAGE BangPatterns #-}

-- | CI regression guard for inference cost (#700).
--
-- Runs the flat, cache-bypassing full-universe inference path
-- ('CodeGeneration.inferModules', the same function 'writeModulesJson' calls
-- via 'inferModulesIO' on a cache miss) over the whole hydra-kernel module
-- set, and reports wall-clock time plus peak memory in use. This is
-- deliberately the worst-case path: 'update-json-main''s normal invocation
-- takes the cheaper per-package incremental route (#381) specifically to
-- avoid this cost, which is exactly why that cheaper path cannot serve as
-- the regression guard — a guard must always measure the expensive case the
-- incremental path is designed to dodge, or regressions in that case go
-- unmeasured.
--
-- Peak memory is read via 'GHC.Stats.getRTSStats', which requires the
-- process to be started with @+RTS -T@ (enabled unconditionally here via
-- -with-rtsopts so callers don't need to remember the flag).
--
-- Usage:
--
-- @
--   stack exec inference-cost-guard -- [--out path/to/result.json]
-- @
--
-- Emits a single JSON object {elapsed_seconds, max_live_bytes,
-- max_mem_in_use_bytes, module_count, ok} to stdout or --out. Exit code is
-- nonzero if inference itself fails (a real inference bug, not a cost-guard
-- concern — bin/test-inference-cost-guard.sh is what asserts thresholds).
module Main where

import Hydra.Kernel
import Hydra.Generation (showError)
import qualified Hydra.Core.Codegen as CodeGeneration
import qualified Hydra.Sources.All as All
import Hydra.Core.Overlay.Haskell.Bootstrap (bootstrapGraph)

import qualified Data.Time.Clock    as Clock
import qualified GHC.Stats          as Stats
import qualified System.Environment as Env
import qualified System.Exit        as Exit
import qualified System.IO          as IO
import           Text.Printf        (printf, hPrintf)

parseOut :: [String] -> Maybe FilePath
parseOut ("--out":p:_) = Just p
parseOut (_:rest)      = parseOut rest
parseOut []            = Nothing

resultJson :: Double -> Word -> Word -> Int -> Bool -> String
resultJson elapsed maxLive maxInUse moduleCount ok =
    "{\n"
    ++ "  \"elapsed_seconds\": " ++ printf "%.6f" elapsed ++ ",\n"
    ++ "  \"max_live_bytes\": " ++ show maxLive ++ ",\n"
    ++ "  \"max_mem_in_use_bytes\": " ++ show maxInUse ++ ",\n"
    ++ "  \"module_count\": " ++ show moduleCount ++ ",\n"
    ++ "  \"ok\": " ++ (if ok then "true" else "false") ++ "\n"
    ++ "}\n"

main :: IO ()
main = do
    IO.hSetBuffering IO.stdout IO.NoBuffering
    args <- Env.getArgs
    let mOut = parseOut args

    statsEnabled <- Stats.getRTSStatsEnabled
    if not statsEnabled
      then do
        IO.hPutStrLn IO.stderr "ERROR: RTS stats not enabled; invoke with +RTS -T -RTS (or rely on -with-rtsopts=-T)."
        Exit.exitWith (Exit.ExitFailure 2)
      else return ()

    -- The whole kernel: universe and target are the same set, matching
    -- update-json-kernel's "one let over the whole universe" shape (#700).
    let universe = All.mainModules
        moduleCount = length universe

    hPrintf IO.stderr "Universe: %d modules\n" moduleCount

    t0 <- Clock.getCurrentTime
    let result = CodeGeneration.inferModules emptyInferenceContext bootstrapGraph universe universe
    let !ok = case result of
                Right _ -> True
                Left _  -> False
    let !err = case result of
                 Right _ -> ""
                 Left e  -> take 500 (showError e)
    t1 <- Clock.getCurrentTime
    stats <- Stats.getRTSStats

    let elapsed = realToFrac (Clock.diffUTCTime t1 t0) :: Double
        maxLive = Stats.max_live_bytes stats
        maxInUse = Stats.max_mem_in_use_bytes stats

    hPrintf IO.stderr "Elapsed: %.2fs  max_live_bytes=%d  max_mem_in_use_bytes=%d\n"
      elapsed maxLive maxInUse

    if not ok
      then IO.hPutStrLn IO.stderr ("Inference FAILED: " ++ err)
      else return ()

    let jsonBody = resultJson elapsed maxLive maxInUse moduleCount ok
    case mOut of
      Just path -> do
        writeFile path jsonBody
        hPrintf IO.stderr "Wrote %s\n" path
      Nothing -> putStr jsonBody

    if ok
      then Exit.exitSuccess
      else Exit.exitWith (Exit.ExitFailure 1)
