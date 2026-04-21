-- | Per-target freshness check + digest refresh tool.
--
-- Two subcommands:
--
--   digest-check fresh --inputs <file> --output-digest <file>
--     Reads the input digest (e.g. dist/json/<pkg>/digest.json) and
--     the output digest (e.g. dist/<lang>/<pkg>/digest.json).
--     Returns exit 0 if:
--       * the inputs section of the output digest matches the input
--         digest's hashes,
--       * every output file recorded in the output digest exists and
--         hashes to its recorded value,
--       * the generator stamp matches the current generator stamp.
--     Returns exit 1 (cache miss) otherwise.
--
--     Callers use this with an `if !` shell idiom to skip work when
--     fresh and run the work otherwise.
--
--   digest-check refresh --inputs <file> --output-dir <dir>
--                        --output-digest <file>
--     Recomputes the hash of every regular file under <dir> (recursive),
--     reads the input digest, and writes a new output digest with both
--     sections populated plus the current generator stamp.
--
--     Run after a successful regen to record what was produced.
--
-- All paths are taken as-is (no implicit normalization).

module Main where

import Hydra.Digest
import Hydra.Packaging (Namespace(..))

import Control.Monad (when, forM)
import Data.List (isPrefixOf)
import qualified Data.Map as M
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import qualified System.FilePath as FP
import System.IO (hPutStrLn, stderr)

data Mode = Fresh | Refresh deriving (Eq, Show)

data Options = Options
  { optMode         :: Mode
  , optInputDigest  :: FilePath
  , optOutputDigest :: FilePath
  , optOutputDir    :: Maybe FilePath
  } deriving Show

usage :: String
usage = unlines
  [ "Usage:"
  , "  digest-check fresh   --inputs <file> --output-dir <dir> --output-digest <file>"
  , "  digest-check refresh --inputs <file> --output-dir <dir> --output-digest <file>"
  , ""
  , "  fresh:    exit 0 if cache hit (skip work), exit 1 if miss (do work)."
  , "            Resolves recorded (relative) output paths against <output-dir>."
  , "  refresh:  walk <output-dir>, hash every file, write a new"
  , "            <output-digest> with paths stored relative to <output-dir>."
  ]

parseArgs :: [String] -> Either String Options
parseArgs [] = Left "Missing subcommand"
parseArgs (cmd : rest) = do
  mode <- case cmd of
    "fresh"   -> Right Fresh
    "refresh" -> Right Refresh
    _         -> Left ("Unknown subcommand: " ++ cmd)
  go (Options mode "" "" Nothing) rest
  where
    go opts [] = if null (optInputDigest opts) || null (optOutputDigest opts)
      then Left "Missing required --inputs or --output-digest"
      else if optOutputDir opts == Nothing
        then Left "--output-dir is required"
        else Right opts
    go opts ("--inputs" : v : xs)        = go (opts { optInputDigest  = v }) xs
    go opts ("--output-digest" : v : xs) = go (opts { optOutputDigest = v }) xs
    go opts ("--output-dir" : v : xs)    = go (opts { optOutputDir    = Just v }) xs
    go _ (a : _) = Left ("Unknown argument: " ++ a)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr ("Error: " ++ err)
      hPutStrLn stderr usage
      exitFailure
    Right opts -> case optMode opts of
      Fresh   -> doFresh opts
      Refresh -> doRefresh opts

doFresh :: Options -> IO ()
doFresh opts = do
  -- Input digest: must exist. Absent means we have no record of what
  -- was used, so always treat as cache miss.
  inputExists <- doesFileExist (optInputDigest opts)
  if not inputExists
    then do
      putStrLn $ "  digest-check: input digest absent ("
        ++ optInputDigest opts ++ "); cache miss"
      exitFailure
    else return ()

  -- Output digest: must exist. Absent means we have no record of
  -- what was produced previously.
  outputExists <- doesFileExist (optOutputDigest opts)
  if not outputExists
    then do
      putStrLn $ "  digest-check: output digest absent ("
        ++ optOutputDigest opts ++ "); cache miss"
      exitFailure
    else return ()

  inputDigest  <- Hydra.Digest.readDigest (optInputDigest opts)
  outputDigest <- Hydra.Digest.readDigestV2 (optOutputDigest opts)

  -- Compare: each input hash from the v1 input digest must appear,
  -- with the same hash, in the v2 output digest's inputs map. (We
  -- key by namespace string in v1; in v2 we use FilePath. The bridge
  -- is namespace strings live as paths in the output digest, recorded
  -- by 'refresh' below.)
  let recordedInputs = M.map entryHash (digestInputs outputDigest)
      currentInputs  = M.fromList
        [ (k, v) | (Namespace k, v) <- M.toList inputDigest ]

  if recordedInputs /= currentInputs
    then do
      putStrLn $ "  digest-check: input mismatch; cache miss"
      exitFailure
    else return ()

  -- Generator stamp must match.
  currentGen <- generatorStamp
  if currentGen /= digestGenerator outputDigest
    then do
      putStrLn $ "  digest-check: generator stamp mismatch ("
        ++ digestGenerator outputDigest ++ " vs " ++ currentGen
        ++ "); cache miss"
      exitFailure
    else return ()

  -- Output files must all exist with matching hashes. Paths recorded
  -- in the digest are relative to outputDir (per 'refresh' below).
  let outputDir = case optOutputDir opts of
        Just d  -> d
        Nothing -> error "doFresh called without output-dir (parseArgs bug)"
  outputsOk <- fmap and $ forM (M.toList (digestOutputs outputDigest)) $ \(rel, entry) -> do
    let abs_ = outputDir FP.</> rel
    exists <- doesFileExist abs_
    if not exists then return False else do
      h <- Hydra.Digest.hashFile abs_
      return (h == entryHash entry)
  if not outputsOk
    then do
      putStrLn $ "  digest-check: output files missing or modified; cache miss"
      exitFailure
    else do
      putStrLn $ "  digest-check: cache hit; skipping work"
      exitSuccess

doRefresh :: Options -> IO ()
doRefresh opts = do
  let outputDir = case optOutputDir opts of
        Just d  -> d
        Nothing -> error "doRefresh called without output-dir (parseArgs bug)"
  -- Read the input digest. May be absent if the inputs themselves
  -- aren't cached yet; that's OK, we'll just record an empty inputs
  -- map and the next 'fresh' check will miss until inputs settle.
  inputDigest <- Hydra.Digest.readDigest (optInputDigest opts)
  let inputsAsMap = M.fromList
        [ (k, DigestEntry KindOther v)
        | (Namespace k, v) <- M.toList inputDigest
        ]

  -- Walk the output dir. Paths are stored relative to outputDir for
  -- portability (absolute paths would bake in the worktree location
  -- and break across machines, CI runners, etc).
  --
  -- Exclude the output-digest file itself from the walk: we're about
  -- to overwrite it, and hashing it here would (a) be a self-reference
  -- that changes every run and (b) race with writeDigestV2 below.
  allFiles <- listFilesRecursive outputDir
  -- Normalize both sides so "dir//digest.json" (double slash from a
  -- pkg_dir with trailing /) compares equal to "dir/digest.json" as
  -- emitted by listFilesRecursive.
  let digestPath = FP.normalise (optOutputDigest opts)
      files = filter (\fp -> FP.normalise fp /= digestPath) allFiles
  outputs <- fmap M.fromList $ forM files $ \fp -> do
    h <- Hydra.Digest.hashFile fp
    let rel = makeRelative' outputDir fp
    return (rel, DigestEntry KindTargetFile h)

  gen <- generatorStamp

  let d = Digest
        { digestInputs    = inputsAsMap
        , digestOutputs   = outputs
        , digestGenerator = gen
        }

  Hydra.Digest.writeDigestV2 (optOutputDigest opts) d
  putStrLn $ "  digest-check: wrote " ++ optOutputDigest opts
    ++ " (" ++ show (M.size inputsAsMap) ++ " inputs, "
    ++ show (M.size outputs) ++ " outputs)"

-- | Recursively list every regular file under a directory.
-- Skips dotfiles and dot-directories.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive root = do
  exists <- doesDirectoryExist root
  if not exists then return [] else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \e ->
        if "." `isPrefixOf` e
          then return []
          else do
            let p = dir FP.</> e
            isDir <- doesDirectoryExist p
            if isDir
              then go p
              else do
                isFile <- doesFileExist p
                return (if isFile then [p] else [])

-- | Compute 'path' relative to 'base'. If 'path' isn't under 'base',
-- returns 'path' unchanged (callers should guard against that, but the
-- fallback keeps us from producing absolute paths accidentally).
makeRelative' :: FilePath -> FilePath -> FilePath
makeRelative' base path =
  let prefix = if not (null base) && last base == '/' then base else base ++ "/"
  in if prefix `isPrefixOf` path
       then drop (length prefix) path
       else path
