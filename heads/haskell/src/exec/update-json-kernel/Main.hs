module Main where

import Hydra.Generation (writeModulesJson, generateEncoderModules, generateDecoderModules)
import Hydra.Sources.All (kernelModules, kernelEncodingSourceModules)
import qualified Hydra.Kernel as Kernel
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Exception (catch, SomeException)
import qualified Data.List as L
import qualified Data.Set as S


-- | Deduplicate a list of modules by namespace, keeping the first occurrence.
-- Mirrors update-json-main's dedupByNamespace: the synthesized encode/decode
-- modules must not collide with (or shadow) a kernel module of the same name.
dedupByNamespace :: [Kernel.Module] -> [Kernel.Module]
dedupByNamespace = go S.empty
  where
    go _    []     = []
    go seen (m:ms)
      | ns `S.member` seen = go seen ms
      | otherwise          = m : go (S.insert ns seen) ms
      where ns = Kernel.moduleName m

main :: IO ()
main = do
  outputDir <- parseOutputDir "../../dist/json/hydra-kernel/src/main/json"
  putStrLn "=== Generate Hydra kernel JSON ==="
  putStrLn ""

  -- The write set is exactly the kernel modules; this driver is the
  -- authoritative writer for dist/json/hydra-kernel/.../ .
  let writeMods = kernelModules

  -- The INFERENCE universe must additionally include the synthesized
  -- hydra.encode.*/hydra.decode.* modules. Several kernel modules reference
  -- decoder bindings (e.g. hydra.decoding's decodeType, since #740, pulls in
  -- hydra.decode.core.type via decoderFullResultType), and inference of the
  -- kernel write set cannot resolve those references unless the derived
  -- decode modules are present in the universe. update-json-main resolves the
  -- identical references by synthesizing these modules in-memory and adding
  -- them to its universe; this driver must do the same or inference fails with
  -- "no such binding: hydra.decode.core.type".
  --
  -- The synthesizer resolves each source module's type references against its
  -- universe argument, so that universe must cover every encoding-source
  -- module (kernelEncodingSourceModules = kernelTypesModules ++ jsonModules ++
  -- otherModules). kernelModules omits otherModules (e.g. hydra.yaml.model),
  -- so pass kernelModules ++ kernelEncodingSourceModules (deduped) as the
  -- synthesizer universe, otherwise encoder synthesis fails with e.g.
  -- "no such element: hydra.yaml.model.Node".
  let synthUniverse = dedupByNamespace (writeMods ++ kernelEncodingSourceModules)
  encMods <- generateEncoderModules synthUniverse kernelEncodingSourceModules
  decMods <- generateDecoderModules synthUniverse kernelEncodingSourceModules
  let synthesizedEncodeDecode = dedupByNamespace (encMods ++ decMods)
      universe = dedupByNamespace (synthUniverse ++ synthesizedEncodeDecode)

  putStrLn $ "Generating " ++ show (length writeMods) ++ " kernel modules to JSON"
    ++ " (inference universe: " ++ show (length universe) ++ " modules, incl. "
    ++ show (length synthesizedEncodeDecode) ++ " synthesized encode/decode)..."
  putStrLn ""

  result <- catch (writeModulesJson True outputDir universe writeMods >> return True)
                  (\e -> do
                    putStrLn $ "Error: " ++ show (e :: SomeException)
                    return False)

  if result
    then do
      putStrLn ""
      putStrLn "=== Done! ==="
      putStrLn ""
      putStrLn $ "Generated files are in: " ++ outputDir ++ "/"
    else do
      putStrLn ""
      putStrLn "=== FAILED ==="
      exitFailure

parseOutputDir :: String -> IO String
parseOutputDir defaultDir = do
  args <- getArgs
  return $ go args
  where
    go ("--output-dir" : dir : _) = dir
    go (_ : rest) = go rest
    go [] = defaultDir
