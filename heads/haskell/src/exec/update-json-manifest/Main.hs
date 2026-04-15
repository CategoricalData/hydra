module Main where

import Hydra.Generation (writeManifestJson)
import Hydra.Sources.All (kernelModules, haskellModules, jsonModules, otherModules, mainModules, dslSourceModules, testModules)


main :: IO ()
main = do
  putStrLn "=== Generate JSON manifest ==="
  putStrLn ""
  -- Mirror the DSL input set used by update-json-main's writeDslJson call.
  -- Using kernelTypesModules would miss Dsl modules for JSON runtime, yaml,
  -- and the haskell coder.
  let dslInputMods = kernelModules ++ jsonModules ++ otherModules ++ haskellModules
  -- Include dslSourceModules (hydra.dsls, a Source aggregator module) in the
  -- published mainModules list so consumers of manifest.json see it alongside
  -- the rest. Today's mainModules excludes it to work around an inference
  -- stack-overflow at DSL→Haskell generation time, but that only applies to
  -- the live-DSL path; at JSON export time, the module is already typed.
  let mainModulesWithDsls = mainModules ++ dslSourceModules
  writeManifestJson "../../dist/json/hydra-kernel/src/main/json" kernelModules dslInputMods mainModulesWithDsls testModules
  putStrLn ""
  putStrLn "=== Done! ==="
