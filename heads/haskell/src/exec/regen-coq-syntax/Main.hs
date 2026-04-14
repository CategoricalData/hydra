-- | Temporary one-shot exec: regenerate only the Hydra.Coq.Syntax AST module from
-- its DSL source (packages/hydra-ext/src/main/haskell/Hydra/Sources/Other/Coq.hs).
-- Used while iterating on the Coq AST model during the #337 DSL promotion.
-- Delete once the Coq coder port is done.
module Main where

import Hydra.Kernel
import Hydra.Haskell.Coder (moduleToHaskell)
import Hydra.Haskell.Language (haskellLanguage)
import Hydra.Dsl.Bootstrap (bootstrapGraph)
import Hydra.Sources.Ext (hydraExtModules, mainModules)
import qualified Hydra.Sources.Other.Coq as CoqSyntax
import qualified Hydra.Codegen as CG
import qualified Hydra.Context as Context
import qualified Data.Map as M
import qualified System.FilePath as FP
import qualified System.Directory as SD

main :: IO ()
main = do
  putStrLn "Regenerating only Hydra.Coq.Syntax..."
  let universe = mainModules ++ hydraExtModules
  let modsToGen = [CoqSyntax.module_]
  let cx = Context.Context [] [] M.empty
  case CG.generateSourceFiles
         moduleToHaskell haskellLanguage True False False False
         bootstrapGraph universe modsToGen cx of
    Left err -> do
      putStrLn "Raw error structure:"
      print err
    Right files -> do
      let basePath = "../../dist/haskell/hydra-ext/src/main/haskell"
      mapM_ (writePair basePath) files
      putStrLn $ "Wrote " ++ show (length files) ++ " file(s)"
  where
    writePair basePath (path, s) = do
      let fullPath = FP.combine basePath path
      SD.createDirectoryIfMissing True (FP.takeDirectory fullPath)
      writeFile fullPath s
