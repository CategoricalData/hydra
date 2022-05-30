module Hydra.Impl.Haskell.GraphIO (
  generateHaskell,
  generateJava,
  generatePdl,
  generateScala,
  coreModules,
  
  testModules,
  javaTestModules,
) where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Extras
import Hydra.Ext.Haskell.Serde
import Hydra.Ext.Java.Serde
import Hydra.Ext.Pegasus.Serde
import Hydra.Ext.Scala.Serde
import Hydra.Util.Codetree.Print
import Hydra.Impl.Haskell.Sources.Adapter
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Errors
import Hydra.Impl.Haskell.Sources.Evaluation
import Hydra.Impl.Haskell.Sources.Graph
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Impl.Haskell.Sources.Ext.Haskell.Ast
import Hydra.Impl.Haskell.Sources.Ext.Java.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Json.Json
import Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl
import Hydra.Impl.Haskell.Sources.Ext.Scala.Meta
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Features
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Typed
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3
import Hydra.Impl.Haskell.Sources.Ext.Yaml.Model
import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Util.Formatting
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Libraries
import qualified Hydra.Lib.Strings as Strings

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD


generateHaskell :: [Module Meta] -> FP.FilePath -> IO ()
generateHaskell = generateSources (toFileName True ".hs") moduleToHaskellString

generateJava :: [Module Meta] -> FP.FilePath -> IO ()
generateJava = generateSources (toFileName False ".java") moduleToJavaString

generatePdl :: [Module Meta] -> FP.FilePath -> IO ()
generatePdl = generateSources (toFileName False ".pdl") moduleToPdlString

generateScala :: [Module Meta] -> FP.FilePath -> IO ()
generateScala = generateSources (toFileName False ".scala") moduleToScalaString

generateSources :: (GraphName -> FP.FilePath) -> (Context Meta -> Graph Meta -> Qualified String) -> [Module Meta] -> FP.FilePath -> IO ()
generateSources toFile serialize modules baseDir = do
    CM.mapM writeDataGraph modules
    return ()
  where
    writeDataGraph (Module g deps) = do
      let cx = setContextElements (g:(moduleGraph <$> deps)) $ standardContext {
             contextGraphs = GraphSet (M.fromList [
               (graphName g, g),
               (hydraCoreName, hydraCore)]) (graphName g),
             contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives}
      writeGraph serialize cx g $ Just $ FP.combine baseDir $ toFile (graphName g)

coreModules :: [Module Meta]
coreModules = [
  hydraAdapterModule,
  hydraCoreModule,
  hydraErrorsModule,
  hydraEvaluationModule,
  hydraGraphModule,
  hydraBasicsModule,
  adapterUtilsModule,
  haskellAstModule,
  javaSyntaxModule,
  jsonJsonModule,
  pegasusPdlModule,
  scalaMetaModule,
  yamlModelModule,
  codetreeAstModule,
  tinkerpopFeaturesModule,
  tinkerpopTypedModule,
  tinkerpopV3Module]

-- TODO: remove me eventually. Handy for debugging.
testModules :: [Module Meta]
testModules = [javaSyntaxModule]
javaTestModules :: [Module Meta]
javaTestModules = [jsonJsonModule]


toFileName :: Bool -> String -> GraphName -> String
toFileName caps ext (GraphName name) = L.intercalate "/" parts ++ ext
  where
    parts = (if caps then capitalize else id) <$> Strings.splitOn "/" name

writeGraph :: (Default m, Eq m, Ord m, Read m, Show m)
  => (Context m -> Graph m -> Qualified String)
  -> Context m -> Graph m -> Maybe FilePath -> IO ()
writeGraph serialize cx g path = do
  case serialize cx g of
    Qualified Nothing warnings -> putStrLn $ "Transformation failed in " ++ h (graphName g) ++ ": " ++ indent (unlines warnings)
      where
        h (GraphName n) = n
    Qualified (Just s) warnings -> do
      if not (L.null warnings)
        then putStrLn $ "Warnings: " ++ indent (unlines warnings) ++ "\n"
        else pure ()
      case path of
        Nothing -> putStrLn s
        Just p -> do
          SD.createDirectoryIfMissing True $ FP.takeDirectory p
          writeFile p s
