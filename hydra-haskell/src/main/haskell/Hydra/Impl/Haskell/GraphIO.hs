module Hydra.Impl.Haskell.GraphIO where

import Hydra.Core
import Hydra.Errors
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Sources.Adapter
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Errors
import Hydra.Impl.Haskell.Sources.Evaluation
import Hydra.Impl.Haskell.Sources.Ext.Atlas.Model
import Hydra.Impl.Haskell.Sources.Ext.Coq.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Haskell.Ast
import Hydra.Impl.Haskell.Sources.Ext.Java.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Json.Json
import Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl
import Hydra.Impl.Haskell.Sources.Ext.Scala.Meta
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Features
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Typed
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3
import Hydra.Impl.Haskell.Sources.Ext.Xml.Schema
import Hydra.Impl.Haskell.Sources.Ext.Yaml.Model
import Hydra.Impl.Haskell.Sources.Graph
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Util.Codetree.Script
import Hydra.Util.Formatting
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Ext.Haskell.Coder as Haskell
import qualified Hydra.Ext.Java.Coder as Java
import qualified Hydra.Ext.Pegasus.Coder as PDL
import qualified Hydra.Ext.Scala.Coder as Scala

import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD


generateHaskell :: [Result (Module Meta)] -> FP.FilePath -> IO ()
generateHaskell = generateSources (toFileName True ".hs") Haskell.printGraph

generateJava :: [Result (Module Meta)] -> FP.FilePath -> IO ()
generateJava = generateSources (toFileName False ".java") Java.printGraph

generatePdl :: [Result (Module Meta)] -> FP.FilePath -> IO ()
generatePdl = generateSources (toFileName False ".pdl") PDL.printGraph

generateScala :: [Result (Module Meta)] -> FP.FilePath -> IO ()
generateScala = generateSources (toFileName False ".scala") Scala.printGraph

generateSources :: (GraphName -> FP.FilePath) -> (Context Meta -> Graph Meta -> Qualified String)
  -> [Result (Module Meta)] -> FP.FilePath -> IO ()
generateSources toFile serialize modules baseDir = case sequence modules of
    ResultFailure msg -> fail msg
    ResultSuccess mods -> mapM_ writeDataGraph mods
  where
    writeDataGraph (Module g deps) = do
      let cx = setContextElements (g:(moduleGraph <$> deps)) $ standardContext {
             contextGraphs = GraphSet (M.fromList [
               (graphName g, g),
               (hydraCoreName, hydraCore)]) (graphName g),
             contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives}
      writeGraph serialize cx g $ Just $ FP.combine baseDir $ toFile (graphName g)

coreModules :: [Result (Module Meta)]
coreModules = [
  pure hydraAdapterModule,
  pure hydraCoreModule,
  pure hydraErrorsModule,
  pure hydraEvaluationModule,
  pure hydraGraphModule,
  hydraBasicsModule,
  adapterUtilsModule,
  pure atlasModelModule,
  pure coqSyntaxModule,
  pure haskellAstModule,
  pure javaSyntaxModule,
  pure jsonJsonModule,
  pure pegasusPdlModule,
  pure scalaMetaModule,
  pure yamlModelModule,
  pure codetreeAstModule,
  pure tinkerpopFeaturesModule,
  pure tinkerpopTypedModule,
  pure tinkerpopV3Module,
  pure xmlSchemaModule]

-- TODO: remove these eventually. They are handy for debugging.
singleModule :: [Result (Module Meta)]
singleModule = [pure hydraCoreModule, pure hydraAdapterModule, hydraBasicsModule]
testModules :: [Result (Module Meta)]
testModules = pure <$> [javaSyntaxModule, xmlSchemaModule, atlasModelModule, coqSyntaxModule]
javaTestModules :: [Result (Module Meta)]
javaTestModules = pure <$> [jsonJsonModule]

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
