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
import Hydra.Impl.Haskell.Sources.Ext.Rdf.Model
import Hydra.Impl.Haskell.Sources.Graph
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Util.Codetree.Script
import qualified Hydra.Ext.Haskell.Coder as Haskell
import qualified Hydra.Ext.Java.Coder as Java
import qualified Hydra.Ext.Pegasus.Coder as PDL
import qualified Hydra.Ext.Scala.Coder as Scala

import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD


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
  pure xmlSchemaModule,
  pure rdfModelModule]

-- TODO: remove these eventually. They are handy for debugging.
singleModule :: [Result (Module Meta)]
singleModule = [pure hydraCoreModule, pure hydraAdapterModule, hydraBasicsModule]
testModules :: [Result (Module Meta)]
testModules = pure <$> [javaSyntaxModule, xmlSchemaModule, atlasModelModule, coqSyntaxModule]
javaTestModules :: [Result (Module Meta)]
javaTestModules = pure <$> [jsonJsonModule]

tmpModule = [adapterUtilsModule]

writeHaskell :: [Result (Module Meta)] -> FilePath -> IO ()
writeHaskell = generateSources Haskell.printGraph

writeJava :: [Result (Module Meta)] -> FP.FilePath -> IO ()
writeJava = generateSources Java.printGraph

writePdl :: [Result (Module Meta)] -> FP.FilePath -> IO ()
writePdl = generateSources PDL.printGraph

writeScala :: [Result (Module Meta)] -> FP.FilePath -> IO ()
writeScala = generateSources Scala.printGraph

generateSources ::
  (Context Meta -> Graph Meta -> Qualified (M.Map FilePath String)) -> [Result (Module Meta)] -> FilePath -> IO ()
generateSources printGraph modules basePath = case sequence modules of
    ResultFailure msg -> fail msg
    ResultSuccess mods -> mapM_ writeDataGraph mods
  where
    writeDataGraph mod@(Module g _) = writeGraph printGraph cx g basePath
      where
        cx = setContextElements allGraphs $ standardContext {
          contextGraphs = GraphSet allGraphsByName (graphName g),
          contextFunctions = M.fromList $ fmap (\p -> (primitiveFunctionName p, p)) standardPrimitives}
        allGraphs = moduleGraph <$> M.elems allModules
        allGraphsByName = M.fromList $ (\g -> (graphName g, g)) <$> allGraphs
        allModules = addModule (M.fromList [(hydraCoreName, hydraCoreModule)]) mod
          where
            addModule m mod@(Module g' deps) = if M.member gname m
                then m
                else L.foldl addModule (M.insert gname mod m) deps
              where
                gname = graphName g'

writeGraph :: (Context m -> Graph m -> Qualified (M.Map FilePath String)) -> Context m -> Graph m -> FilePath -> IO ()
writeGraph printGraph cx g basePath = do
  case printGraph cx g of
    Qualified Nothing warnings -> putStrLn $
      "Transformation failed in " ++ unGraphName (graphName g) ++ ": " ++ indent (unlines warnings)
    Qualified (Just m) warnings -> do
      if not (L.null warnings)
        then putStrLn $ "Warnings: " ++ indent (unlines warnings) ++ "\n"
        else pure ()
      mapM_ writePair $ M.toList m
  where
    writePair (path, s) = do
      let fullPath = FP.combine basePath path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath s
