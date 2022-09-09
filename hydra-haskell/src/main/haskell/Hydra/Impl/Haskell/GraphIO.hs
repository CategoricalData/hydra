module Hydra.Impl.Haskell.GraphIO where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Monads
import Hydra.Util.Codetree.Script
import Hydra.Lexical

import qualified Hydra.Ext.Haskell.Coder as Haskell
import qualified Hydra.Ext.Java.Coder as Java
import qualified Hydra.Ext.Pegasus.Coder as PDL
import qualified Hydra.Ext.Scala.Coder as Scala

import Hydra.Impl.Haskell.Sources.Adapter
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Evaluation
import Hydra.Impl.Haskell.Sources.Phantoms
import Hydra.Impl.Haskell.Sources.Graph
import Hydra.Impl.Haskell.Sources.Grammar
import Hydra.Impl.Haskell.Sources.Libraries
--import Hydra.Impl.Haskell.Sources.Monads

import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Impl.Haskell.Sources.Ext.Coq.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Datalog.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Graphql.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Haskell.Ast
import Hydra.Impl.Haskell.Sources.Ext.Java.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Json.Model
import Hydra.Impl.Haskell.Sources.Ext.Pegasus.Pdl
import Hydra.Impl.Haskell.Sources.Ext.Owl.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Scala.Meta
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Features
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Typed
import Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3
import Hydra.Impl.Haskell.Sources.Ext.Xml.Schema
import Hydra.Impl.Haskell.Sources.Ext.Yaml.Model
import Hydra.Impl.Haskell.Sources.Ext.Rdf.Syntax
import Hydra.Impl.Haskell.Sources.Ext.Shacl.Model

import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


allModules :: [GraphFlow Meta (Module Meta)]
allModules = coreModules ++ extModules

coreModules :: [GraphFlow Meta (Module Meta)]
coreModules = [
  adapterUtilsModule,
  pure codetreeAstModule,
  pure haskellAstModule,
  pure hydraAdapterModule,
  hydraBasicsModule,
  pure hydraCoreModule,
  pure hydraEvaluationModule,
  pure hydraGraphModule,
  pure hydraGrammarModule,
--  pure hydraMonadsModule,
  pure hydraPhantomsModule,
  pure jsonModelModule]

extModules :: [GraphFlow Meta (Module Meta)]
extModules = [
  pure coqSyntaxModule,
  pure datalogSyntaxModule,
  pure graphqlSyntaxModule,
  pure javaSyntaxModule,
  pure pegasusPdlModule,
  pure owlSyntaxModule,
  pure rdfSyntaxModule,
  pure scalaMetaModule,
  pure shaclModelModule,
  pure tinkerpopFeaturesModule,
  pure tinkerpopTypedModule,
  pure tinkerpopV3Module,
  pure xmlSchemaModule,
  pure yamlModelModule]

writeHaskell :: [GraphFlow Meta (Module Meta)] -> FilePath -> IO ()
writeHaskell = generateSources Haskell.printGraph

writeJava :: [GraphFlow Meta (Module Meta)] -> FP.FilePath -> IO ()
writeJava = generateSources Java.printGraph

writePdl :: [GraphFlow Meta (Module Meta)] -> FP.FilePath -> IO ()
writePdl = generateSources PDL.printGraph

writeScala :: [GraphFlow Meta (Module Meta)] -> FP.FilePath -> IO ()
writeScala = generateSources Scala.printGraph

generateSources :: (Graph Meta -> GraphFlow Meta (M.Map FilePath String)) -> [GraphFlow Meta (Module Meta)] -> FilePath -> IO ()
generateSources printGraph modules basePath = do
    mfiles <- runFlow coreContext generateFiles
    case mfiles of
      Nothing -> fail "Transformation failed"
      Just files -> mapM_ writePair files
  where
    generateFiles = do
      mods <- sequence modules
      maps <- mapM (moduleToFiles printGraph) mods
      return $ L.concat (M.toList <$> maps)

    writePair (path, s) = do
      let fullPath = FP.combine basePath path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath s

moduleToFiles :: (Graph Meta -> GraphFlow Meta (M.Map FilePath String)) -> Module Meta -> GraphFlow Meta (M.Map FilePath String)
moduleToFiles printGraph mod@(Module g _) = withState cx $ printGraph g
  where
    cx = setContextElements allGraphs $ coreContext {
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

printTrace :: Bool -> Trace -> IO ()
printTrace isError (Trace stack messages) = do
    if (isError && not (L.null stack))
      then putStrLn $ "Error trace: " ++ L.intercalate " > " (L.reverse stack)
      else pure ()
    if not (L.null warnings)
      then putStrLn $ "Warnings: " ++ indent (unlines $ L.nub warnings) ++ "\n"
      else pure ()
  where
    warnings = L.nub (L.head <$> messages)

runFlow :: s -> Flow s a -> IO (Maybe a)
runFlow cx f = do
  let FlowWrapper v _ t = unFlow f cx emptyTrace
  printTrace (Y.isNothing v) t
  return v
