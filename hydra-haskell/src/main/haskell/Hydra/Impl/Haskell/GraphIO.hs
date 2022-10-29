module Hydra.Impl.Haskell.GraphIO where

import Hydra.All
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.CoreEncoding
import Hydra.Types.Inference

import qualified Hydra.Ext.Haskell.Coder as Haskell
import qualified Hydra.Ext.Java.Coder as Java
import qualified Hydra.Ext.Pegasus.Coder as PDL
import qualified Hydra.Ext.Scala.Coder as Scala
import qualified Hydra.Ext.Yaml.Modules as Yaml

import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Compute
import Hydra.Impl.Haskell.Sources.Grammar
import Hydra.Impl.Haskell.Sources.Libraries
import Hydra.Impl.Haskell.Sources.Mantle
import Hydra.Impl.Haskell.Sources.Module
import Hydra.Impl.Haskell.Sources.Phantoms

import Hydra.Impl.Haskell.Sources.Util.Codetree.Ast
import Hydra.Impl.Haskell.Sources.Ext.Avro.Schema
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

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


addDeepTypeAnnotations :: (Ord m, Show m) => Module m -> GraphFlow m (Module m)
addDeepTypeAnnotations mod = do
    els <- CM.mapM annotateElementWithTypes $ moduleElements mod
    return $ mod {moduleElements = els}

allModules :: [Module Meta]
allModules = coreModules ++ extModules

assignSchemas :: (Ord m, Show m) => Bool -> Module m -> GraphFlow m (Module m)
assignSchemas doInfer mod = do
    cx <- getState
    els <- CM.mapM (annotate cx) $ moduleElements mod
    return $ mod {moduleElements = els}
  where
    annotate cx el = do
      typ <- findType cx (elementData el)
      case typ of
        Nothing -> if doInfer
          then do
            t <- typeSchemeType . snd <$> inferType (elementData el)
            return el {elementSchema = encodeType t}
          else return el
        Just typ -> return el {elementSchema = encodeType typ}

coreModules :: [Module Meta]
coreModules = [
  adapterUtilsModule,
  codetreeAstModule,
  haskellAstModule,
  hydraBasicsModule,
  hydraCoreModule,
  hydraComputeModule,
  hydraMantleModule,
  hydraModuleModule,
  hydraGrammarModule,
--  hydraMonadsModule,
  hydraPhantomsModule,
  jsonModelModule]

extModules :: [Module Meta]
extModules = [
  avroSchemaModule,
  coqSyntaxModule,
  datalogSyntaxModule,
  graphqlSyntaxModule,
  javaSyntaxModule,
  pegasusPdlModule,
  owlSyntaxModule,
  rdfSyntaxModule,
  scalaMetaModule,
  shaclModelModule,
  tinkerpopFeaturesModule,
  tinkerpopTypedModule,
  tinkerpopV3Module,
  xmlSchemaModule,
  yamlModelModule]

findType :: Context m -> Term m -> GraphFlow m (Maybe (Type m))
findType cx term = annotationClassTermType (contextAnnotations cx) term

generateSources :: (Module Meta -> GraphFlow Meta (M.Map FilePath String)) -> [Module Meta] -> FilePath -> IO ()
generateSources printModule mods0 basePath = do
    mfiles <- runFlow mantleContext generateFiles
    case mfiles of
      Nothing -> fail "Transformation failed"
      Just files -> mapM_ writePair files
  where
    generateFiles = do
      withTrace "generate files" $ do
        mods1 <- CM.mapM (assignSchemas False) mods0
        withState (modulesToContext mods1) $ do
          mods2 <- CM.mapM addDeepTypeAnnotations mods1
          maps <- CM.mapM printModule mods2
          return $ L.concat (M.toList <$> maps)

    writePair (path, s) = do
      let fullPath = FP.combine basePath path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath s

mantleContext = graphContext hydraMantle

modulesToContext :: [Module Meta] -> Context Meta
modulesToContext mods = mantleContext {contextGraph = elementsToGraph (Just hydraMantle) elements}
  where
    elements = L.concat (moduleElements <$> allModules)
    allModules = L.concat (close <$> mods)
      where
        close mod = mod:(L.concat (close <$> moduleDependencies mod))

printTrace :: Bool -> Trace -> IO ()
printTrace isError t = do
  if not (L.null $ traceMessages t)
    then do
      putStrLn $ if isError then "Flow failed. Messages:" else "Messages:"
      putStrLn $ indentLines $ traceSummary t
    else pure ()

runFlow :: s -> Flow s a -> IO (Maybe a)
runFlow cx f = do
  let FlowState v _ t = unFlow f cx emptyTrace
  printTrace (Y.isNothing v) t
  return v

writeHaskell :: [Module Meta] -> FilePath -> IO ()
writeHaskell = generateSources Haskell.printModule

writeJava :: [Module Meta] -> FP.FilePath -> IO ()
writeJava = generateSources Java.printModule

writePdl :: [Module Meta] -> FP.FilePath -> IO ()
writePdl = generateSources PDL.printModule

writeScala :: [Module Meta] -> FP.FilePath -> IO ()
writeScala = generateSources Scala.printModule

writeYaml :: [Module Meta] -> FP.FilePath -> IO ()
writeYaml = generateSources Yaml.printModule
