-- | Entry point for Hydra code generation utilities

module Hydra.Codegen (
  allModules,
  kernelModules,
  mainModules,
  modulesToGraph,
  testModules,
  tier0Modules,
  tier1Modules,
  tier2Modules,
  tier3Modules,
  tier4LangModules,
  tier4Modules,
  tier4TestModules,
  writeGraphql,
  writeHaskell,
  writeJava,
  writePdl,
  writeScala,
  writeYaml,
) where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import qualified Hydra.Langs.Graphql.Coder as Graphql
import qualified Hydra.Langs.Haskell.Coder as Haskell
import qualified Hydra.Langs.Java.Coder as Java
import qualified Hydra.Langs.Json.Coder as Json
import qualified Hydra.Langs.Pegasus.Coder as PDL
import qualified Hydra.Langs.Scala.Coder as Scala
import qualified Hydra.Langs.Yaml.Modules as Yaml
import Hydra.Sources.Libraries

import Hydra.Sources.Tier0.Ast
import Hydra.Sources.Tier0.Coders
import Hydra.Sources.Tier0.Compute
import Hydra.Sources.Tier0.Constraints
import Hydra.Sources.Core
import Hydra.Sources.Tier0.Grammar
import Hydra.Sources.Tier0.Graph
import Hydra.Sources.Tier0.Langs.Json.Model
import Hydra.Sources.Tier0.Mantle
import Hydra.Sources.Tier0.Module
import Hydra.Sources.Tier0.Phantoms
import Hydra.Sources.Tier0.Query
import Hydra.Sources.Tier0.Testing
import Hydra.Sources.Tier0.Workflow

import Hydra.Sources.Tier1.Constants
import Hydra.Sources.Tier1.CoreEncoding
import Hydra.Sources.Tier1.Strip
import Hydra.Sources.Tier1.Tier1

import Hydra.Sources.Tier2.Basics
import Hydra.Sources.Tier2.CoreLanguage
import Hydra.Sources.Tier2.Extras
import Hydra.Sources.Tier2.Printing
import Hydra.Sources.Tier2.Tier2

import Hydra.Sources.Tier3.Tier3

import Hydra.Sources.Tier4.Test.TestSuite

import Hydra.Sources.Tier4.Langs.Avro.Schema
import Hydra.Sources.Tier4.Langs.Graphql.Syntax
import Hydra.Sources.Tier4.Langs.Haskell.Ast
import Hydra.Sources.Tier4.Langs.Java.Language
import Hydra.Sources.Tier4.Langs.Java.Syntax
import Hydra.Sources.Tier4.Langs.Owl.Syntax
import Hydra.Sources.Tier4.Langs.Parquet.Format
import Hydra.Sources.Tier4.Langs.Pegasus.Pdl
import Hydra.Sources.Tier4.Langs.Protobuf.Any
import Hydra.Sources.Tier4.Langs.Protobuf.SourceContext
import Hydra.Sources.Tier4.Langs.Protobuf.Type
import Hydra.Sources.Tier4.Langs.Rdf.Syntax
import Hydra.Sources.Tier4.Langs.RelationalModel
import Hydra.Sources.Tier4.Langs.Scala.Meta
import Hydra.Sources.Tier4.Langs.Shacl.Model
import Hydra.Sources.Tier4.Langs.Shex.Syntax
import Hydra.Sources.Tier4.Langs.Sql.Ansi
import Hydra.Sources.Tier4.Langs.Tabular
import Hydra.Sources.Tier4.Langs.Tinkerpop.Features
import Hydra.Sources.Tier4.Langs.Tinkerpop.Mappings
import Hydra.Sources.Tier4.Langs.Tinkerpop.PropertyGraph
import Hydra.Sources.Tier4.Langs.Xml.Schema
import Hydra.Sources.Tier4.Langs.Yaml.Model

import qualified Control.Monad as CM
import qualified System.FilePath as FP
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified Data.Maybe as Y


allModules :: [Module Kv]
allModules = mainModules ++ testModules

findType :: Graph a -> Term a -> Flow (Graph a) (Maybe (Type a))
findType cx term = annotationClassTermType (graphAnnotations cx) term

generateSources :: (Module Kv -> Flow (Graph Kv) (M.Map FilePath String)) -> FilePath -> [Module Kv] -> IO ()
generateSources printModule basePath mods = do
    mfiles <- runFlow kernelSchema generateFiles
    case mfiles of
      Nothing -> fail "Transformation failed"
      Just files -> mapM_ writePair files
  where
    generateFiles = do
      withTrace "generate files" $ do
        withState (modulesToGraph mods) $ do
          g' <- inferGraphTypes
          withState g' $ do
              maps <- CM.mapM forModule $ refreshModule (graphElements g') <$> mods
              return $ L.concat (M.toList <$> maps)
            where
              refreshModule els mod = mod {
                moduleElements = Y.catMaybes ((\e -> M.lookup (elementName e) els) <$> moduleElements mod)}

    writePair (path, s) = do
      let fullPath = FP.combine basePath path
      SD.createDirectoryIfMissing True $ FP.takeDirectory fullPath
      writeFile fullPath s

    forModule mod = withTrace ("module " ++ unNamespace (moduleNamespace mod)) $ printModule mod

kernelModules :: [Module Kv]
kernelModules = tier0Modules ++ tier1Modules ++ tier2Modules ++ tier3Modules

-- Note: currently a subset of the kernel which is used as a schema graph.
--       The other modules are not yet needed in the runtime environment
kernelSchema :: Graph Kv
kernelSchema = elementsToGraph bootstrapGraph Nothing $ L.concatMap moduleElements [
  hydraCodersModule,
  hydraComputeModule,
  hydraCoreModule,
  hydraGraphModule,
  hydraMantleModule,
  hydraModuleModule,
  hydraTestingModule]

mainModules :: [Module Kv]
mainModules = kernelModules ++ tier4LangModules

modulesToGraph :: [Module Kv] -> Graph Kv
modulesToGraph mods = elementsToGraph kernelSchema (Just kernelSchema) elements
  where
    elements = L.concat (moduleElements <$> modules)
    modules = L.concat (close <$> mods)
      where
        close mod = mod:(L.concat (close <$> moduleDependencies mod))

printTrace :: Bool -> Trace -> IO ()
printTrace isError t = do
  CM.unless (L.null $ traceMessages t) $ do
      putStrLn $ if isError then "Flow failed. Messages:" else "Messages:"
      putStrLn $ indentLines $ traceSummary t

runFlow :: s -> Flow s a -> IO (Maybe a)
runFlow cx f = do
  let FlowState v _ t = unFlow f cx emptyTrace
  printTrace (Y.isNothing v) t
  return v

testModules :: [Module Kv]
testModules = tier4TestModules

tier0Modules :: [Module Kv]
tier0Modules = [
  hydraAstModule,
  hydraCodersModule,
  hydraComputeModule,
  hydraConstantsModule,
  hydraConstraintsModule,
  hydraCoreModule,
  hydraGrammarModule,
  hydraGraphModule,
  hydraMantleModule,
  hydraModuleModule,
  hydraPhantomsModule,
  hydraQueryModule,
  hydraStripModule,
  hydraTestingModule,
  hydraWorkflowModule,
  jsonModelModule] -- JSON module is part of the kernel, despite being an external language; JSON support is built in to Hydra

tier1Modules :: [Module Kv]
tier1Modules = [
  coreEncodingModule,
  hydraCoreLanguageModule,
  hydraTier1Module]

tier2Modules :: [Module Kv]
tier2Modules = [
  hydraBasicsModule,
  hydraExtrasModule,
--  hydraMonadsModule,
  hydraPrintingModule,
  hydraTier2Module]

tier3Modules :: [Module Kv]
tier3Modules = [
  hydraTier3Module]

tier4Modules :: [Module Kv]
tier4Modules = tier4LangModules ++ tier4TestModules

tier4LangModules :: [Module Kv]
tier4LangModules = [
  avroSchemaModule,
  graphqlSyntaxModule,
  haskellAstModule,
  javaLanguageModule,
  javaSyntaxModule,
  owlSyntaxModule,
  parquetFormatModule,
  pegasusPdlModule,
  protobufAnyModule,
  protobufSourceContextModule,
  protobufTypeModule,
  rdfSyntaxModule,
  relationalModelModule,
  scalaMetaModule,
  shaclModelModule,
  shexSyntaxModule,
  sqlModule,
  tabularModule,
  tinkerpopFeaturesModule,
  tinkerpopMappingsModule,
  tinkerpopPropertyGraphModule,
  xmlSchemaModule,
  yamlModelModule]

tier4TestModules :: [Module Kv]
tier4TestModules = [
  testSuiteModule]

writeGraphql :: FP.FilePath -> [Module Kv] -> IO ()
writeGraphql = generateSources Graphql.printModule

writeHaskell :: FilePath -> [Module Kv] -> IO ()
writeHaskell = generateSources Haskell.printModule

writeJava :: FP.FilePath -> [Module Kv] -> IO ()
writeJava = generateSources Java.printModule

-- writeJson :: FP.FilePath -> [Module Kv] -> IO ()
-- writeJson = generateSources Json.printModule

writePdl :: FP.FilePath -> [Module Kv] -> IO ()
writePdl = generateSources PDL.printModule

writeScala :: FP.FilePath -> [Module Kv] -> IO ()
writeScala = generateSources Scala.printModule

writeYaml :: FP.FilePath -> [Module Kv] -> IO ()
writeYaml = generateSources Yaml.printModule
