import Hydra.Ext.Generation
import qualified Hydra.Ext.Sources.Avro.Coder as AvroCoder
import qualified Hydra.Ext.Sources.Avro.SchemaJson as AvroSchemaJson
import qualified Hydra.Ext.Sources.Cpp.Coder as CppCoder
import qualified Hydra.Ext.Sources.Cpp.Environment as CppEnvironment
import qualified Hydra.Ext.Sources.Cpp.Names as CppNames
import qualified Hydra.Ext.Sources.Cpp.Serde as CppSerde
import qualified Hydra.Ext.Sources.Cpp.Utils as CppUtils
import qualified Hydra.Ext.Sources.Graphql.Coder as GraphqlCoder
import qualified Hydra.Ext.Sources.Graphviz.Coder as GraphvizCoder
import qualified Hydra.Ext.Sources.Java.TestCodec as JavaTestCodec
import qualified Hydra.Ext.Sources.Java.Coder as JavaCoder
import qualified Hydra.Ext.Sources.Json.Schema.Coder as JsonSchemaCoder
import qualified Hydra.Ext.Sources.Json.Schema.Serde as JsonSchemaSerde
import qualified Hydra.Ext.Sources.Pegasus.Coder as PegasusCoder
import qualified Hydra.Ext.Sources.Pg.Coder as PgCoder
import qualified Hydra.Ext.Sources.Pg.Printing as PgPrinting
import qualified Hydra.Ext.Sources.Pg.TermsToElements as PgTermsToElements
import qualified Hydra.Ext.Sources.Pg.Utils as PgUtils
import qualified Hydra.Ext.Sources.Protobuf.Coder as ProtobufCoder
import qualified Hydra.Ext.Sources.Protobuf.Serde as ProtobufSerde
import qualified Hydra.Ext.Sources.Python.TestCodec as PythonTestCodec
import qualified Hydra.Ext.Sources.Rdf.Serde as RdfSerde
import qualified Hydra.Ext.Sources.Rdf.Utils as RdfUtils
import qualified Hydra.Ext.Sources.Scala.Coder as ScalaCoder
import qualified Hydra.Ext.Sources.Scala.Prepare as ScalaPrepare
import qualified Hydra.Ext.Sources.Scala.Serde as ScalaSerde
import qualified Hydra.Ext.Sources.Scala.Utils as ScalaUtils
import qualified Hydra.Ext.Sources.Shacl.Coder as ShaclCoder
import qualified Hydra.Ext.Sources.Tinkerpop.Language as TinkerpopLanguage
-- Pre-existing modules for baseline comparison
import qualified Hydra.Ext.Sources.Python.Coder as ExistingPythonCoder
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let uni = mainModules ++ hydraExtModules
  case args of
    ["AvroCoder"] -> go uni AvroCoder.module_
    ["AvroSchemaJson"] -> go uni AvroSchemaJson.module_
    ["CppCoder"] -> go uni CppCoder.module_
    ["CppEnvironment"] -> go uni CppEnvironment.module_
    ["CppNames"] -> go uni CppNames.module_
    ["CppSerde"] -> go uni CppSerde.module_
    ["CppUtils"] -> go uni CppUtils.module_
    ["GraphqlCoder"] -> go uni GraphqlCoder.module_
    ["GraphvizCoder"] -> go uni GraphvizCoder.module_
    ["JavaTestCodec"] -> go uni JavaTestCodec.module_
    ["JavaCoder"] -> go uni JavaCoder.module_
    ["JsonSchemaCoder"] -> go uni JsonSchemaCoder.module_
    ["JsonSchemaSerde"] -> go uni JsonSchemaSerde.module_
    ["PegasusCoder"] -> go uni PegasusCoder.module_
    ["PgCoder"] -> go uni PgCoder.module_
    ["PgPrinting"] -> go uni PgPrinting.module_
    ["PgTermsToElements"] -> go uni PgTermsToElements.module_
    ["PgUtils"] -> go uni PgUtils.module_
    ["ProtobufCoder"] -> go uni ProtobufCoder.module_
    ["ProtobufSerde"] -> go uni ProtobufSerde.module_
    ["PythonTestCodec"] -> go uni PythonTestCodec.module_
    ["RdfSerde"] -> go uni RdfSerde.module_
    ["RdfUtils"] -> go uni RdfUtils.module_
    ["ScalaCoder"] -> go uni ScalaCoder.module_
    ["ScalaPrepare"] -> go uni ScalaPrepare.module_
    ["ScalaSerde"] -> go uni ScalaSerde.module_
    ["ScalaUtils"] -> go uni ScalaUtils.module_
    ["ShaclCoder"] -> go uni ShaclCoder.module_
    ["TinkerpopLanguage"] -> go uni TinkerpopLanguage.module_
    -- Pre-existing baseline tests
    ["ExistingPythonCoder"] -> go uni ExistingPythonCoder.module_
    _ -> putStrLn "Unknown module"
  where
    go uni m = do
      putStrLn "Generating..."
      writeHaskell "src/gen-main/haskell" uni [m]
      putStrLn "Done!"
