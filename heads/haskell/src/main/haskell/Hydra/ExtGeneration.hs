-- | Entry point for code generation in hydra-ext; provides additional sources and coders not found in hydra-haskell.

module Hydra.ExtGeneration (
  module Hydra.ExtGeneration,
  module Hydra.Sources.Ext,
  module Hydra.Generation,
  module Hydra.Haskell.Generation,
  module Hydra.Sources.All,
) where

import Hydra.Kernel
import Hydra.Generation
import Hydra.Haskell.Generation
import Hydra.Sources.Ext
import Hydra.Sources.All

import Hydra.Cpp.Language (cppLanguage)
import Hydra.Java.Language
import Hydra.Json.Schema.Language (jsonSchemaLanguage)
import Hydra.Protobuf.Language (protobufLanguage)
import Hydra.Python.Language
import Hydra.Pegasus.Language (pdlLanguage)
import Hydra.Scala.Language (scalaLanguage)
import Hydra.Cpp.Coder (moduleToCpp)
import Hydra.Graphql.Coder (moduleToGraphql)
import Hydra.Graphql.Language (graphqlLanguage)
import Hydra.Java.Coder (moduleToJava)
import Hydra.Json.Schema.Coder
import Hydra.Pegasus.Coder (moduleToPdl)
import Hydra.Protobuf.Coder (moduleToProtobuf)
import Hydra.Python.Coder (moduleToPython)
import Hydra.Rust.Coder (moduleToRust)
import Hydra.Rust.Language (rustLanguage)
import Hydra.Lisp.Coder (moduleToLisp)
import Hydra.Lisp.Language (lispLanguage)
import Hydra.Lisp.Serde (programToExpr)
import qualified Hydra.Lisp.Syntax as LispSyntax
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import Hydra.Scala.Coder (moduleToScala)
import Hydra.Coq.Generate (moduleToCoq, globalFieldMapping, globalConstructorCounts, globalAmbiguousNames, globalSanitizedAccessors)
import Hydra.Coq.Language (coqLanguage)

import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter

import qualified Data.Map as M
import qualified System.FilePath as FP


-- | Options for JSON Schema code generation (was previously in Staging module)
data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

-- | Write a manifest.json listing ext module namespaces.
-- This mirrors writeManifestJson in Hydra.Generation but for hydra-ext module lists.
writeExtManifestJson :: FilePath -> IO ()
writeExtManifestJson basePath = do
    let jsonVal = Json.ValueObject $ M.fromList [
            ("hydraBootstrapCoderModules", namespacesJson hydraBootstrapCoderModules),
            ("hydraExtDemoModules", namespacesJson hydraExtDemoModules),
            ("hydraExtEssentialModules", namespacesJson hydraExtEssentialModules),
            ("hydraExtJavaModules", namespacesJson hydraExtJavaModules),  -- legacy alias
            ("hydraExtModules", namespacesJson hydraExtJsonModules)]
        jsonStr = JsonWriter.printJson jsonVal
        filePath = basePath FP.</> "manifest.json"
    writeFile filePath (jsonStr ++ "\n")
    putStrLn $ "Wrote manifest: " ++ filePath
  where
    namespacesJson mods = Json.ValueArray $ fmap (Json.ValueString . unNamespace . moduleNamespace) mods

-- | Generate C++ source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCpp :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeCpp = generateSources moduleToCpp cppLanguage True False False False

-- | Generate GraphQL source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeGraphql :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeGraphql = generateSources moduleToGraphql graphqlLanguage True False False False

-- | Generate GraphQL source files without type adaptation.
-- Useful when the source types include constructs (like forall) that the adapter doesn't handle,
-- but the coder can encode directly.
writeGraphqlRaw :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeGraphqlRaw = generateSources moduleToGraphql graphqlLanguage False False False False

-- | Generate Java source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Java uses doHoistPolymorphicLetBindings=True to hoist polymorphic let bindings to class level
writeJava :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeJava = generateSources moduleToJava javaLanguage True True False True

-- | Generate JSON Schema files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeJsonSchema :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeJsonSchema = generateSources (moduleToJsonSchema (JsonSchemaOptions True)) jsonSchemaLanguage True False False False

-- | Generate PDL (Pegasus) source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writePdl :: FP.FilePath -> [Module] -> [Module] -> IO Int
writePdl = generateSources moduleToPdl pdlLanguage True False False False

-- | Generate Protocol Buffers source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeProtobuf :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeProtobuf = generateSources moduleToProtobuf protobufLanguage True False False False

-- | Generate Python source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
-- Note: Python uses doHoistCaseStatements=True to hoist case statements to let bindings
writePython :: FP.FilePath -> [Module] -> [Module] -> IO Int
writePython = generateSources moduleToPython pythonLanguage True True True False

-- | Generate Rust source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeRust :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeRust = generateSources moduleToRust rustLanguage True False False False

-- | Generate Coq (.v) source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCoq :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeCoq basePath universeModules modulesToGenerate =
  let allMods = universeModules ++ modulesToGenerate
      fm = globalFieldMapping allMods
      cc = globalConstructorCounts allMods
      an = globalAmbiguousNames allMods
      sa = globalSanitizedAccessors allMods
  in generateSources (moduleToCoq fm cc an sa) coqLanguage True False False False basePath universeModules modulesToGenerate

-- | Wrap moduleToLisp for a specific dialect
moduleToLispDialect
  :: LispSyntax.Dialect -> String
  -> Module -> [Definition] -> Context -> Graph
  -> Either Error (M.Map FilePath String)
moduleToLispDialect dialect ext mod defs cx g =
  case moduleToLisp dialect mod defs cx g of
    Left err -> Left err
    Right program ->
      let code = Serialization.printExpr (Serialization.parenthesize (programToExpr program))
          caseConvention = case dialect of
            LispSyntax.DialectClojure -> Util.CaseConventionCamel
            _ -> Util.CaseConventionLowerSnake
          filePath = Names.namespaceToFilePath caseConvention (FileExtension ext) (moduleNamespace mod)
      in Right (M.singleton filePath code)

writeClojure :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeClojure = generateSources (moduleToLispDialect LispSyntax.DialectClojure "clj") lispLanguage True False False False

writeScheme :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeScheme = generateSources (moduleToLispDialect LispSyntax.DialectScheme "scm") lispLanguage True False False False

writeCommonLisp :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeCommonLisp = generateSources (moduleToLispDialect LispSyntax.DialectCommonLisp "lisp") lispLanguage True False False False

writeEmacsLisp :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeEmacsLisp = generateSources (moduleToLispDialect LispSyntax.DialectEmacsLisp "el") lispLanguage True False False False

-- | Generate Scala source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeScala :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeScala = generateSources moduleToScala scalaLanguage True True False False
