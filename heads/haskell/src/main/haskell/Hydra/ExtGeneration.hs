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
import Hydra.Wasm.Coder (moduleToWasm)
import Hydra.Wasm.Language (wasmLanguage)
import Hydra.Lisp.Coder (moduleToLisp)
import Hydra.Lisp.Language (lispLanguage)
import Hydra.Lisp.Serde (programToExpr)
import qualified Hydra.Lisp.Syntax as LispSyntax
import qualified Hydra.Serialization as Serialization
import qualified Hydra.Names as Names
import qualified Hydra.Util as Util
import Hydra.Scala.Coder (moduleToScala)
import Hydra.Coq.GenerateDriver (moduleToCoq)
import Hydra.Coq.Generate (globalFieldMapping, globalConstructorCounts, globalAmbiguousNames, globalSanitizedAccessors)
import Hydra.Coq.Language (coqLanguage)

import Control.Monad (when)
import qualified Data.List as L
import qualified Data.Map as M
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified System.IO as SIO


-- | Options for JSON Schema code generation (was previously in Staging module)
data JsonSchemaOptions = JsonSchemaOptions {
  jsonSchemaOptionsShortNames :: Bool
}

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
--
-- After generation, walks the output directory and wraps long lines in each
-- generated .scala file via 'wrapLongScalaLines'. This avoids the Scala
-- compiler's memory issues on extremely long single-line expressions, and
-- replaces the previously-external break-long-lines.py post-processor.
writeScala :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeScala basePath universeMods mods = do
  n <- generateSources moduleToScala scalaLanguage True True False False basePath universeMods mods
  wrapLongLinesInScalaTree basePath
  return n

-- | Generate WebAssembly text format (WAT) files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeWasm :: FP.FilePath -> [Module] -> [Module] -> IO Int
writeWasm = generateSources moduleToWasm wasmLanguage True False False False

-- | Soft maximum line length for generated source files in any target
--   language. Lines longer than this are broken at the first eligible break
--   point; segments below this length are left intact even if they could be
--   broken further. The target is readability first; the secondary goal is
--   to keep individual lines short enough that downstream compilers do not
--   blow their stacks on a single megaline.
maxLineLength :: Int
maxLineLength = 120

-- | Once the current segment exceeds this length, the next ',' is a break
--   point. Smaller than 'maxLineLength' so that the post-break continuation
--   has room before the next forced wrap.
commaBreakThreshold :: Int
commaBreakThreshold = 80

-- | Once the current segment exceeds this length, '=>' immediately after
--   a ')' becomes a break point.
arrowBreakThreshold :: Int
arrowBreakThreshold = 60

-- | Walk a directory tree and wrap long lines in every .scala file.
wrapLongLinesInScalaTree :: FP.FilePath -> IO ()
wrapLongLinesInScalaTree dir = do
    exists <- SD.doesDirectoryExist dir
    when exists $ do
      entries <- SD.listDirectory dir
      mapM_ visit entries
  where
    visit name = do
      let path = dir FP.</> name
      isDir <- SD.doesDirectoryExist path
      if isDir
        then wrapLongLinesInScalaTree path
        else when (FP.takeExtension path == ".scala") $ do
          -- Read strictly to release the file handle before writing back.
          contents <- SIO.withFile path SIO.ReadMode $ \h -> do
            cs <- SIO.hGetContents h
            length cs `seq` return cs
          let wrapped = wrapLongScalaText contents
          when (wrapped /= contents) (writeFile path wrapped)

-- | Apply line-wrapping to every line in a Scala source file. Lines under
--   the max length pass through unchanged; long lines are broken at safe
--   points (commas outside string literals, when the segment is long).
wrapLongScalaText :: String -> String
wrapLongScalaText = unlines . fmap wrapLongScalaLine . lines

-- | Break a single long line at safe break points. If the line is short
--   enough, returns it unchanged. Otherwise walks character-by-character,
--   tracking string-literal state, and breaks at:
--     * a ',' after the current segment exceeds 'commaBreakThreshold' chars
--     * '=>' immediately after ')' (lambda body), after the current segment
--       exceeds 'arrowBreakThreshold' chars
wrapLongScalaLine :: String -> String
wrapLongScalaLine line
  | length line <= maxLineLength = line
  | otherwise =
      let indent = takeWhile (== ' ') line
          commaCont = indent ++ "  "
          arrowCont = indent ++ "    "
          (segments, lastSeg) = scan line "" [] False '\NUL' commaCont arrowCont
          allSegments = if null lastSeg then segments else segments ++ [lastSeg]
      in if length allSegments <= 1
           then line
           else L.intercalate "\n" allSegments
  where
    scan :: String -> String -> [String] -> Bool -> Char -> String -> String -> ([String], String)
    scan [] cur acc _ _ _ _ = (acc, cur)
    scan (c:rest) cur acc inString prevChar commaCont arrowCont =
      let cur' = cur ++ [c]
          inString' = if c == '"' && prevChar /= '\\'
                        then not inString
                        else inString
          isArrowStart = not inString' && c == '=' && case rest of
                           ('>':_) -> True
                           _       -> False
          arrowAfterParen = isArrowStart && prevChar == ')' && length cur' > arrowBreakThreshold
          shouldBreakComma = c == ',' && not inString' && length cur' > commaBreakThreshold
      in if arrowAfterParen
           then -- consume '>', emit segment "...=>", continue with arrow indent
                let cur'' = cur' ++ ['>']
                    rest' = drop 1 rest
                in scan rest' arrowCont (acc ++ [cur'']) inString' '>' commaCont arrowCont
           else if shouldBreakComma
             then scan rest commaCont (acc ++ [cur']) inString' c commaCont arrowCont
             else scan rest cur' acc inString' c commaCont arrowCont
