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
import Hydra.TypeScript.Coder (moduleToTypeScript)
import Hydra.TypeScript.Language (typeScriptLanguage)
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
import qualified Data.Set as Set
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified System.IO as SIO


-- | Generate C++ source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCpp :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeCpp = generateSources moduleToCpp cppLanguage True

-- | Generate GraphQL source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeGraphql :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeGraphql = generateSources moduleToGraphql graphqlLanguage True

-- | Generate GraphQL source files without type adaptation.
-- Useful when the source types include constructs (like forall) that the adapter doesn't handle,
-- but the coder can encode directly.
writeGraphqlRaw :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeGraphqlRaw = generateSources moduleToGraphql graphqlLanguage False

-- | Generate Java source files from modules.
-- Emission flags (eta-expansion, case-hoisting, polymorphic-let-hoisting)
-- come from javaLanguage's supportedFeatures.
writeJava :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeJava = generateSources moduleToJava javaLanguage True

-- | Generate JSON Schema files from modules.
writeJsonSchema :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeJsonSchema = generateSources moduleToJsonSchema jsonSchemaLanguage True

-- | Generate PDL (Pegasus) source files from modules.
writePdl :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writePdl = generateSources moduleToPdl pdlLanguage True

-- | Generate Protocol Buffers source files from modules.
writeProtobuf :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeProtobuf = generateSources moduleToProtobuf protobufLanguage True

-- | Generate Python source files from modules.
-- Emission flags come from pythonLanguage's supportedFeatures.
writePython :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writePython = generateSources moduleToPython pythonLanguage True

-- | Generate Rust source files from modules.
writeRust :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeRust = generateSources moduleToRust rustLanguage True

-- | Generate TypeScript source files from modules.
-- Emission flags come from typeScriptLanguage's supportedFeatures.
writeTypeScript :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeTypeScript = generateSources moduleToTypeScript typeScriptLanguage False

-- | Generate Coq (.v) source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeCoq :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeCoq basePath universeModules modulesToGenerate =
  let allMods = universeModules ++ modulesToGenerate
      fm = globalFieldMapping allMods
      cc = globalConstructorCounts allMods
      -- Hydra primitive library functions are not Hydra modules — they are
      -- implemented host-side (in this case as Coq Definitions/Axioms in
      -- `hydra/lib/*.v`) and are therefore invisible to `globalAmbiguousNames`.
      -- Collisions like `hydra.lib.eithers.either` vs `hydra.show.core.either`
      -- only manifest at Coq's import level. Inject the lib primitive names
      -- into the ambiguous set so that cross-module references in the
      -- non-lib source (`hydra.show.core.maybe`) stay fully qualified.
      an = Set.union (globalAmbiguousNames allMods) coqLibPrimitiveNames
      sa = globalSanitizedAccessors allMods
  in generateSources (moduleToCoq fm cc an sa) coqLanguage True basePath universeModules modulesToGenerate

-- | Names of Hydra primitive library functions exported by `hydra.lib.*`
-- Coq modules. These are invisible to `globalAmbiguousNames` (which walks
-- Hydra module definitions) because the lib modules are implemented
-- host-side in hand-written Coq. Used to disambiguate references like
-- `hydra.show.core.maybe` that would otherwise collide with bare imports
-- of the lib modules.
coqLibPrimitiveNames :: Set.Set String
coqLibPrimitiveNames = Set.fromList [
  "abs","acos","acosh","add","addFloat64","alter","and","apply","asin",
  "asinh","at","atan","atan2","atanh","bigintToDecimal","bigintToInt16",
  "bigintToInt32","bigintToInt64","bigintToInt8","bigintToUint16",
  "bigintToUint32","bigintToUint64","bigintToUint8","bimap","binaryToBytes",
  "binaryToString","bind","cases","cat","cat2","ceiling","charAt","compare",
  "compose","concat","concat2","cons","contains","cos","cosh",
  "decimalToBigint","decimalToFloat32","decimalToFloat64","delete",
  "difference","div","drop","dropWhile","e","either","elem","elems","empty",
  "equal","even","exp","filter","filterWithKey","find","findAll",
  "findWithDefault","first","float32ToDecimal","float32ToFloat64",
  "float64ToDecimal","float64ToFloat32","floor","foldl","foldr","fromJust",
  "fromLeft","fromList","fromOptional","fromRight","group","gt","gte","head",
  "identity","ifElse","init","insert","int16ToBigint","int32ToBigint",
  "int64ToBigint","int8ToBigint","intercalate","intersection","intersperse",
  "isAlphaNum","isGiven","isLeft","isLower","isNone","isRight","isSpace",
  "isUpper","keys","last","lefts","length","lines","log","logBase","lookup",
  "lt","lte","map","mapKeys","mapList","mapOptional","mapSet","matches","max",
  "maybe","maybeAt","maybeCharAt","maybeDiv","maybeHead","maybeInit",
  "maybeLast","maybeMod","maybePred","maybeRem","maybeSucc","maybeTail",
  "member","min","mod","mul","mulFloat64","negate","negateFloat64","not",
  "nub","null","odd","or","partition","partitionEithers","pi","pow","pred",
  "pure","range","readBigint","readBoolean","readDecimal",
  "readFloat32","readFloat64","readInt16","readInt32","readInt64","readInt8",
  "readString","readUint16","readUint32","readUint64","readUint8","rem",
  "replace","replaceAll","replicate","reverse","rights","round",
  "roundFloat32","roundFloat64","safeHead","second",
  "showBigint","showBoolean","showDecimal","showFloat32",
  "showFloat64","showInt16","showInt32","showInt64","showInt8","showString",
  "showUint16","showUint32","showUint64","showUint8","signum","sin",
  "singleton","sinh","size","sort","sortOn","span","split","splitOn","sqrt",
  "stringToBinary","sub","subFloat64","succ","tail","take","tan","tanh",
  "toList","toLower","toUpper","transpose","truncate","uint16ToBigint",
  "uint32ToBigint","uint64ToBigint","uint8ToBigint","union","unions",
  "unlines","zip","zipWith"]

-- | Wrap moduleToLisp for a specific dialect
moduleToLispDialect
  :: LispSyntax.Dialect -> String
  -> Module -> [Definition] -> InferenceContext -> Graph
  -> Either Error (M.Map FilePath String)
moduleToLispDialect dialect ext mod defs cx g =
  case moduleToLisp dialect mod defs cx g of
    Left err -> Left err
    Right program ->
      let code = Serialization.printExpr (Serialization.parenthesize (programToExpr program))
          caseConvention = case dialect of
            LispSyntax.DialectClojure -> Util.CaseConventionCamel
            _ -> Util.CaseConventionLowerSnake
          filePath = Names.moduleNameToFilePath caseConvention (FileExtension ext) (moduleName mod)
      in Right (M.singleton filePath code)

writeClojure :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeClojure = generateSources (moduleToLispDialect LispSyntax.DialectClojure "clj") lispLanguage True

writeScheme :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeScheme = generateSources (moduleToLispDialect LispSyntax.DialectScheme "scm") lispLanguage True

writeCommonLisp :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeCommonLisp = generateSources (moduleToLispDialect LispSyntax.DialectCommonLisp "lisp") lispLanguage True

writeEmacsLisp :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeEmacsLisp = generateSources (moduleToLispDialect LispSyntax.DialectEmacsLisp "el") lispLanguage True

-- | Generate Scala source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
--
-- The Scala compiler hits stack/memory limits on extremely long
-- single-line expressions; 'wrapLongScalaText' breaks long lines at safe
-- points (commas / arrow-after-paren outside string literals). Applied
-- as part of the generation pipeline (via the per-file content
-- transform in 'generateSourcesWithTransform'), not as a read-back
-- post-pass on disk.
writeScala :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeScala = generateSourcesWithTransform wrapLongScalaText
  moduleToScala scalaLanguage True

-- | Generate WebAssembly text format (WAT) files from modules.
writeWasm :: FP.FilePath -> [Module] -> [Module] -> IO [FilePath]
writeWasm = generateSources moduleToWasm wasmLanguage True

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

-- (The previous wrapLongLinesInScalaTree post-pass — which walked
-- the output directory and rewrote each .scala file in place — has
-- been retired. wrapLongScalaText is now applied during emission via
-- generateSourcesWithTransform; see writeScala above and the scala
-- dispatch in heads/haskell/src/exec/bootstrap-from-json/Main.hs.)

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
          -- Drop a whitespace-only trailing segment: when scan breaks at the
          -- final comma in the input, the residual `cur` is just `commaCont`
          -- (the indent prefix) with no real content. Emitting it adds a
          -- blank-but-indented line at the end, which the post-strip used to
          -- silently fix; with the strip removed, we filter here at source.
          isWhitespaceOnly = all (== ' ')
          allSegments = if null lastSeg || isWhitespaceOnly lastSeg
                          then segments
                          else segments ++ [lastSeg]
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
