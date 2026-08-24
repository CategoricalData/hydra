-- | Typed codec for the on-disk digest formats (#512).
--
-- Bridges the build system's digest files to their hydra.build.format
-- specifications: each digest.json is read and written as the canonical JSON
-- encoding of Format.InputDigest / Format.OutputDigest, via the generated
-- encode/decode modules, replacing Hydra.Digest's hand-rolled regex codec.
--
-- Module placement (#622 pattern): this module — not Hydra.Digest — carries
-- the Hydra.Build.* imports, keeping that coupling isolated in one place.
--
-- Layering: DigestFormat sits BELOW Hydra.Generation (Generation may import
-- DigestFormat, never the reverse) and beside Hydra.Digest, whose hashing and
-- orphan-reconcile helpers remain the shared substrate. The small JSON-parse
-- helpers are duplicated from Generation for now; Generation drops its copies
-- when its digest call sites migrate (parallel-change, phase 2).

module Hydra.DigestFormat (
    FormatContext(..),
    mkFormatContext,
    defaultFormatContext,
    -- Typed file IO
    readInputDigestFile,
    writeInputDigestFile,
    readOutputDigestFile,
    writeOutputDigestFile,
    -- Value ⇄ JSON string (pure; exposed for tests)
    inputDigestToJsonString,
    inputDigestFromJsonString,
    outputDigestToJsonString,
    outputDigestFromJsonString,
    -- Legacy-struct bridges (parallel-change phase only)
    fromPerPackageDigest,
    toPerPackageDigest,
    fromDigestV2,
    toDigestV2,
    -- Legacy-shaped drop-in IO (typed on disk, legacy structs in memory;
    -- callers swap one name and keep their logic)
    readDigestMapFile,
    writeDigestMapFile,
    readPerPackageDigestFile,
    writePerPackageDigestFile,
    readDigestV2File,
    writeDigestV2File,
    -- JSON parse helpers (shared with Generation after phase 2)
    parseJsonFile,
    parseJsonString,
    aesonToHydra,
) where

import qualified Hydra.Digest as Digest
import Hydra.Packaging (Module, ModuleName(..), PackageName(..), Version(..))
import Hydra.Overlay.Haskell.Bootstrap (bootstrapGraph)
import Hydra.Sources.Kernel.Types.All (kernelTypesModules)
import qualified Hydra.Sources.Build.Format as SourcesBuildFormat

import qualified Hydra.Build.Format as Format
import qualified Hydra.Decode.Build.Format as DecodeFormat
import qualified Hydra.Encode.Build.Format as EncodeFormat
import qualified Hydra.Codegen as Codegen
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Decode as JsonDecode
import qualified Hydra.Json.Encode as JsonEncode
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Writer as JsonWriter

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Directory as SD
import qualified System.FilePath as FP

-- | The schema context the generated decoders require: a graph carrying the
-- hydra.build.format type definitions plus the schema map derived from it.
-- Built once per process from the format module's DSL source (callers pass
-- Hydra.Sources.Build.Format.module_ and its dependency closure); never
-- rebuilt per digest file — digest-check runs per (package, source set).
data FormatContext = FormatContext
  { fcGraph :: Graph.Graph
  , fcSchemaMap :: M.Map Core.Name Core.Type
  }

-- | Build the codec context from the format module and its dependency
-- closure (hydra.build.format + hydra.packaging reach the whole schema).
mkFormatContext :: Graph.Graph -> [Module] -> FormatContext
mkFormatContext bsGraph mods = FormatContext g (Codegen.buildSchemaMap g)
  where
    g = Codegen.modulesToGraph bsGraph mods mods

-- | The shared codec context: the kernel type modules plus the format
-- module itself. A CAF — the schema graph is built once, on first use,
-- and shared by every digest read/write in the process. This is why the
-- Sources imports above live HERE rather than in callers.
defaultFormatContext :: FormatContext
defaultFormatContext = mkFormatContext bootstrapGraph
  (kernelTypesModules ++ [SourcesBuildFormat.module_])

inputDigestName :: Core.Name
inputDigestName = Core.Name "hydra.build.format.InputDigest"

outputDigestName :: Core.Name
outputDigestName = Core.Name "hydra.build.format.OutputDigest"

----------------------------------------------------------------------
-- Pure value ⇄ JSON-string codec

-- compactMaps = False: digest.json files are compared byte-for-byte across builds/commits for
-- change detection, so flipping to the #624 compact-map form would read as a spurious digest
-- change even when the underlying data is identical. Enabling it is a deliberate follow-up
-- decision (not made here), not something to pick up incidentally via this bug fix.
typedToJsonString :: FormatContext -> Core.Name -> (a -> Core.Term) -> a -> Either String String
typedToJsonString ctx name encode value =
  JsonWriter.printJson
    <$> JsonEncode.toJson (fcSchemaMap ctx) False name (Core.TypeVariable name) (encode value)

typedFromJsonString
  :: FormatContext -> Core.Name
  -> (Graph.Graph -> Core.Term -> Either err a) -> (err -> String)
  -> Json.Value -> Either String a
typedFromJsonString ctx name decode showErr jsonVal = do
  term <- JsonDecode.fromJson (fcSchemaMap ctx) False name (Core.TypeVariable name) jsonVal
  either (Left . showErr) Right (decode (fcGraph ctx) term)

inputDigestToJsonString :: FormatContext -> Format.InputDigest -> Either String String
inputDigestToJsonString ctx = typedToJsonString ctx inputDigestName EncodeFormat.inputDigest

inputDigestFromJsonString :: FormatContext -> Json.Value -> Either String Format.InputDigest
inputDigestFromJsonString ctx =
  typedFromJsonString ctx inputDigestName DecodeFormat.inputDigest show

outputDigestToJsonString :: FormatContext -> Format.OutputDigest -> Either String String
outputDigestToJsonString ctx = typedToJsonString ctx outputDigestName EncodeFormat.outputDigest

outputDigestFromJsonString :: FormatContext -> Json.Value -> Either String Format.OutputDigest
outputDigestFromJsonString ctx =
  typedFromJsonString ctx outputDigestName DecodeFormat.outputDigest show

----------------------------------------------------------------------
-- File IO. Read failures return Left; freshness callers treat any Left as a
-- cache miss (an old-format or absent digest is stale by definition, never an
-- error). Writes fail loudly — a digest we cannot encode is a bug.

readTyped :: (FormatContext -> Json.Value -> Either String a) -> FormatContext -> FilePath -> IO (Either String a)
readTyped decode ctx path = do
  exists <- SD.doesFileExist path
  if not exists
    then return (Left ("digest file absent: " ++ path))
    else do
      parsed <- parseJsonFile path
      return (parsed >>= decode ctx)

writeTyped :: (FormatContext -> a -> Either String String) -> FormatContext -> FilePath -> a -> IO ()
writeTyped encode ctx path value = case encode ctx value of
  Left err -> error ("failed to encode digest for " ++ path ++ ": " ++ err)
  Right s -> do
    SD.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path s

readInputDigestFile :: FormatContext -> FilePath -> IO (Either String Format.InputDigest)
readInputDigestFile = readTyped inputDigestFromJsonString

writeInputDigestFile :: FormatContext -> FilePath -> Format.InputDigest -> IO ()
writeInputDigestFile = writeTyped inputDigestToJsonString

readOutputDigestFile :: FormatContext -> FilePath -> IO (Either String Format.OutputDigest)
readOutputDigestFile = readTyped outputDigestFromJsonString

writeOutputDigestFile :: FormatContext -> FilePath -> Format.OutputDigest -> IO ()
writeOutputDigestFile = writeTyped outputDigestToJsonString

----------------------------------------------------------------------
-- Legacy-shaped drop-in IO. Same shapes and tolerance semantics as the
-- Hydra.Digest functions they replace (missing/unreadable reads degrade to
-- empty — the caller's existing miss/refresh logic then fires), but the
-- on-disk bytes are the canonical typed encoding. Callers migrate by
-- swapping the function name; the legacy in-memory structs disappear with
-- phase 3.

readDigestMapFile :: FilePath -> IO Digest.DigestMap
readDigestMapFile path =
  either (const M.empty) (Digest.ppHashes . toPerPackageDigest)
    <$> readInputDigestFile defaultFormatContext path

writeDigestMapFile :: FilePath -> Digest.DigestMap -> IO ()
writeDigestMapFile path m = writeInputDigestFile defaultFormatContext path
  (fromPerPackageDigest (Digest.PerPackageDigest m "" M.empty))

readPerPackageDigestFile :: FilePath -> IO Digest.PerPackageDigest
readPerPackageDigestFile path =
  either (const Digest.emptyPerPackageDigest) toPerPackageDigest
    <$> readInputDigestFile defaultFormatContext path

writePerPackageDigestFile :: FilePath -> Digest.PerPackageDigest -> IO ()
writePerPackageDigestFile path =
  writeInputDigestFile defaultFormatContext path . fromPerPackageDigest

readDigestV2File :: FilePath -> IO Digest.Digest
readDigestV2File path =
  either (const Digest.emptyDigest) toDigestV2
    <$> readOutputDigestFile defaultFormatContext path

writeDigestV2File :: FilePath -> Digest.Digest -> IO ()
writeDigestV2File path =
  writeOutputDigestFile defaultFormatContext path . fromDigestV2

----------------------------------------------------------------------
-- Legacy-struct bridges. Field-for-field by design: the Format types were
-- lifted verbatim from Hydra.Digest's structs (#413 note). These let callers
-- migrate one at a time; they disappear with the old structs in phase 3.

-- The typed format's schema version. Distinct from the legacy files'
-- "digestFormatVersion": 1 — a version-2 digest is canonically encoded, so
-- old-format files fail decode and degrade to a cache miss, which is the
-- digest contract for any unreadable state.
digestFormatVersion :: Int
digestFormatVersion = 2

moduleFormatVersion :: Int
moduleFormatVersion = 1

hash :: String -> Format.Sha256Hash
hash = Format.Sha256Hash

fromPerPackageDigest :: Digest.PerPackageDigest -> Format.InputDigest
fromPerPackageDigest ppd = Format.InputDigest
  { Format.inputDigestDigestFormatVersion = digestFormatVersion
  , Format.inputDigestModuleFormatVersion = moduleFormatVersion
  , Format.inputDigestSelfHash =
      if null (Digest.ppSelfHash ppd) then Nothing else Just (hash (Digest.ppSelfHash ppd))
  , Format.inputDigestDependencyHashes =
      M.fromList [ (PackageName p, hash h) | (p, h) <- M.toList (Digest.ppDeps ppd) ]
  , Format.inputDigestModuleHashes =
      M.fromList [ (mn, hash h) | (mn, h) <- M.toList (Digest.ppHashes ppd) ]
  }

toPerPackageDigest :: Format.InputDigest -> Digest.PerPackageDigest
toPerPackageDigest d = Digest.PerPackageDigest
  { Digest.ppHashes =
      M.fromList [ (mn, Format.unSha256Hash h) | (mn, h) <- M.toList (Format.inputDigestModuleHashes d) ]
  , Digest.ppSelfHash = maybe "" Format.unSha256Hash (Format.inputDigestSelfHash d)
  , Digest.ppDeps =
      M.fromList [ (unPackageName p, Format.unSha256Hash h) | (p, h) <- M.toList (Format.inputDigestDependencyHashes d) ]
  }
  where unPackageName (PackageName s) = s

fromDigestKind :: Digest.DigestKind -> Format.DigestKind
fromDigestKind k = case k of
  Digest.KindDslSource -> Format.DigestKindDslSource
  Digest.KindJsonFile -> Format.DigestKindJsonFile
  Digest.KindTargetFile -> Format.DigestKindTargetFile
  Digest.KindRuntimeFile -> Format.DigestKindRuntimeFile
  Digest.KindOther -> Format.DigestKindOther

toDigestKind :: Format.DigestKind -> Digest.DigestKind
toDigestKind k = case k of
  Format.DigestKindDslSource -> Digest.KindDslSource
  Format.DigestKindJsonFile -> Digest.KindJsonFile
  Format.DigestKindTargetFile -> Digest.KindTargetFile
  Format.DigestKindRuntimeFile -> Digest.KindRuntimeFile
  Format.DigestKindOther -> Digest.KindOther

fromEntry :: Digest.DigestEntry -> Format.DigestEntry
fromEntry (Digest.DigestEntry k h) = Format.DigestEntry (fromDigestKind k) (hash h)

toEntry :: Format.DigestEntry -> Digest.DigestEntry
toEntry (Format.DigestEntry k h) = Digest.DigestEntry (toDigestKind k) (Format.unSha256Hash h)

fromGeneration :: Digest.Generation -> Format.Generation
fromGeneration g = Format.Generation
  { Format.generationGeneratorId = Digest.genGeneratorId g
  , Format.generationMode = case Digest.genMode g of
      Digest.ModePublished -> Format.GenerationModePublished
      Digest.ModeShim -> Format.GenerationModeShim
  , Format.generationHost = Format.LanguageName (Digest.genHost g)
  , Format.generationHydraVersion = Version <$> Digest.genHydraVersion g
  , Format.generationRevision = Digest.genRevision g
  , Format.generationTimestamp = Digest.genTimestamp g
  }

toGeneration :: Format.Generation -> Digest.Generation
toGeneration g = Digest.Generation
  { Digest.genGeneratorId = Format.generationGeneratorId g
  , Digest.genMode = case Format.generationMode g of
      Format.GenerationModePublished -> Digest.ModePublished
      Format.GenerationModeShim -> Digest.ModeShim
  , Digest.genHost = Format.unLanguageName (Format.generationHost g)
  , Digest.genHydraVersion = unVersion <$> Format.generationHydraVersion g
  , Digest.genRevision = Format.generationRevision g
  , Digest.genTimestamp = Format.generationTimestamp g
  }
  where unVersion (Version s) = s

fromDigestV2 :: Digest.Digest -> Format.OutputDigest
fromDigestV2 d = Format.OutputDigest
  { Format.outputDigestDigestFormatVersion = digestFormatVersion
  , Format.outputDigestModuleFormatVersion = moduleFormatVersion
  , Format.outputDigestGenerator = Digest.digestGenerator d
  , Format.outputDigestGeneration = fromGeneration (Digest.digestGeneration d)
  , Format.outputDigestSelfHash =
      if null (Digest.digestRecordedSelfHash d) then Nothing else Just (hash (Digest.digestRecordedSelfHash d))
  , Format.outputDigestDependencyHashes =
      M.fromList [ (PackageName p, hash h) | (p, h) <- M.toList (Digest.digestRecordedDeps d) ]
  , Format.outputDigestInputs = M.map fromEntry (Digest.digestInputs d)
  , Format.outputDigestOutputs = M.map fromEntry (Digest.digestOutputs d)
  }

toDigestV2 :: Format.OutputDigest -> Digest.Digest
toDigestV2 d = Digest.Digest
  { Digest.digestInputs = M.map toEntry (Format.outputDigestInputs d)
  , Digest.digestOutputs = M.map toEntry (Format.outputDigestOutputs d)
  , Digest.digestGenerator = Format.outputDigestGenerator d
  , Digest.digestGeneration = toGeneration (Format.outputDigestGeneration d)
  , Digest.digestRecordedSelfHash = maybe "" Format.unSha256Hash (Format.outputDigestSelfHash d)
  , Digest.digestRecordedDeps =
      M.fromList [ (unPackageName p, Format.unSha256Hash h) | (p, h) <- M.toList (Format.outputDigestDependencyHashes d) ]
  }
  where unPackageName (PackageName s) = s

----------------------------------------------------------------------
-- JSON parse helpers, duplicated from Hydra.Generation for layering (see
-- module comment); Generation's copies are removed in phase 2.

-- | Convert an Aeson JSON value to a Hydra JSON value.
aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject (mapPair <$> AKM.toList km)
    where
      mapPair (k, v') = (AK.toString k, aesonToHydra v')
  A.Array a -> Json.ValueArray (aesonToHydra <$> V.toList a)
  A.String t -> Json.ValueString (T.unpack t)
  A.Number s -> Json.ValueNumber s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

-- | Parse a JSON file using Aeson and convert to Hydra JSON. Control
-- characters are escaped first, matching Generation's reader.
-- The read is STRICT (then converted) so the handle closes before this
-- function returns: digest flows read and then immediately rewrite the same
-- path (e.g. finalizePerPackageDigests), and a lazy read's still-open handle
-- makes that rewrite fail with "resource busy".
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- BS.fromStrict <$> SBS.readFile fp
  return (parseJsonBytes content)

-- | Parse an in-memory JSON string (pure; the specs' round-trip half).
parseJsonString :: String -> Either String Json.Value
parseJsonString = parseJsonBytes . BS.pack . fmap (fromIntegral . fromEnum)

parseJsonBytes :: BS.ByteString -> Either String Json.Value
parseJsonBytes content =
  let escaped = BS.pack (fmap fromIntegral (Codegen.escapeControlCharsInJson (fmap fromIntegral (BS.unpack content))))
  in aesonToHydra <$> A.eitherDecode escaped
