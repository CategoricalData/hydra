-- | Verification tool that decodes all kernel modules from JSON and compares with originals.
-- This is a standalone executable that does not run with regular tests (too slow).
--
-- This tests that: JSON file → parse → Term → Module equals the original Module

module Main where

import Hydra.Kernel
import Hydra.Packaging (_Module)
import Hydra.Sources.All (kernelModules)
import Hydra.Generation (modulesToGraph)
import Hydra.Codegen (namespaceToPath)
import qualified Hydra.Json.Model as Json
import qualified Hydra.Json.Decode as JsonDecode
import qualified Hydra.Decode.Packaging as DecodePackaging
import qualified Hydra.Decode.Core as DecodeCore
import qualified Hydra.Strip as Strip

import Control.Monad (forM, when)
import System.Exit (exitFailure, exitSuccess)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Key as AK
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Scientific as SC

flushPut :: String -> IO ()
flushPut s = putStrLn s >> hFlush stdout

-- | Convert Aeson JSON value to Hydra JSON value
aesonToHydra :: A.Value -> Json.Value
aesonToHydra v = case v of
  A.Object km -> Json.ValueObject $ M.fromList (mapPair <$> AKM.toList km)
    where
      mapPair (k, v') = (AK.toString k, aesonToHydra v')
  A.Array a -> Json.ValueArray (aesonToHydra <$> V.toList a)
  A.String t -> Json.ValueString $ T.unpack t
  A.Number s -> Json.ValueNumber s
  A.Bool b -> Json.ValueBoolean b
  A.Null -> Json.ValueNull

-- | Parse JSON from file using Aeson (fast)
parseJsonFile :: FilePath -> IO (Either String Json.Value)
parseJsonFile fp = do
  content <- BS.readFile fp
  return $ aesonToHydra <$> A.eitherDecode content

-- | Build a schema map (Name -> Type) from a graph's schema types.
-- This is used by the JSON decoder to resolve type variables.
-- Uses graphSchemaTypes (which contains TypeScheme values) and extracts the underlying Type.
buildSchemaMap :: Graph -> M.Map Name Type
buildSchemaMap g = M.map extractType (graphSchemaTypes g)
  where
    extractType (TypeScheme _ t _) = stripTypeAnnotationsTop t

    -- Strip only top-level annotations
    stripTypeAnnotationsTop (TypeAnnotated (AnnotatedType t _)) = stripTypeAnnotationsTop t
    stripTypeAnnotationsTop t = t

main :: IO ()
main = do
  flushPut "=== Verify JSON Kernel ==="
  flushPut ""

  let basePath = "../../dist/json/hydra-kernel/src/main/json"

  flushPut "Building graph from kernel modules..."
  -- Build the graph from all kernel modules - this provides the type info
  let graph = modulesToGraph kernelModules kernelModules

  flushPut "Building schema map for JSON decoding..."
  let schemaMap = buildSchemaMap graph
  flushPut $ "  Schema map has " ++ show (M.size schemaMap) ++ " type definitions"

  -- Use TypeVariable to reference the Module type - the decoder will resolve it
  let modType = TypeVariable _Module
  flushPut "Using TypeVariable for Module type (decoder will resolve it)."

  flushPut "Counting modules..."
  let numModules = length kernelModules
  flushPut $ "Verifying " ++ show numModules ++ " kernel modules..."
  putStrLn ""

  -- For each module, read JSON file, decode to Term, then to Module, and compare
  results <- forM kernelModules $ \origMod -> do
    let ns = moduleNamespace origMod
        nsStr = unNamespace ns
        filePath = basePath </> namespaceToPath ns ++ ".json"

    flushPut $ "  Processing: " ++ nsStr

    exists <- doesFileExist filePath
    if not exists
      then do
        flushPut $ "  ✗ " ++ nsStr ++ " (file not found)"
        return (False, nsStr ++ ": file not found")
      else do
        -- Parse JSON using Aeson (fast!)
        parseResult <- parseJsonFile filePath
        case parseResult of
          Left err -> do
            flushPut $ "  ✗ " ++ nsStr ++ " (JSON parse error)"
            flushPut $ "    " ++ err
            return (False, nsStr ++ ": JSON parse error")
          Right jsonVal -> do
            flushPut $ "    JSON parsed successfully"

            -- Decode JSON to Term using type-directed decoder with Module type
            -- The schemaMap is used to resolve type variables
            case JsonDecode.fromJson schemaMap _Module modType jsonVal of
              Left err -> do
                flushPut $ "  ✗ " ++ nsStr ++ " (JSON decode error)"
                flushPut $ "    " ++ err
                return (False, nsStr ++ ": " ++ err)
              Right term -> do
                flushPut $ "    JSON decoded to Term"

                -- Decode Term to Module
                case DecodePackaging.module_ graph term of
                  Left (DecodingError err) -> do
                    flushPut $ "  ✗ " ++ nsStr ++ " (module decode error)"
                    flushPut $ "    " ++ err
                    return (False, nsStr ++ ": " ++ err)
                  Right decodedMod ->
                    let compareMod = if isTypeModule origMod then decodedMod else stripTypeAnnotations decodedMod
                        compareOrig = if isTypeModule origMod then origMod else stripTypeAnnotations origMod
                    in if compareMod == compareOrig
                      then do
                        flushPut $ "  ✓ " ++ nsStr
                        return (True, nsStr)
                      else do
                        flushPut $ "  ✗ " ++ nsStr ++ " (content mismatch)"
                        let diff = findDifference compareOrig compareMod
                        flushPut $ "    " ++ diff
                        return (False, nsStr ++ ": " ++ diff)

  let failures = filter (not . fst) results
      successes = filter fst results

  putStrLn ""
  if null failures
    then do
      putStrLn "=== SUCCESS ==="
      putStrLn $ "All " ++ show (length successes) ++ " modules verified successfully!"
      exitSuccess
    else do
      putStrLn "=== FAILED ==="
      putStrLn $ show (length failures) ++ " modules failed verification:"
      mapM_ (putStrLn . ("  " ++) . snd) (take 20 failures)
      when (length failures > 20) $
        putStrLn $ "  ... and " ++ show (length failures - 20) ++ " more"
      exitFailure

-- | Check whether a module contains only native type definitions (no term definitions).
isTypeModule :: Module -> Bool
isTypeModule m = all isNativeType (moduleBindings m)

-- | Strip type annotations from a module for comparison with raw (pre-inference) modules.
-- Removes TypeLambda, TypeApplication, binding TypeSchemes, and lambda domain types from terms,
-- but preserves user annotations.
stripTypeAnnotations :: Module -> Module
stripTypeAnnotations m = m {
  moduleDefinitions = fmap stripDef (moduleDefinitions m) }
  where
    stripDef (DefinitionTerm td) = DefinitionTerm td {
      termDefinitionTerm = Strip.removeTypesFromTerm (termDefinitionTerm td),
      termDefinitionType = Just $ TypeScheme [] (TypeVariable $ Name "hydra.core.Unit") Nothing }
    stripDef d = d

-- | Find the first difference between two modules
findDifference :: Module -> Module -> String
findDifference orig decoded
  | moduleNamespace orig /= moduleNamespace decoded =
      "namespace differs: " ++ unNamespace (moduleNamespace orig) ++ " vs " ++ unNamespace (moduleNamespace decoded)
  | length (moduleDefinitions orig) /= length (moduleDefinitions decoded) =
      "element count differs: " ++ show (length (moduleDefinitions orig)) ++ " vs " ++ show (length (moduleDefinitions decoded))
  | moduleTermDependencies orig /= moduleTermDependencies decoded =
      "termDependencies differ"
  | moduleTypeDependencies orig /= moduleTypeDependencies decoded =
      "typeDependencies differ"
  | moduleDescription orig /= moduleDescription decoded =
      "description differs"
  | otherwise =
      "elements differ (checking first mismatch...)" ++ findElementDiff (moduleDefinitions orig) (moduleDefinitions decoded)

findElementDiff :: [Definition] -> [Definition] -> String
findElementDiff [] [] = ""
findElementDiff (o:os) (d:ds)
  | o == d = findElementDiff os ds
  | otherwise = " - definition differs at " ++ show (defName o) ++ " vs " ++ show (defName d)
  where
    defName (DefinitionTerm td) = termDefinitionName td
    defName (DefinitionType td) = typeDefinitionName td
findElementDiff _ _ = " - different element counts"
