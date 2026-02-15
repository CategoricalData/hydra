-- | Bootstrapping demo executable: loads Hydra kernel modules from JSON and
-- generates code for a target language. This demonstrates that Hydra can
-- regenerate its kernel from a language-independent representation (JSON with
-- System F type annotations).
--
-- Output is written to a standalone directory (default: /tmp/hydra-bootstrapping-demo)
-- with subdirectories for each bootstrapping path.

module Main where

import Hydra.Kernel
import Hydra.Ext.Generation

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.FilePath as FP


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  (target, outBase) <- case args of
    ["--target", t] -> return (t, "/tmp/hydra-bootstrapping-demo")
    ["--target", t, "--output", o] -> return (t, o)
    _ -> do
      putStrLn "Usage: bootstrap-from-json --target <haskell|java|python> [--output <dir>]"
      exitFailure

  let outDir = outBase FP.</> ("haskell-to-" ++ target)

  putStrLn "=========================================="
  putStrLn $ "Bootstrapping Hydra kernel to " ++ target ++ " from JSON"
  putStrLn $ "Output directory: " ++ outDir
  putStrLn "=========================================="
  putStrLn ""

  -- JSON directory (relative to hydra-ext working directory)
  let haskellMainJson = "../hydra-haskell/src/gen-main/json"

  -- Load main modules from JSON, then strip System F type annotations
  -- from terms (type applications, type lambdas, lambda domain types,
  -- let binding type schemes). Module-level TypeSchemes on term bindings
  -- are also stripped to avoid bigfloat/float64 unification conflicts.
  -- TypeSchemes on type-defining bindings are preserved so the schema
  -- graph can identify type-defining bindings via isNativeType.
  putStrLn "Loading main modules from JSON..."
  putStrLn $ "  Source: " ++ haskellMainJson
  rawMods <- loadAllModulesFromJsonDirWith False haskellMainJson kernelModules
  -- Two views of main modules:
  --   mainModsWithConstraints: preserves Ord/Eq constraints as minimal TypeSchemes
  --     (used for main code generation where constraints must appear in output)
  --   mainModsClean: strips all TypeSchemes (used as universe for test generation
  --     to avoid schema-type unification conflicts in the larger combined graph)
  let mainModsWithConstraints = fmap (stripTermTypesPreservingConstraints) rawMods
  let mainModsClean = fmap stripTermTypes rawMods
  putStrLn $ "  Loaded " ++ show (length rawMods) ++ " main modules."
  putStrLn ""

  -- Generate code for the target language (kernel modules only)
  let outMain = outDir FP.</> ("src/gen-main/" ++ target)
  putStrLn $ "Generating " ++ target ++ " code to " ++ outMain ++ " ..."
  putStrLn ""

  case target of
    "haskell" -> writeHaskell outMain mainModsWithConstraints mainModsWithConstraints
    "java"    -> writeJava    outMain mainModsWithConstraints mainModsWithConstraints
    "python"  -> writePython  outMain mainModsWithConstraints mainModsWithConstraints
    _ -> do
      putStrLn $ "Unknown target: " ++ target
      exitFailure

  putStrLn ""

  -- Load and generate test modules
  let haskellTestJson = "../hydra-haskell/src/gen-test/json"
  putStrLn "Loading test modules from JSON..."
  putStrLn $ "  Source: " ++ haskellTestJson
  rawTestMods <- loadAllModulesFromJsonDirWith False haskellTestJson (kernelModules ++ mainModsClean)
  let testMods = fmap stripTermTypes rawTestMods
  putStrLn $ "  Loaded " ++ show (length testMods) ++ " test modules."
  putStrLn ""

  let allUniverse = mainModsClean ++ testMods
  let outTest = outDir FP.</> ("src/gen-test/" ++ target)
  putStrLn $ "Generating " ++ target ++ " test code to " ++ outTest ++ " ..."
  putStrLn ""

  case target of
    "haskell" -> writeHaskell outTest allUniverse testMods
    "java"    -> writeJava    outTest allUniverse testMods
    "python"  -> writePython  outTest allUniverse testMods
    _ -> return ()

  putStrLn ""
  putStrLn "=========================================="
  putStrLn "Bootstrap complete!"
  putStrLn $ "Output written to: " ++ outDir
  putStrLn "=========================================="

-- | Strip System F type annotations from all term bodies in a module.
-- All TypeSchemes on term bindings are removed; only type-defining
-- bindings are preserved (needed by isNativeType for schema graph
-- construction).
stripTermTypes :: Module -> Module
stripTermTypes m = m { moduleElements = fmap stripBinding (moduleElements m) }
  where
    stripBinding b = b
      { bindingTerm = removeTypesFromTerm (bindingTerm b)
      , bindingType = if isNativeType b then bindingType b else Nothing }

-- | Like stripTermTypes, but preserves typeclass constraints (Ord, Eq)
-- by keeping the original TypeScheme with schema type names replaced by
-- fresh-like type variables. This preserves the type structure (so
-- constraints map to the correct type variable positions during
-- inference unification) while avoiding schema-name unification
-- conflicts. This is needed because the inference engine cannot
-- rediscover Ord constraints hidden behind nominal types (e.g. Set/Map
-- inside newtype wrappers).
stripTermTypesPreservingConstraints :: Module -> Module
stripTermTypesPreservingConstraints m = m { moduleElements = fmap stripBinding (moduleElements m) }
  where
    stripBinding b = b
      { bindingTerm = removeTypesFromTerm (bindingTerm b)
      , bindingType = stripTypeScheme b }
    stripTypeScheme b
      | isNativeType b = bindingType b
      | otherwise = case bindingType b of
          Just ts -> case typeSchemeConstraints ts of
            Just c | not (M.null c) -> Just (eraseSchemaNames ts)
            _ -> Nothing
          Nothing -> Nothing
    -- Replace schema type names (qualified names like "hydra.core.Term")
    -- with unique placeholder type variables to avoid schema unification
    -- errors while preserving the type structure. Polymorphic variables
    -- (unqualified names like "v", "t0") are left unchanged.
    eraseSchemaNames ts =
      let vars = typeSchemeVariables ts
          varSet = S.fromList (fmap unName vars)
          rewriteType t = case t of
            TypeVariable n ->
              if isSchemaName n && not (S.member (unName n) varSet)
                then TypeVariable (Name ("_s" ++ show (abs (hash (unName n)))))
                else t
            TypeFunction ft -> TypeFunction (FunctionType
              { functionTypeDomain = rewriteType (functionTypeDomain ft)
              , functionTypeCodomain = rewriteType (functionTypeCodomain ft) })
            TypeApplication at -> TypeApplication (ApplicationType
              { applicationTypeFunction = rewriteType (applicationTypeFunction at)
              , applicationTypeArgument = rewriteType (applicationTypeArgument at) })
            TypeList lt -> TypeList (rewriteType lt)
            TypeSet st -> TypeSet (rewriteType st)
            TypeMap mt -> TypeMap (MapType
              { mapTypeKeys = rewriteType (mapTypeKeys mt)
              , mapTypeValues = rewriteType (mapTypeValues mt) })
            TypeMaybe ot -> TypeMaybe (rewriteType ot)
            TypeEither et -> TypeEither (EitherType
              { eitherTypeLeft = rewriteType (eitherTypeLeft et)
              , eitherTypeRight = rewriteType (eitherTypeRight et) })
            TypePair pt -> TypePair (PairType
              { pairTypeFirst = rewriteType (pairTypeFirst pt)
              , pairTypeSecond = rewriteType (pairTypeSecond pt) })
            _ -> t
      in ts { typeSchemeType = rewriteType (typeSchemeType ts) }
    isSchemaName (Name n) = elem '.' n
    hash s = foldl (\h c -> h * 31 + fromEnum c) 0 s
