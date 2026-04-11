-- | Haskell-specific code generation helpers.
--
-- Depends on the kernel-agnostic 'Hydra.Generation' module plus the Haskell coder
-- and language binding. These are the functions that drive code generation into
-- Haskell source trees.

module Hydra.Haskell.Generation (
  module Hydra.Haskell.Generation,
) where

import Hydra.Kernel
import Hydra.Ext.Haskell.Coder
import Hydra.Ext.Haskell.Language
import Hydra.Generation
import qualified Hydra.Sources.All as Sources
import qualified Hydra.Sources.Kernel.Types.Core as CoreTypes


-- | Generate Haskell source files from modules.
-- First argument: output directory
-- Second argument: universe modules (all modules for type/term resolution)
-- Third argument: modules to transform and generate
writeHaskell :: FilePath -> [Module] -> [Module] -> IO Int
writeHaskell = generateSources moduleToHaskell haskellLanguage True False False False

----------------------------------------

writeCoderSourceHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderSourceHaskell generate basePath universeModules typeModules = do
  sourceMods <- generateCoderSourceModules generate universeModules typeModules
  -- The source modules need the Module encoder/decoder and Core types
  _ <- writeHaskell basePath (universeModules ++ sourceMods) sourceMods
  return ()

-- | Write decoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Decode/
writeDecoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderSourceHaskell = writeCoderSourceHaskell generateDecoderModules

-- | Write encoder Source modules as Haskell to the given path.
-- These typically go to src/gen-main/haskell/Hydra/Sources/Encode/
writeEncoderSourceHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderSourceHaskell = writeCoderSourceHaskell generateEncoderModules

----------------------------------------

-- | Write encoder/decoder modules as Haskell to the given path.
-- First argument: generator function for encoder or decoder modules
-- Second argument: output directory
-- Third argument: universe modules (all modules for type/term resolution)
-- Fourth argument: type modules to generate encoders/decoders for
-- Note: This function bypasses type inference; for efficiency, we generate type signatures directly.
writeCoderHaskell :: ([Module] -> [Module] -> IO [Module]) -> FilePath -> [Module] -> [Module] -> IO ()
writeCoderHaskell generate basePath universeModules typeModules = do
    coderMods <- generate universeModules typeModules
    -- Add core types namespace to each encoder/decoder module's type dependencies
    -- since the encoders/decoders reference hydra.core.Term, hydra.core.Injection, etc.
    let withCoreDeps = fmap addCoreDep coderMods
    _ <- writeHaskell basePath universeModules withCoreDeps
    return ()
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

writeDecoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDecoderHaskell = writeCoderHaskell generateDecoderModules

writeEncoderHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeEncoderHaskell = writeCoderHaskell generateEncoderModules

----------------------------------------
-- DSL Module Generation
----------------------------------------

-- | Write the hydra.dsls source module (the DSL generator itself) to Haskell.
-- The Dsls module is NOT included in the universe to avoid infinite recursion
-- during graph construction (its terms reference decoders that reference types).
writeDslSourceHaskell :: FilePath -> IO ()
writeDslSourceHaskell basePath = do
    _ <- writeHaskell basePath Sources.mainModules Sources.dslSourceModules
    return ()

-- | Write DSL modules with doInfer=False. All bindings are fully typed.
writeDslHaskell :: FilePath -> [Module] -> [Module] -> IO ()
writeDslHaskell basePath universeModules typeModules = do
    dslMods <- generateDslModules universeModules typeModules
    let nonEmpty = filter (not . null . moduleDefinitions) dslMods
    let withCoreDeps = fmap addCoreDep nonEmpty
    _ <- generateSources moduleToHaskell haskellLanguage False False False False basePath universeModules withCoreDeps
    return ()
  where
    addCoreDep m = m { moduleTypeDependencies = CoreTypes.ns : moduleTypeDependencies m }

----------------------------------------
-- Lexicon
----------------------------------------

-- | Generate the lexicon to the standard location, using the Haskell-host kernel modules.
writeLexiconToStandardPath :: IO ()
writeLexiconToStandardPath = writeLexicon "../docs/hydra-lexicon.txt" Sources.kernelModules
