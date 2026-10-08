-- | Primitive declarations for the hydra.core.lib.hashing namespace.

module Hydra.Sources.Kernel.Lib.Hashing where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "hydra.core.lib.hashing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.core.lib.hashing module.")}
  where
    definitions = [sha256, sha256Hex]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

sha256 :: PrimitiveDefinition
sha256 = withProvisions [
  requirement "matchesKnownAnswerVectors"
    "sha256 returns the correct 32-byte SHA-256 digest for known-answer input vectors, including\
    \ the empty byte sequence."]
  $ define "sha256" "Compute the SHA-256 digest of a sequence of bytes."
  (sigWithParams [("bytes", "the sequence of bytes to hash")] $ TypeScheme [] (Types.binary Types.~> Types.binary) mempty)
  ["sha256(bytes) returns the 32-byte SHA-256 digest of bytes. Total: hashing is pure and never fails.",
   "Pairs with hydra.core.lib.files.readFile, which returns raw bytes, to hash file contents."]

sha256Hex :: PrimitiveDefinition
sha256Hex = withProvisions [
  requirement "matchesKnownAnswerVectors"
    "sha256Hex returns the correct 64-character lowercase hexadecimal SHA-256 digest for\
    \ known-answer input vectors, including the empty byte sequence and a multi-block input.",
  requirement "handlesPaddingBoundariesCorrectly"
    "sha256Hex returns the correct digest at every SHA-256 message-padding boundary length (55\
    \ bytes, the largest message whose padding still fits one 64-byte block; 56 bytes, which\
    \ forces a second block; and 63 and 64 bytes, straddling the block size), guarding against a\
    \ padding-logic regression that would only surface at these specific lengths."]
  $ define "sha256Hex" "Compute the SHA-256 digest of a sequence of bytes as a lowercase hex string."
  (sigWithParams [("bytes", "the sequence of bytes to hash")] $ TypeScheme [] (Types.binary Types.~> Types.string) mempty)
  ["sha256Hex(bytes) returns the SHA-256 digest of bytes as a 64-character lowercase hexadecimal\
  \ string. Equivalent to hex-encoding the output of sha256. Total: hashing is pure and never fails.",
   "Pairs with hydra.core.lib.files.readFile, which returns raw bytes, to hash file contents."]
