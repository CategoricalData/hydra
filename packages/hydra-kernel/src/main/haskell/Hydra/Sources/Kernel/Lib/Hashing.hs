-- | Primitive declarations for the hydra.lib.hashing namespace.

module Hydra.Sources.Kernel.Lib.Hashing where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "hydra.lib.hashing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.hashing module.")}
  where
    definitions = [sha256, sha256Hex]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

sha256 :: PrimitiveDefinition
sha256 = define "sha256" "Compute the SHA-256 digest of a sequence of bytes."
  (sig $ TypeScheme [] (Types.binary Types.~> Types.binary) Nothing)
  ["sha256(bytes) returns the 32-byte SHA-256 digest of bytes. Total: hashing is pure and never fails.",
   "Pairs with hydra.lib.files.readFile, which returns raw bytes, to hash file contents."]

sha256Hex :: PrimitiveDefinition
sha256Hex = define "sha256Hex" "Compute the SHA-256 digest of a sequence of bytes as a lowercase hex string."
  (sig $ TypeScheme [] (Types.binary Types.~> Types.string) Nothing)
  ["sha256Hex(bytes) returns the SHA-256 digest of bytes as a 64-character lowercase hexadecimal\
  \ string. Equivalent to hex-encoding the output of sha256. Total: hashing is pure and never fails.",
   "Pairs with hydra.lib.files.readFile, which returns raw bytes, to hash file contents."]
