-- | Primitive declarations for the hydra.lib.text namespace.

module Hydra.Sources.Kernel.Lib.Text where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "hydra.lib.text"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.text module.")}
  where
    definitions = [decodeUtf8, encodeUtf8]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

decodeUtf8 :: PrimitiveDefinition
decodeUtf8 = define "decodeUtf8" "Decode a sequence of bytes as UTF-8 text."
  (sigWithParams [("bytes", "the byte sequence to decode as UTF-8")] $ TypeScheme [] (Types.binary Types.~> Types.either_ Types.string Types.string) Nothing)
  ["decodeUtf8(bytes) attempts to interpret bytes as a UTF-8 encoded string. A byte sequence which\
  \ is not valid UTF-8 yields left(message), where message is a host-provided description of the\
  \ decoding failure; a successful decode yields right(text).",
   "Pairs with hydra.lib.files.readFile, which returns raw bytes."]

encodeUtf8 :: PrimitiveDefinition
encodeUtf8 = define "encodeUtf8" "Encode text as a sequence of UTF-8 bytes."
  (sigWithParams [("text", "the text to encode as UTF-8")] $ TypeScheme [] (Types.string Types.~> Types.binary) Nothing)
  ["encodeUtf8(text) returns the UTF-8 encoding of text as raw bytes. Total: every Hydra string is\
  \ valid Unicode and therefore always encodes.",
   "Pairs with hydra.lib.files.writeFile, which expects raw bytes."]
