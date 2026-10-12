-- | Primitive declarations for the hydra.core.lib.text namespace.

module Hydra.Sources.Kernel.Lib.Text where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "hydra.core.lib.text"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = descriptionMetadataWithComments (Just "Primitives in the hydra.core.lib.text module.") [
              "Text encoding.",
              "These primitives are the canonical UTF-8 conversions between Hydra's binary and string\
              \ types, for interpreting bytes as text. They are distinct from\
              \ hydra.lib.literals.binaryToBase64 / base64ToBinary, which are total, round-trippable\
              \ base64 conversions for embedding arbitrary binary in a string context (e.g. a JSON\
              \ value) -- use those when the bytes are not necessarily text.",
              "Both text primitives are pure: file and other byte-oriented I/O is performed separately\
              \ (see hydra.lib.files), with encoding and decoding applied before or after."]}
  where
    definitions = [decodeUtf8, encodeUtf8]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

decodeUtf8 :: PrimitiveDefinition
decodeUtf8 = availableSince "0.17" $ withProvisions [
  requirement "decodesValidUtf8"
    "decodeUtf8 decodes a valid UTF-8 byte sequence to its text, wrapped in right, including the\
    \ empty byte sequence, ASCII-only text, and multi-byte (non-ASCII) text.",
  requirement "leftOnInvalidUtf8"
    "decodeUtf8 returns left(message) when the byte sequence is not valid UTF-8, such as a lone\
    \ continuation byte."]
  $ define "decodeUtf8" "Decode a sequence of bytes as UTF-8 text."
  (sigWithParams [("bytes", "the byte sequence to decode as UTF-8")] $ TypeScheme [] (Types.binary Types.~> Types.either_ Types.string Types.string) mempty)
  ["decodeUtf8(bytes) attempts to interpret bytes as a UTF-8 encoded string. A byte sequence which\
  \ is not valid UTF-8 yields left(message), where message is a host-provided description of the\
  \ decoding failure; a successful decode yields right(text).",
   "Pairs with hydra.core.lib.files.readFile, which returns raw bytes."]

encodeUtf8 :: PrimitiveDefinition
encodeUtf8 = availableSince "0.17" $ withProvisions [
  requirement "encodesExactUtf8Bytes"
    "encodeUtf8 returns the exact UTF-8 byte sequence for the given text, for the empty string,\
    \ ASCII-only text, and multi-byte (non-ASCII) text."]
  $ define "encodeUtf8" "Encode text as a sequence of UTF-8 bytes."
  (sigWithParams [("text", "the text to encode as UTF-8")] $ TypeScheme [] (Types.string Types.~> Types.binary) mempty)
  ["encodeUtf8(text) returns the UTF-8 encoding of text as raw bytes. Total: every Hydra string is\
  \ valid Unicode and therefore always encodes.",
   "Pairs with hydra.core.lib.files.writeFile, which expects raw bytes."]
