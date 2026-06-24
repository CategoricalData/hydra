-- | Primitive declarations for the hydra.lib.text namespace.

module Hydra.Sources.Kernel.Lib.Text where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap     as Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms as Phantoms
import           Hydra.Overlay.Haskell.Dsl.Prims         (sig)
import qualified Hydra.Overlay.Haskell.Dsl.Types         as Types
import           Hydra.Sources.Kernel.Types.All


ns :: ModuleName
ns = ModuleName "hydra.lib.text"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.text module.")}
  where
    definitions = [
      primNoDef "decodeUtf8" "Decode a sequence of bytes as UTF-8 text." decodeUtf8Sig [
        "decodeUtf8(bytes) attempts to interpret bytes as a UTF-8 encoded string. A byte sequence which\
        \ is not valid UTF-8 yields left(message), where message is a host-provided description of the\
        \ decoding failure; a successful decode yields right(text).",
        "Pairs with hydra.lib.files.readFile, which returns raw bytes."],
      primNoDef "encodeUtf8" "Encode text as a sequence of UTF-8 bytes." encodeUtf8Sig [
        "encodeUtf8(text) returns the UTF-8 encoding of text as raw bytes. Total: every Hydra string is\
        \ valid Unicode and therefore always encodes.",
        "Pairs with hydra.lib.files.writeFile, which expects raw bytes."]]

primNoDef :: String -> String -> TermSignature -> [String] -> Definition
primNoDef localName description s comments =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName)) comments

-- decodeUtf8 : binary -> either<string, string>
decodeUtf8Sig :: TermSignature
decodeUtf8Sig = sig $ TypeScheme []
  (Types.binary Types.~> Types.either_ Types.string Types.string) Nothing

-- encodeUtf8 : string -> binary
encodeUtf8Sig :: TermSignature
encodeUtf8Sig = sig $ TypeScheme []
  (Types.string Types.~> Types.binary) Nothing
