module Hydra.Sources.Test.Lib.Hashing where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Phantoms hiding ((++))  -- (@@), primitive, string
import Hydra.Overlay.Haskell.Dsl.Typed.Literals               (string)
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Lib.Hashing as DefHashing
import qualified Hydra.Lib.Literals as DefLiterals
import qualified Hydra.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.test.lib.hashing"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.reduction", ModuleName "hydra.show.core"] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.hashing primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- The UTF-8 bytes of a string, as a binary-typed term. SHA-256 known-answer vectors are stated over
-- byte sequences; this pairs sha256/sha256Hex with encodeUtf8 exactly as a file-hashing caller would.
bytes :: String -> TypedTerm Binary
bytes s = retype (primitive DefText.encodeUtf8 @@ string s)
  where
    retype :: TypedTerm x -> TypedTerm Binary
    retype (TypedTerm t) = TypedTerm t

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.hashing primitives" $
    supergroup "hydra.lib.hashing primitives" [
      hashingSha256,
      hashingSha256Hex]

-- sha256 : binary -> binary. The raw digest is projected to a string via literals.binaryToString
-- (base64) and compared against the known base64 of the digest. Comparing against a base64 STRING
-- (rather than a binary literal) keeps the expected value host-portable: a binary literal of the
-- 32 raw digest bytes would need every host to emit high bytes (128..255) correctly, which the Java
-- coder cannot (byte-literal overflow) — the string form sidesteps that entirely.
hashingSha256 :: TypedTerm TestGroup
hashingSha256 = subgroup "sha256" [
    test "empty input" ""
      "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=",
    test "abc" "abc"
      "ungWv48Bz+pBQUDeXa4iI7ADYaOWF3qctBD/YfIAFa0="]
  where
    test name input expected = stringEvalPair name
      (retype (primitive DefLiterals.binaryToString @@ (primitive DefHashing.sha256 @@ bytes input)))
      (string expected)
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t

-- sha256Hex : binary -> string. Known-answer vectors: empty, "abc", a multi-block input, and the
-- four SHA-256 padding-boundary lengths (input = "a" repeated N times). 55 is the largest message
-- whose 1-byte-plus-length padding still fits one 64-byte block; 56 forces a second block; 63/64
-- straddle the block size. These guard the hand-rolled Common Lisp and Scheme implementations
-- against a future padding-logic regression that would pass on shorter inputs.
hashingSha256Hex :: TypedTerm TestGroup
hashingSha256Hex = subgroup "sha256Hex" [
    test "empty input" ""
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    test "abc" "abc"
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
    test "multi-block input (spans two SHA-256 blocks)"
      "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs."
      "d51712a8d1852b5acf942c19caddf168f80120d2f3a72c2d917227fd37f22788",
    test "padding boundary: 55 bytes (fills one block)" (replicate 55 'a')
      "9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318",
    test "padding boundary: 56 bytes (forces a second block)" (replicate 56 'a')
      "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a",
    test "padding boundary: 63 bytes (one below block size)" (replicate 63 'a')
      "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34",
    test "padding boundary: 64 bytes (exactly one block)" (replicate 64 'a')
      "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"]
  where
    test name input expected = stringEvalPair name
      (retype (primitive DefHashing.sha256Hex @@ bytes input))
      (string expected)
    retype :: TypedTerm x -> TypedTerm String
    retype (TypedTerm t) = TypedTerm t
