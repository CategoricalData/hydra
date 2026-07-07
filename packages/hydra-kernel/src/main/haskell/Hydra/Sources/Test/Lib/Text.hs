module Hydra.Sources.Test.Lib.Text where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Hydra.Lib.Eithers as DefEithers
import qualified Hydra.Lib.Text as DefText


ns :: ModuleName
ns = ModuleName "hydra.test.lib.text"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.show.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.text primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.text primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.text primitives" $
    supergroup "hydra.lib.text primitives" [
      textDecodeUtf8,
      textEncodeUtf8]

-- decodeUtf8: valid byte sequences decode to right(text); an invalid sequence yields left(message),
-- where the message itself is host-defined, so only isLeft is asserted for the invalid case.
textDecodeUtf8 :: TypedTerm TestGroup
textDecodeUtf8 = subgroup "decodeUtf8" [
  test "empty bytes" B.empty (right (string "")),
  test "ASCII text" (BC.pack "hello") (right (string "hello")),
  test "multi-byte UTF-8 text"
    (B.pack [0xC3, 0xA9, 0x76, 0xC3, 0xA9, 0x6E, 0x65, 0x6D, 0x65, 0x6E, 0x74])
    (right (string "\233v\233nement")),
  invalidCase "invalid lone continuation byte" (B.pack [0x80])]
  where
    test name x expected = primCase name DefText.decodeUtf8 [binary x] expected
    invalidCase name x = primCase name DefEithers.isLeft
      [primitive DefText.decodeUtf8 @@ binary x]
      (boolean True)

-- encodeUtf8: total; asserts the exact byte sequence for ASCII and multi-byte (non-ASCII) text.
textEncodeUtf8 :: TypedTerm TestGroup
textEncodeUtf8 = subgroup "encodeUtf8" [
  test "empty string" "" B.empty,
  test "ASCII text" "hello" (BC.pack "hello"),
  test "multi-byte UTF-8 text" "\233v\233nement" (B.pack [0xC3, 0xA9, 0x76, 0xC3, 0xA9, 0x6E, 0x65, 0x6D, 0x65, 0x6E, 0x74])]
  where
    test name x result = primCase name DefText.encodeUtf8 [string x] (binary result)
