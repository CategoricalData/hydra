{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.RewritingWrapSpec.spec
-}

module Hydra.RewritingWrapSpec where

import Hydra.Kernel
import qualified Hydra.Rewriting as Rewriting

import qualified Test.Hspec as H


-- | Direct unit coverage for #654's two new kernel functions: wrapTermToRecord and
-- wrapTypeToRecord normalize a wrapped (newtype) term/type to an equivalent single-field
-- record, using the conventional field name "value". See
-- packages/hydra-kernel/src/main/haskell/Hydra/Sources/Kernel/Terms/Rewriting.hs.
spec :: H.Spec
spec = H.describe "Rewriting.wrapTermToRecord / wrapTypeToRecord (#654)" $ do

  H.it "wrapTypeToRecord produces a single-field record with the field named 'value'" $ do
    Rewriting.wrapTypeToRecord (TypeLiteral LiteralTypeString) `H.shouldBe`
      TypeRecord [FieldType (Name "value") (TypeLiteral LiteralTypeString)]

  H.it "wrapTypeToRecord preserves the inner type exactly, whatever its shape" $ do
    let inner = TypeList (TypeVariable (Name "a"))
    Rewriting.wrapTypeToRecord inner `H.shouldBe` TypeRecord [FieldType (Name "value") inner]

  H.it "wrapTermToRecord produces a single-field record term named after the wrapper type" $ do
    let tname = Name "test.MyId"
        body = TermLiteral (LiteralString "hello")
    Rewriting.wrapTermToRecord tname body `H.shouldBe`
      TermRecord (Record tname [Field (Name "value") body])

  H.it "wrapTermToRecord preserves the wrapped body term exactly, whatever its shape" $ do
    let tname = Name "test.Pair"
        body = TermList [TermLiteral (LiteralInteger (IntegerValueInt32 1))]
    Rewriting.wrapTermToRecord tname body `H.shouldBe`
      TermRecord (Record tname [Field (Name "value") body])
