{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.LspSpec.spec
-}

-- | Executable tests for the hydra-lsp analysis and wire layers (#836). Like the coder tests (#699) these are
-- host-native: the wire layer maps between Hydra types and LSP JSON, which the cross-host DSL test framework
-- (string thunks) cannot express conveniently.
module Hydra.LspSpec where

import Hydra.Kernel
import qualified Hydra.Core.Json.Model as JM
import qualified Hydra.Ext.Lsp.Protocol as P
import qualified Hydra.Ext.Lsp.Wire as Wire
import qualified Hydra.Lsp.Analysis as Analysis

import qualified Test.Hspec as H


-- A small module in canonical textual syntax (cf. packages/hydra-kernel/src/main/hydra/hydra/core/docs.hydra).
fixture :: String
fixture = Prelude.unlines [
  "## A model for documentation strings",
  "module test.docs",
  "",
  "import hydra.core.packaging",
  "",
  "## A segment of a documentation string",
  "type DocSegment = union{",
  "",
  "  ## An inline reference",
  "  ref: string,",
  "",
  "  ## A raw prose fragment",
  "  text: string}"]

pos :: Int -> Int -> P.Position
pos = P.Position

spec :: H.Spec
spec = do
  H.describe "Lsp.Analysis.format (#836)" $ do

    H.it "formats a parseable module" $
      Analysis.format fixture `H.shouldSatisfy` (/= Nothing)

    H.it "is idempotent on its own output" $ do
      let once = Analysis.format fixture
      (once >>= Analysis.format) `H.shouldBe` once

    H.it "returns nothing for text that does not parse" $
      Analysis.format "module" `H.shouldBe` Nothing

  H.describe "Lsp.Wire enum tables (#836)" $ do

    H.it "encodes DiagnosticSeverity as LSP integers 1-4" $ do
      let enc = Wire.encodeSeverity
      map enc [P.DiagnosticSeverityError, P.DiagnosticSeverityWarning,
               P.DiagnosticSeverityInformation, P.DiagnosticSeverityHint]
        `H.shouldBe` map Wire.jsonInt [1, 2, 3, 4]

    H.it "encodes TextDocumentSyncKind as LSP integers 0-2" $
      map Wire.encodeSyncKind [P.TextDocumentSyncKindNone, P.TextDocumentSyncKindFull, P.TextDocumentSyncKindIncremental]
        `H.shouldBe` map Wire.jsonInt [0, 1, 2]

  H.describe "Lsp.Wire encoders (#836)" $ do

    H.it "omits absent optional diagnostic fields rather than emitting null" $ do
      let d = P.Diagnostic (P.Range (pos 0 0) (pos 0 1)) Nothing Nothing Nothing "boom"
      case Wire.encodeDiagnostic d of
        JM.ValueObject kvs -> map fst kvs `H.shouldBe` ["range", "message"]
        other -> H.expectationFailure ("expected object, got " ++ show other)

    H.it "encodes ranges as nested line/character objects" $
      Wire.encodeRange (P.Range (pos 1 2) (pos 3 4)) `H.shouldBe` Wire.jsonObject [
        ("start", Wire.jsonObject [("line", Wire.jsonInt 1), ("character", Wire.jsonInt 2)]),
        ("end", Wire.jsonObject [("line", Wire.jsonInt 3), ("character", Wire.jsonInt 4)])]

  H.describe "Lsp.Wire decoders (#836)" $ do

    H.it "decodes didOpen params" $ do
      let td = Wire.jsonObject [("uri", Wire.jsonString "file:///a.hydra"), ("languageId", Wire.jsonString "hydra"),
                                ("version", Wire.jsonInt 3), ("text", Wire.jsonString "module a")]
      Wire.decodeDidOpen (Wire.jsonObject [("textDocument", td)])
        `H.shouldBe` Right (P.DidOpenParams (P.TextDocumentItem "file:///a.hydra" "hydra" 3 "module a"))

    H.it "fails with a message when a required field is missing" $
      Wire.decodeDidClose (Wire.jsonObject []) `H.shouldBe` Left "missing field: textDocument"
