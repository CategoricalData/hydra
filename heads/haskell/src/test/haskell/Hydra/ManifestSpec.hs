-- | Compat tests for moduleFormatVersion in manifest.json (#415).
--
-- Verifies that:
--   1. parseManifestModuleFormatVersion returns Nothing for a legacy manifest
--      that predates the field (backward compat — treat absence as version 1).
--   2. parseManifestModuleFormatVersion returns Just n when the field is present.
--   3. currentModuleFormatVersion is 1 (the only shipped version so far).
--   4. A synthetic future-version manifest (version > current) is detectable
--      as a mismatch — the vN-reads-vN+1 skew case.

module Hydra.ManifestSpec where

import Hydra.Generation (currentModuleFormatVersion, parseManifestModuleFormatVersion)
import qualified Hydra.Json.Model as Json

import qualified Data.Scientific as SC
import qualified Test.Hspec as H


-- | Build a minimal manifest JSON value with an optional moduleFormatVersion.
manifestJson :: Maybe Int -> Json.Value
manifestJson mVer =
    Json.ValueObject $
      [ ("mainModules",        Json.ValueArray [])
      , ("manifestFormatVersion", Json.ValueNumber 1)
      ] ++
      case mVer of
        Nothing -> []
        Just v  -> [("moduleFormatVersion", Json.ValueNumber (fromIntegral v))]


spec :: H.Spec
spec = do
  H.describe "parseManifestModuleFormatVersion (#415)" $ do

    H.it "returns Nothing for a legacy manifest without moduleFormatVersion" $
      parseManifestModuleFormatVersion (manifestJson Nothing)
        `H.shouldBe` Nothing

    H.it "returns Just 1 for a manifest with moduleFormatVersion: 1" $
      parseManifestModuleFormatVersion (manifestJson (Just 1))
        `H.shouldBe` Just 1

    H.it "returns Just 2 for a synthetic future-version manifest (vN+1)" $
      parseManifestModuleFormatVersion (manifestJson (Just 2))
        `H.shouldBe` Just 2

    H.it "returns Nothing for a non-object manifest" $
      parseManifestModuleFormatVersion (Json.ValueArray [])
        `H.shouldBe` Nothing

  H.describe "currentModuleFormatVersion (#415)" $ do

    H.it "is 1 — the only shipped wire-format version" $
      round currentModuleFormatVersion `H.shouldBe` (1 :: Int)

    H.it "matches the value written into manifest.json by the writers" $
      parseManifestModuleFormatVersion (manifestJson (Just 1))
        `H.shouldBe` Just (round currentModuleFormatVersion :: Int)

    H.it "detects a future-version manifest as a mismatch (vN reads vN+1)" $ do
      let expected = round currentModuleFormatVersion :: Int
          future   = Just (expected + 1)
      case parseManifestModuleFormatVersion (manifestJson future) of
        Just v  -> (v /= expected) `H.shouldBe` True
        Nothing -> H.expectationFailure "expected Just (n+1), got Nothing"
