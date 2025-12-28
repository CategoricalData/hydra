-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.literals"},ModuleName {unModuleName = "Literals"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.literals"},ModuleName {unModuleName = "Literals"})]

module Generation.Hydra.Test.Lib.LiteralsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Literals as Literals

spec :: H.Spec
spec = H.describe "hydra.lib.literals primitives" $ do
  H.describe "bigintToInt8" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToInt8 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToInt8 (-42))
      ((-42))
  H.describe "bigintToInt16" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToInt16 1000)
      (1000)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToInt16 (-1000))
      ((-1000))
  H.describe "bigintToInt32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToInt32 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToInt32 (-42))
      ((-42))
    H.it "zero" $ H.shouldBe
      (Literals.bigintToInt32 0)
      (0)
  H.describe "bigintToInt64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToInt64 1000000)
      (1000000)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToInt64 (-1000000))
      ((-1000000))
  H.describe "bigintToUint8" $ do
    H.it "zero" $ H.shouldBe
      (Literals.bigintToUint8 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.bigintToUint8 100)
      (100)
  H.describe "bigintToUint16" $ do
    H.it "zero" $ H.shouldBe
      (Literals.bigintToUint16 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.bigintToUint16 1000)
      (1000)
  H.describe "bigintToUint32" $ do
    H.it "zero" $ H.shouldBe
      (Literals.bigintToUint32 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.bigintToUint32 100000)
      (100000)
  H.describe "bigintToUint64" $ do
    H.it "zero" $ H.shouldBe
      (Literals.bigintToUint64 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.bigintToUint64 1000000)
      (1000000)
  H.describe "int8ToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.int8ToBigint 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.int8ToBigint (-42))
      ((-42))
    H.it "max value" $ H.shouldBe
      (Literals.int8ToBigint 127)
      (127)
    H.it "min value" $ H.shouldBe
      (Literals.int8ToBigint (-128))
      ((-128))
  H.describe "int16ToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.int16ToBigint 1000)
      (1000)
    H.it "negative" $ H.shouldBe
      (Literals.int16ToBigint (-1000))
      ((-1000))
  H.describe "int32ToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.int32ToBigint 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.int32ToBigint (-42))
      ((-42))
    H.it "zero" $ H.shouldBe
      (Literals.int32ToBigint 0)
      (0)
  H.describe "int64ToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.int64ToBigint 1000000)
      (1000000)
    H.it "negative" $ H.shouldBe
      (Literals.int64ToBigint (-1000000))
      ((-1000000))
  H.describe "uint8ToBigint" $ do
    H.it "zero" $ H.shouldBe
      (Literals.uint8ToBigint 0)
      (0)
    H.it "max value" $ H.shouldBe
      (Literals.uint8ToBigint 255)
      (255)
  H.describe "uint16ToBigint" $ do
    H.it "zero" $ H.shouldBe
      (Literals.uint16ToBigint 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.uint16ToBigint 1000)
      (1000)
  H.describe "uint32ToBigint" $ do
    H.it "zero" $ H.shouldBe
      (Literals.uint32ToBigint 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.uint32ToBigint 100000)
      (100000)
  H.describe "uint64ToBigint" $ do
    H.it "zero" $ H.shouldBe
      (Literals.uint64ToBigint 0)
      (0)
    H.it "typical value" $ H.shouldBe
      (Literals.uint64ToBigint 1000000)
      (1000000)
  H.describe "float32ToBigfloat" $ do
    H.it "positive" $ H.shouldBe
      (Literals.float32ToBigfloat 2.5)
      (2.5)
    H.it "negative" $ H.shouldBe
      (Literals.float32ToBigfloat (-2.5))
      ((-2.5))
    H.it "zero" $ H.shouldBe
      (Literals.float32ToBigfloat 0.0)
      (0.0)
  H.describe "float64ToBigfloat" $ do
    H.it "positive" $ H.shouldBe
      (Literals.float64ToBigfloat 3.14159)
      (3.14159)
    H.it "negative" $ H.shouldBe
      (Literals.float64ToBigfloat (-2.71828))
      ((-2.71828))
    H.it "zero" $ H.shouldBe
      (Literals.float64ToBigfloat 0.0)
      (0.0)
  H.describe "bigfloatToFloat32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigfloatToFloat32 3.14)
      (3.14)
    H.it "negative" $ H.shouldBe
      (Literals.bigfloatToFloat32 (-2.5))
      ((-2.5))
    H.it "zero" $ H.shouldBe
      (Literals.bigfloatToFloat32 0.0)
      (0.0)
  H.describe "bigfloatToFloat64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigfloatToFloat64 3.14159)
      (3.14159)
    H.it "negative" $ H.shouldBe
      (Literals.bigfloatToFloat64 (-2.71828))
      ((-2.71828))
    H.it "zero" $ H.shouldBe
      (Literals.bigfloatToFloat64 0.0)
      (0.0)
  H.describe "bigintToBigfloat" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToBigfloat 42)
      (42.0)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToBigfloat (-42))
      ((-42.0))
    H.it "zero" $ H.shouldBe
      (Literals.bigintToBigfloat 0)
      (0.0)
  H.describe "bigfloatToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigfloatToBigint 42.7)
      (43)
    H.it "negative" $ H.shouldBe
      (Literals.bigfloatToBigint (-42.7))
      ((-43))
    H.it "zero" $ H.shouldBe
      (Literals.bigfloatToBigint 0.0)
      (0)
    H.it "round down" $ H.shouldBe
      (Literals.bigfloatToBigint 42.3)
      (42)
    H.it "half even up" $ H.shouldBe
      (Literals.bigfloatToBigint 42.5)
      (42)
    H.it "half even down" $ H.shouldBe
      (Literals.bigfloatToBigint 43.5)
      (44)
  H.describe "showInt8" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showInt8 42)
      ("42")
    H.it "negative" $ H.shouldBe
      (Literals.showInt8 (-42))
      ("-42")
  H.describe "showInt16" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showInt16 1000)
      ("1000")
    H.it "negative" $ H.shouldBe
      (Literals.showInt16 (-1000))
      ("-1000")
  H.describe "showInt32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showInt32 42)
      ("42")
    H.it "negative" $ H.shouldBe
      (Literals.showInt32 (-42))
      ("-42")
    H.it "zero" $ H.shouldBe
      (Literals.showInt32 0)
      ("0")
  H.describe "showInt64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showInt64 1000000)
      ("1000000")
    H.it "negative" $ H.shouldBe
      (Literals.showInt64 (-1000000))
      ("-1000000")
  H.describe "showUint8" $ do
    H.it "zero" $ H.shouldBe
      (Literals.showUint8 0)
      ("0")
    H.it "max value" $ H.shouldBe
      (Literals.showUint8 255)
      ("255")
  H.describe "showUint16" $ do
    H.it "zero" $ H.shouldBe
      (Literals.showUint16 0)
      ("0")
    H.it "typical value" $ H.shouldBe
      (Literals.showUint16 1000)
      ("1000")
  H.describe "showUint32" $ do
    H.it "zero" $ H.shouldBe
      (Literals.showUint32 0)
      ("0")
    H.it "typical value" $ H.shouldBe
      (Literals.showUint32 100000)
      ("100000")
  H.describe "showUint64" $ do
    H.it "zero" $ H.shouldBe
      (Literals.showUint64 0)
      ("0")
    H.it "typical value" $ H.shouldBe
      (Literals.showUint64 1000000)
      ("1000000")
  H.describe "showBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showBigint 42)
      ("42")
    H.it "negative" $ H.shouldBe
      (Literals.showBigint (-42))
      ("-42")
    H.it "zero" $ H.shouldBe
      (Literals.showBigint 0)
      ("0")
  H.describe "showFloat32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showFloat32 3.14)
      ("3.14")
    H.it "negative" $ H.shouldBe
      (Literals.showFloat32 (-2.5))
      ("-2.5")
    H.it "zero" $ H.shouldBe
      (Literals.showFloat32 0.0)
      ("0.0")
  H.describe "showFloat64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showFloat64 3.14159)
      ("3.14159")
    H.it "zero" $ H.shouldBe
      (Literals.showFloat64 0.0)
      ("0.0")
  H.describe "showBigfloat" $ do
    H.it "positive" $ H.shouldBe
      (Literals.showBigfloat 3.14)
      ("3.14")
    H.it "zero" $ H.shouldBe
      (Literals.showBigfloat 0.0)
      ("0.0")
  H.describe "showBoolean" $ do
    H.it "true" $ H.shouldBe
      (Literals.showBoolean True)
      ("true")
    H.it "false" $ H.shouldBe
      (Literals.showBoolean False)
      ("false")
  H.describe "showString" $ do
    H.it "simple" $ H.shouldBe
      (Literals.showString "hello")
      ("\"hello\"")
    H.it "empty" $ H.shouldBe
      (Literals.showString "")
      ("\"\"")
  H.describe "readInt32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.readInt32 "42")
      (Just 42)
    H.it "negative" $ H.shouldBe
      (Literals.readInt32 "-42")
      (Just (-42))
    H.it "invalid" $ H.shouldBe
      (Literals.readInt32 "abc")
      (Nothing)
  H.describe "readInt64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.readInt64 "1000000")
      (Just 1000000)
    H.it "negative" $ H.shouldBe
      (Literals.readInt64 "-1000000")
      (Just (-1000000))
    H.it "invalid" $ H.shouldBe
      (Literals.readInt64 "abc")
      (Nothing)
  H.describe "readFloat32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.readFloat32 "3.14")
      (Just 3.14)
    H.it "negative" $ H.shouldBe
      (Literals.readFloat32 "-2.5")
      (Just (-2.5))
    H.it "invalid" $ H.shouldBe
      (Literals.readFloat32 "abc")
      (Nothing)
  H.describe "readFloat64" $ do
    H.it "positive" $ H.shouldBe
      (Literals.readFloat64 "3.14159")
      (Just 3.14159)
    H.it "negative" $ H.shouldBe
      (Literals.readFloat64 "-2.71828")
      (Just (-2.71828))
    H.it "invalid" $ H.shouldBe
      (Literals.readFloat64 "abc")
      (Nothing)
  H.describe "readBigfloat" $ do
    H.it "positive" $ H.shouldBe
      (Literals.readBigfloat "3.14")
      (Just 3.14)
    H.it "invalid" $ H.shouldBe
      (Literals.readBigfloat "abc")
      (Nothing)
  H.describe "readBoolean" $ do
    H.it "true" $ H.shouldBe
      (Literals.readBoolean "true")
      (Just True)
    H.it "false" $ H.shouldBe
      (Literals.readBoolean "false")
      (Just False)
    H.it "invalid" $ H.shouldBe
      (Literals.readBoolean "yes")
      (Nothing)
  H.describe "readString" $ do
    H.it "quoted string" $ H.shouldBe
      (Literals.readString "\"hello\"")
      (Just "hello")
    H.it "empty quoted" $ H.shouldBe
      (Literals.readString "\"\"")
      (Just "")
    H.it "unquoted" $ H.shouldBe
      (Literals.readString "hello")
      (Nothing)
  H.describe "stringToBinary" $ do
    H.it "simple base64" $ H.shouldBe
      (Literals.stringToBinary "aGVsbG8=")
      (Literals.stringToBinary "aGVsbG8=")
    H.it "empty string" $ H.shouldBe
      (Literals.stringToBinary "")
      (Literals.stringToBinary "")
  H.describe "binaryToString" $ do
    H.it "simple binary" $ H.shouldBe
      (Literals.binaryToString (Literals.stringToBinary "aGVsbG8="))
      ("aGVsbG8=")
    H.it "empty binary" $ H.shouldBe
      (Literals.binaryToString (Literals.stringToBinary ""))
      ("")
