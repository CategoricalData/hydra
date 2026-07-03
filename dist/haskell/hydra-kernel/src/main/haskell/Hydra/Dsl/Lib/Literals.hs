-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.lib.literals

module Hydra.Dsl.Lib.Literals where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Ast as DslAst
import qualified Hydra.Dsl.Coders as DslCoders
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Docs as DslDocs
import qualified Hydra.Dsl.Error.Checking as ErrorChecking
import qualified Hydra.Dsl.Error.Core as DslErrorCore
import qualified Hydra.Dsl.Error.File as DslErrorFile
import qualified Hydra.Dsl.Error.Packaging as DslErrorPackaging
import qualified Hydra.Dsl.Error.System as DslErrorSystem
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.File as DslFile
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Json.Model as JsonModel
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Dsl.Parsing as DslParsing
import qualified Hydra.Dsl.Paths as DslPaths
import qualified Hydra.Dsl.Query as DslQuery
import qualified Hydra.Dsl.Relational as DslRelational
import qualified Hydra.Dsl.System as DslSystem
import qualified Hydra.Dsl.Tabular as DslTabular
import qualified Hydra.Dsl.Testing as DslTesting
import qualified Hydra.Dsl.Time as DslTime
import qualified Hydra.Dsl.Topology as DslTopology
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Validation as DslValidation
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Error.Checking as Checking
import qualified Hydra.Error.Core as ErrorCore
import qualified Hydra.Error.File as ErrorFile
import qualified Hydra.Error.Packaging as ErrorPackaging
import qualified Hydra.Error.System as ErrorSystem
import qualified Hydra.Errors as Errors
import qualified Hydra.File as File
import qualified Hydra.Graph as Graph
import qualified Hydra.Json.Model as Model
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Parsing as Parsing
import qualified Hydra.Paths as Paths
import qualified Hydra.Query as Query
import qualified Hydra.Relational as Relational
import qualified Hydra.System as System
import qualified Hydra.Tabular as Tabular
import qualified Hydra.Testing as Testing
import qualified Hydra.Time as Time
import qualified Hydra.Topology as Topology
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Validation as Validation
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.ByteString as B
import qualified Data.Int as I

-- | DSL reference to hydra.lib.literals.bigintToDecimal
bigintToDecimal :: Typed.TypedTerm Integer -> Typed.TypedTerm Sci.Scientific
bigintToDecimal arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToDecimal")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToInt16
bigintToInt16 :: Typed.TypedTerm Integer -> Typed.TypedTerm I.Int16
bigintToInt16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToInt16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToInt32
bigintToInt32 :: Typed.TypedTerm Integer -> Typed.TypedTerm Int
bigintToInt32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToInt32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToInt64
bigintToInt64 :: Typed.TypedTerm Integer -> Typed.TypedTerm I.Int64
bigintToInt64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToInt64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToInt8
bigintToInt8 :: Typed.TypedTerm Integer -> Typed.TypedTerm I.Int8
bigintToInt8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToInt8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToUint16
bigintToUint16 :: Typed.TypedTerm Integer -> Typed.TypedTerm Int
bigintToUint16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToUint16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToUint32
bigintToUint32 :: Typed.TypedTerm Integer -> Typed.TypedTerm I.Int64
bigintToUint32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToUint32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToUint64
bigintToUint64 :: Typed.TypedTerm Integer -> Typed.TypedTerm Integer
bigintToUint64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToUint64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.bigintToUint8
bigintToUint8 :: Typed.TypedTerm Integer -> Typed.TypedTerm I.Int16
bigintToUint8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.bigintToUint8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.binaryToBytes
binaryToBytes :: Typed.TypedTerm B.ByteString -> Typed.TypedTerm [Int]
binaryToBytes arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.binaryToBytes")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.binaryToString
binaryToString :: Typed.TypedTerm B.ByteString -> Typed.TypedTerm String
binaryToString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.binaryToString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.decimalToBigint
decimalToBigint :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Integer
decimalToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.decimalToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.decimalToFloat32
decimalToFloat32 :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Float
decimalToFloat32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.decimalToFloat32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.decimalToFloat64
decimalToFloat64 :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm Double
decimalToFloat64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.decimalToFloat64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.float32ToDecimal
float32ToDecimal :: Typed.TypedTerm Float -> Typed.TypedTerm Sci.Scientific
float32ToDecimal arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.float32ToDecimal")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.float32ToFloat64
float32ToFloat64 :: Typed.TypedTerm Float -> Typed.TypedTerm Double
float32ToFloat64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.float32ToFloat64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.float64ToDecimal
float64ToDecimal :: Typed.TypedTerm Double -> Typed.TypedTerm Sci.Scientific
float64ToDecimal arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.float64ToDecimal")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.float64ToFloat32
float64ToFloat32 :: Typed.TypedTerm Double -> Typed.TypedTerm Float
float64ToFloat32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.float64ToFloat32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.int16ToBigint
int16ToBigint :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm Integer
int16ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.int16ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.int32ToBigint
int32ToBigint :: Typed.TypedTerm Int -> Typed.TypedTerm Integer
int32ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.int32ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.int64ToBigint
int64ToBigint :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Integer
int64ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.int64ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.int8ToBigint
int8ToBigint :: Typed.TypedTerm I.Int8 -> Typed.TypedTerm Integer
int8ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.int8ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readBigint
readBigint :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Integer)
readBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readBoolean
readBoolean :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Bool)
readBoolean arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readBoolean")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readDecimal
readDecimal :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Sci.Scientific)
readDecimal arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readDecimal")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readFloat32
readFloat32 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Float)
readFloat32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readFloat32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readFloat64
readFloat64 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Double)
readFloat64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readFloat64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readInt16
readInt16 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe I.Int16)
readInt16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readInt16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readInt32
readInt32 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Int)
readInt32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readInt32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readInt64
readInt64 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe I.Int64)
readInt64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readInt64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readInt8
readInt8 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe I.Int8)
readInt8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readInt8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readString
readString :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe String)
readString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readUint16
readUint16 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Int)
readUint16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readUint16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readUint32
readUint32 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe I.Int64)
readUint32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readUint32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readUint64
readUint64 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe Integer)
readUint64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readUint64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.readUint8
readUint8 :: Typed.TypedTerm String -> Typed.TypedTerm (Maybe I.Int16)
readUint8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.readUint8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showBigint
showBigint :: Typed.TypedTerm Integer -> Typed.TypedTerm String
showBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showBoolean
showBoolean :: Typed.TypedTerm Bool -> Typed.TypedTerm String
showBoolean arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showBoolean")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showDecimal
showDecimal :: Typed.TypedTerm Sci.Scientific -> Typed.TypedTerm String
showDecimal arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showDecimal")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showFloat32
showFloat32 :: Typed.TypedTerm Float -> Typed.TypedTerm String
showFloat32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showFloat32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showFloat64
showFloat64 :: Typed.TypedTerm Double -> Typed.TypedTerm String
showFloat64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showFloat64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showInt16
showInt16 :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm String
showInt16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showInt32
showInt32 :: Typed.TypedTerm Int -> Typed.TypedTerm String
showInt32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showInt64
showInt64 :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm String
showInt64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showInt8
showInt8 :: Typed.TypedTerm I.Int8 -> Typed.TypedTerm String
showInt8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showInt8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showString
showString :: Typed.TypedTerm String -> Typed.TypedTerm String
showString arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showString")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showUint16
showUint16 :: Typed.TypedTerm Int -> Typed.TypedTerm String
showUint16 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showUint16")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showUint32
showUint32 :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm String
showUint32 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showUint32")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showUint64
showUint64 :: Typed.TypedTerm Integer -> Typed.TypedTerm String
showUint64 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showUint64")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.showUint8
showUint8 :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm String
showUint8 arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.showUint8")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.stringToBinary
stringToBinary :: Typed.TypedTerm String -> Typed.TypedTerm B.ByteString
stringToBinary arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.stringToBinary")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.uint16ToBigint
uint16ToBigint :: Typed.TypedTerm Int -> Typed.TypedTerm Integer
uint16ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.uint16ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.uint32ToBigint
uint32ToBigint :: Typed.TypedTerm I.Int64 -> Typed.TypedTerm Integer
uint32ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.uint32ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.uint64ToBigint
uint64ToBigint :: Typed.TypedTerm Integer -> Typed.TypedTerm Integer
uint64ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.uint64ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))

-- | DSL reference to hydra.lib.literals.uint8ToBigint
uint8ToBigint :: Typed.TypedTerm I.Int16 -> Typed.TypedTerm Integer
uint8ToBigint arg0 =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermVariable (Core.Name "hydra.lib.literals.uint8ToBigint")),
      Core.applicationArgument = (Typed.unTypedTerm arg0)}))
