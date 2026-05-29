-- | Primitive declarations for the hydra.lib.literals namespace.

module Hydra.Sources.Kernel.Lib.Literals where

import Hydra.Kernel
import qualified Hydra.Dsl.Bootstrap         as Bootstrap
import           Hydra.Dsl.Meta.Phantoms     as Phantoms
import qualified Hydra.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))


ns :: ModuleName
ns = ModuleName "hydra.lib.literals"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleDescription = Just "Primitives in the hydra.lib.literals namespace."}
  where
    definitions = [
      primNoDef "bigintToDecimal"  "Convert a bigint to a decimal." (sigFn Types.bigint Types.decimal),
      primNoDef "bigintToInt16"    "Convert a bigint to an int16 (truncating)." (sigFn Types.bigint Types.int16),
      primNoDef "bigintToInt32"    "Convert a bigint to an int32 (truncating)." (sigFn Types.bigint Types.int32),
      primNoDef "bigintToInt64"    "Convert a bigint to an int64 (truncating)." (sigFn Types.bigint Types.int64),
      primNoDef "bigintToInt8"     "Convert a bigint to an int8 (truncating)." (sigFn Types.bigint Types.int8),
      primNoDef "bigintToUint16"   "Convert a bigint to a uint16 (truncating)." (sigFn Types.bigint Types.uint16),
      primNoDef "bigintToUint32"   "Convert a bigint to a uint32 (truncating)." (sigFn Types.bigint Types.uint32),
      primNoDef "bigintToUint64"   "Convert a bigint to a uint64 (truncating)." (sigFn Types.bigint Types.uint64),
      primNoDef "bigintToUint8"    "Convert a bigint to a uint8 (truncating)." (sigFn Types.bigint Types.uint8),
      primNoDef "binaryToBytes"    "Convert binary data to a list of byte values." (sigFn Types.binary (Types.list Types.int32)),
      primNoDef "binaryToString"   "Convert binary data to a UTF-8 string." (sigFn Types.binary Types.string),
      primNoDef "decimalToBigint"  "Convert a decimal to a bigint (truncating)." (sigFn Types.decimal Types.bigint),
      primNoDef "decimalToFloat32" "Convert a decimal to a float32." (sigFn Types.decimal Types.float32),
      primNoDef "decimalToFloat64" "Convert a decimal to a float64." (sigFn Types.decimal Types.float64),
      primNoDef "float32ToDecimal" "Convert a float32 to a decimal." (sigFn Types.float32 Types.decimal),
      primNoDef "float32ToFloat64" "Convert a float32 to a float64." (sigFn Types.float32 Types.float64),
      primNoDef "float64ToDecimal" "Convert a float64 to a decimal." (sigFn Types.float64 Types.decimal),
      primNoDef "float64ToFloat32" "Convert a float64 to a float32 (lossy)." (sigFn Types.float64 Types.float32),
      primNoDef "int16ToBigint"    "Convert an int16 to a bigint." (sigFn Types.int16 Types.bigint),
      primNoDef "int32ToBigint"    "Convert an int32 to a bigint." (sigFn Types.int32 Types.bigint),
      primNoDef "int64ToBigint"    "Convert an int64 to a bigint." (sigFn Types.int64 Types.bigint),
      primNoDef "int8ToBigint"     "Convert an int8 to a bigint." (sigFn Types.int8 Types.bigint),
      primNoDef "readBigint"       "Parse a string as a bigint." (sigFn Types.string (Types.optional Types.bigint)),
      primNoDef "readBoolean"      "Parse a string as a boolean." (sigFn Types.string (Types.optional Types.boolean)),
      primNoDef "readDecimal"      "Parse a string as a decimal." (sigFn Types.string (Types.optional Types.decimal)),
      primNoDef "readFloat32"      "Parse a string as a float32." (sigFn Types.string (Types.optional Types.float32)),
      primNoDef "readFloat64"      "Parse a string as a float64." (sigFn Types.string (Types.optional Types.float64)),
      primNoDef "readInt16"        "Parse a string as an int16." (sigFn Types.string (Types.optional Types.int16)),
      primNoDef "readInt32"        "Parse a string as an int32." (sigFn Types.string (Types.optional Types.int32)),
      primNoDef "readInt64"        "Parse a string as an int64." (sigFn Types.string (Types.optional Types.int64)),
      primNoDef "readInt8"         "Parse a string as an int8." (sigFn Types.string (Types.optional Types.int8)),
      primNoDef "readString"       "Parse a string-literal token to a plain string (Just) or Nothing on parse failure." (sigFn Types.string (Types.optional Types.string)),
      primNoDef "readUint16"       "Parse a string as a uint16." (sigFn Types.string (Types.optional Types.uint16)),
      primNoDef "readUint32"       "Parse a string as a uint32." (sigFn Types.string (Types.optional Types.uint32)),
      primNoDef "readUint64"       "Parse a string as a uint64." (sigFn Types.string (Types.optional Types.uint64)),
      primNoDef "readUint8"        "Parse a string as a uint8." (sigFn Types.string (Types.optional Types.uint8)),
      primNoDef "showBigint"       "Render a bigint as a string." (sigFn Types.bigint Types.string),
      primNoDef "showBoolean"      "Render a boolean as a string." (sigFn Types.boolean Types.string),
      primNoDef "showDecimal"      "Render a decimal as a string." (sigFn Types.decimal Types.string),
      primNoDef "showFloat32"      "Render a float32 as a string." (sigFn Types.float32 Types.string),
      primNoDef "showFloat64"      "Render a float64 as a string." (sigFn Types.float64 Types.string),
      primNoDef "showInt16"        "Render an int16 as a string." (sigFn Types.int16 Types.string),
      primNoDef "showInt32"        "Render an int32 as a string." (sigFn Types.int32 Types.string),
      primNoDef "showInt64"        "Render an int64 as a string." (sigFn Types.int64 Types.string),
      primNoDef "showInt8"         "Render an int8 as a string." (sigFn Types.int8 Types.string),
      primNoDef "showString"       "Render a string as a string-literal token (escaped and quoted)." (sigFn Types.string Types.string),
      primNoDef "showUint16"       "Render a uint16 as a string." (sigFn Types.uint16 Types.string),
      primNoDef "showUint32"       "Render a uint32 as a string." (sigFn Types.uint32 Types.string),
      primNoDef "showUint64"       "Render a uint64 as a string." (sigFn Types.uint64 Types.string),
      primNoDef "showUint8"        "Render a uint8 as a string." (sigFn Types.uint8 Types.string),
      primNoDef "stringToBinary"   "Convert a UTF-8 string to binary data." (sigFn Types.string Types.binary),
      primNoDef "uint16ToBigint"   "Convert a uint16 to a bigint." (sigFn Types.uint16 Types.bigint),
      primNoDef "uint32ToBigint"   "Convert a uint32 to a bigint." (sigFn Types.uint32 Types.bigint),
      primNoDef "uint64ToBigint"   "Convert a uint64 to a bigint." (sigFn Types.uint64 Types.bigint),
      primNoDef "uint8ToBigint"    "Convert a uint8 to a bigint." (sigFn Types.uint8 Types.bigint)]

primNoDef :: String -> String -> TermSignature -> Definition
primNoDef localName description s =
  toPrimitiveNoDefault description s (unqualifyName (QualifiedName (Just ns) localName))

sig :: TypeScheme -> TermSignature
sig = typeSchemeToTermSignature

-- Build a monomorphic signature `a -> b`.
sigFn :: Type -> Type -> TermSignature
sigFn a b = sig $ TypeScheme [] (a Types.~> b) Nothing
