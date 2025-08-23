module Hydra.Sources.Kernel.Terms.Literals where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import qualified Hydra.Dsl.Mantle        as Mantle
import qualified Hydra.Dsl.Module        as Module
import qualified Hydra.Dsl.TTerms        as TTerms
import qualified Hydra.Dsl.TTypes        as TTypes
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Typing        as Typing
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.literals") elements
    []
    kernelTypesModules $
    Just "Conversion functions for literal values."
  where
   elements = [
     el bigfloatToFloatValueDef,
     el bigintToIntegerValueDef,
     el floatValueToBigfloatDef,
     el integerValueToBigintDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

bigfloatToFloatValueDef :: TElement (FloatType -> Bigfloat -> FloatValue)
bigfloatToFloatValueDef = define "bigfloatToFloatValue" $
  doc "Convert a bigfloat to a floating-point value of a given type (note: lossy)" $
  "ft" ~> "bf" ~> cases _FloatType (var "ft")
    Nothing [
    _FloatType_bigfloat>>: constant $ Core.floatValueBigfloat $ var "bf",
    _FloatType_float32>>: constant $ Core.floatValueFloat32 $ Literals.bigfloatToFloat32 $ var "bf",
    _FloatType_float64>>: constant $ Core.floatValueFloat64 $ Literals.bigfloatToFloat64 $ var "bf"]

bigintToIntegerValueDef :: TElement (IntegerType -> Integer -> IntegerValue)
bigintToIntegerValueDef = define "bigintToIntegerValue" $
  doc "Convert a bigint to an integer value of a given type (note: lossy)" $
  "it" ~> "bi" ~> cases _IntegerType (var "it")
    Nothing [
    _IntegerType_bigint>>: constant $ Core.integerValueBigint $ var "bi",
    _IntegerType_int8>>: constant $ Core.integerValueInt8 $ Literals.bigintToInt8 $ var "bi",
    _IntegerType_int16>>: constant $ Core.integerValueInt16 $ Literals.bigintToInt16 $ var "bi",
    _IntegerType_int32>>: constant $ Core.integerValueInt32 $ Literals.bigintToInt32 $ var "bi",
    _IntegerType_int64>>: constant $ Core.integerValueInt64 $ Literals.bigintToInt64 $ var "bi",
    _IntegerType_uint8>>: constant $ Core.integerValueUint8 $ Literals.bigintToUint8 $ var "bi",
    _IntegerType_uint16>>: constant $ Core.integerValueUint16 $ Literals.bigintToUint16 $ var "bi",
    _IntegerType_uint32>>: constant $ Core.integerValueUint32 $ Literals.bigintToUint32 $ var "bi",
    _IntegerType_uint64>>: constant $ Core.integerValueUint64 $ Literals.bigintToUint64 $ var "bi"]

floatValueToBigfloatDef :: TElement (FloatValue -> Bigfloat)
floatValueToBigfloatDef = define "floatValueToBigfloat" $
  doc "Convert a floating-point value of any precision to a bigfloat" $
  match _FloatValue
    Nothing [
    _FloatValue_bigfloat>>: "bf" ~> var "bf",
    _FloatValue_float32>>: "f32" ~> Literals.float32ToBigfloat $ var "f32",
    _FloatValue_float64>>: "f64" ~> Literals.float64ToBigfloat $ var "f64"]

integerValueToBigintDef :: TElement (IntegerValue -> Integer)
integerValueToBigintDef = define "integerValueToBigint" $
  doc "Convert an integer value of any precision to a bigint" $
  match _IntegerValue
    Nothing [
    _IntegerValue_bigint>>: "bi" ~> var "bi",
    _IntegerValue_int8>>: "i8" ~> Literals.int8ToBigint $ var "i8",
    _IntegerValue_int16>>: "i16" ~> Literals.int16ToBigint $ var "i16",
    _IntegerValue_int32>>: "i32" ~> Literals.int32ToBigint $ var "i32",
    _IntegerValue_int64>>: "i64" ~> Literals.int64ToBigint $ var "i64",
    _IntegerValue_uint8>>: "ui8" ~> Literals.uint8ToBigint $ var "ui8",
    _IntegerValue_uint16>>: "ui16" ~> Literals.uint16ToBigint $ var "ui16",
    _IntegerValue_uint32>>: "ui32" ~> Literals.uint32ToBigint $ var "ui32",
    _IntegerValue_uint64>>: "ui64" ~> Literals.uint64ToBigint $ var "ui64"]
