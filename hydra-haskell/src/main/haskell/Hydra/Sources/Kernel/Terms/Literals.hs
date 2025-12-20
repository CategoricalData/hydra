module Hydra.Sources.Kernel.Terms.Literals where

-- Standard imports for kernel terms modules
import Hydra.Kernel hiding (
  bigfloatToFloatValue, bigintToIntegerValue, floatValueToBigfloat, integerValueToBigint)
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Accessors     as Accessors
import qualified Hydra.Dsl.Annotations   as Annotations
import qualified Hydra.Dsl.Meta.Ast           as Ast
import qualified Hydra.Dsl.Bootstrap     as Bootstrap
import qualified Hydra.Dsl.Meta.Coders        as Coders
import qualified Hydra.Dsl.Meta.Compute       as Compute
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Grammar       as Grammar
import qualified Hydra.Dsl.Grammars      as Grammars
import qualified Hydra.Dsl.Meta.Graph         as Graph
import qualified Hydra.Dsl.Meta.Json          as Json
import qualified Hydra.Dsl.Meta.Lib.Chars     as Chars
import qualified Hydra.Dsl.Meta.Lib.Eithers   as Eithers
import qualified Hydra.Dsl.Meta.Lib.Equality  as Equality
import qualified Hydra.Dsl.Meta.Lib.Flows     as Flows
import qualified Hydra.Dsl.Meta.Lib.Lists     as Lists
import qualified Hydra.Dsl.Meta.Lib.Literals  as Literals
import qualified Hydra.Dsl.Meta.Lib.Logic     as Logic
import qualified Hydra.Dsl.Meta.Lib.Maps      as Maps
import qualified Hydra.Dsl.Meta.Lib.Math      as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes    as Maybes
import qualified Hydra.Dsl.Meta.Lib.Pairs     as Pairs
import qualified Hydra.Dsl.Meta.Lib.Sets      as Sets
import           Hydra.Dsl.Meta.Lib.Strings   as Strings
import qualified Hydra.Dsl.Literals      as Literals
import qualified Hydra.Dsl.LiteralTypes  as LiteralTypes
import qualified Hydra.Dsl.Meta.Base     as MetaBase
import qualified Hydra.Dsl.Meta.Terms    as MetaTerms
import qualified Hydra.Dsl.Meta.Types    as MetaTypes
import qualified Hydra.Dsl.Meta.Module        as Module
import           Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Prims         as Prims
import qualified Hydra.Dsl.Tabular       as Tabular
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Meta.Testing       as Testing
import qualified Hydra.Dsl.Tests         as Tests
import qualified Hydra.Dsl.Meta.Topology      as Topology
import qualified Hydra.Dsl.Types         as Types
import qualified Hydra.Dsl.Meta.Typing        as Typing
import qualified Hydra.Dsl.Meta.Util          as Util
import qualified Hydra.Dsl.Meta.Variants      as Variants
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


ns :: Namespace
ns = Namespace "hydra.literals"

module_ :: Module
module_ = Module ns elements
    []
    kernelTypesNamespaces $
    Just "Conversion functions for literal values."
  where
   elements = [
     toBinding bigfloatToFloatValue,
     toBinding bigintToIntegerValue,
     toBinding floatValueToBigfloat,
     toBinding integerValueToBigint]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

bigfloatToFloatValue :: TBinding (FloatType -> Bigfloat -> FloatValue)
bigfloatToFloatValue = define "bigfloatToFloatValue" $
  doc "Convert a bigfloat to a floating-point value of a given type (note: lossy)" $
  "ft" ~> "bf" ~> cases _FloatType (var "ft")
    Nothing [
    _FloatType_bigfloat>>: constant $ Core.floatValueBigfloat $ var "bf",
    _FloatType_float32>>: constant $ Core.floatValueFloat32 $ Literals.bigfloatToFloat32 $ var "bf",
    _FloatType_float64>>: constant $ Core.floatValueFloat64 $ Literals.bigfloatToFloat64 $ var "bf"]

bigintToIntegerValue :: TBinding (IntegerType -> Integer -> IntegerValue)
bigintToIntegerValue = define "bigintToIntegerValue" $
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

floatValueToBigfloat :: TBinding (FloatValue -> Bigfloat)
floatValueToBigfloat = define "floatValueToBigfloat" $
  doc "Convert a floating-point value of any precision to a bigfloat" $
  match _FloatValue
    Nothing [
    _FloatValue_bigfloat>>: "bf" ~> var "bf",
    _FloatValue_float32>>: "f32" ~> Literals.float32ToBigfloat $ var "f32",
    _FloatValue_float64>>: "f64" ~> Literals.float64ToBigfloat $ var "f64"]

integerValueToBigint :: TBinding (IntegerValue -> Integer)
integerValueToBigint = define "integerValueToBigint" $
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
