module Hydra.Sources.Kernel.Terms.Literals where

-- Standard Tier-2 imports
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
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
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


module_ :: Module
module_ = Module (Namespace "hydra.literals") elements [] [KernelTypes.hydraCoreModule] $
    Just "Conversion functions for literal values."
  where
   elements = [
     el floatValueToBigfloatDef,
     el integerValueToBigintDef]

define :: String -> TTerm a -> TElement a
define = definitionInModule module_

floatValueToBigfloatDef :: TElement (Double -> Double)
floatValueToBigfloatDef = define "floatValueToBigfloat" $
  doc "Convert a floating-point value of any precision to a bigfloat" $
  match _FloatValue Nothing [
    _FloatValue_bigfloat>>: lambda "f" $ Equality.identity $ var "f",
    _FloatValue_float32>>: unaryFunction Literals.float32ToBigfloat,
    _FloatValue_float64>>: unaryFunction Literals.float64ToBigfloat]

integerValueToBigintDef :: TElement (IntegerValue -> Integer)
integerValueToBigintDef = define "integerValueToBigint" $
  doc "Convert an integer value of any precision to a bigint" $
  match _IntegerValue Nothing [
    _IntegerValue_bigint>>: lambda "i" $ Equality.identity $ var "i",
    _IntegerValue_int8>>: unaryFunction Literals.int8ToBigint,
    _IntegerValue_int16>>: unaryFunction Literals.int16ToBigint,
    _IntegerValue_int32>>: unaryFunction Literals.int32ToBigint,
    _IntegerValue_int64>>: unaryFunction Literals.int64ToBigint,
    _IntegerValue_uint8>>: unaryFunction Literals.uint8ToBigint,
    _IntegerValue_uint16>>: unaryFunction Literals.uint16ToBigint,
    _IntegerValue_uint32>>: unaryFunction Literals.uint32ToBigint,
    _IntegerValue_uint64>>: unaryFunction Literals.uint64ToBigint]
