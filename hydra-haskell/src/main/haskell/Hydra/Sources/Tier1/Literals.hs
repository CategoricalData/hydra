module Hydra.Sources.Tier1.Literals where

-- Standard term-level Tier-1 imports
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Io        as Io
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Sets      as Sets
import           Hydra.Dsl.Lib.Strings   as Strings
import           Hydra.Dsl.Phantoms      as Phantoms
import qualified Hydra.Dsl.Terms         as Terms
import qualified Hydra.Dsl.Types         as Types
import           Hydra.Sources.Tier0.Core
import           Prelude hiding ((++))
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y


literalsDefinition :: String -> TTerm a -> TElement a
literalsDefinition = definitionInModule hydraLiteralsModule

hydraLiteralsModule :: Module
hydraLiteralsModule = Module (Namespace "hydra.literals") elements [] [hydraCoreModule] $
    Just "Conversion functions for literal values."
  where
   elements = [
     el floatValueToBigfloatDef,
     el integerValueToBigintDef]

floatValueToBigfloatDef :: TElement (Double -> Double)
floatValueToBigfloatDef = literalsDefinition "floatValueToBigfloat" $
  doc "Convert a floating-point value of any precision to a bigfloat" $
  match _FloatValue Nothing [
    _FloatValue_bigfloat>>: lambda "f" $ Equality.identity $ var "f",
    _FloatValue_float32>>: asFunction Literals.float32ToBigfloat,
    _FloatValue_float64>>: asFunction Literals.float64ToBigfloat]

integerValueToBigintDef :: TElement (IntegerValue -> Integer)
integerValueToBigintDef = literalsDefinition "integerValueToBigint" $
  doc "Convert an integer value of any precision to a bigint" $
  match _IntegerValue Nothing [
    _IntegerValue_bigint>>: lambda "i" $ Equality.identity $ var "i",
    _IntegerValue_int8>>: asFunction Literals.int8ToBigint,
    _IntegerValue_int16>>: asFunction Literals.int16ToBigint,
    _IntegerValue_int32>>: asFunction Literals.int32ToBigint,
    _IntegerValue_int64>>: asFunction Literals.int64ToBigint,
    _IntegerValue_uint8>>: asFunction Literals.uint8ToBigint,
    _IntegerValue_uint16>>: asFunction Literals.uint16ToBigint,
    _IntegerValue_uint32>>: asFunction Literals.uint32ToBigint,
    _IntegerValue_uint64>>: asFunction Literals.uint64ToBigint]
