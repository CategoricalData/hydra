module Hydra.Sources.Equality where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import Prelude hiding ((++))


equalityDefinition :: String -> Datum a -> Definition a
equalityDefinition = definitionInModule hydraEqualityModule

hydraEqualityModule :: Module Kv
hydraEqualityModule = Module (Namespace "hydra/equality") elements [hydraCoreModule] $
    Just "Functions for determining equality of types and terms"
  where
   elements = [
     el floatEqualDef,
     el integerEqualDef,
     el literalEqualDef,
     el termEqualDef]

eqcase tname fname eq = Case fname --> lambda "x" $
  match tname (Just Terms.false)
    [Case fname --> eq @@ var "x"]

floatEqualDef :: Definition (FloatValue -> FloatValue -> Bool)
floatEqualDef = equalityDefinition "floatEqual" $
  match _FloatValue Nothing [
    eqcase _FloatValue _FloatValue_bigfloat Literals.equalBigfloat,
    eqcase _FloatValue _FloatValue_float32 Literals.equalFloat32,
    eqcase _FloatValue _FloatValue_float64 Literals.equalFloat64]

integerEqualDef :: Definition (IntegerValue -> IntegerValue -> Bool)
integerEqualDef = equalityDefinition "integerEqual" $
  match _IntegerValue Nothing [
    eqcase _IntegerValue _IntegerValue_bigint Literals.equalBigint,
    eqcase _IntegerValue _IntegerValue_int8 Literals.equalInt8,
    eqcase _IntegerValue _IntegerValue_int16 Literals.equalInt16,
    eqcase _IntegerValue _IntegerValue_int32 Literals.equalInt32,
    eqcase _IntegerValue _IntegerValue_int64 Literals.equalInt64,
    eqcase _IntegerValue _IntegerValue_uint8 Literals.equalUint8,
    eqcase _IntegerValue _IntegerValue_uint16 Literals.equalUint16,
    eqcase _IntegerValue _IntegerValue_uint32 Literals.equalUint32,
    eqcase _IntegerValue _IntegerValue_uint64 Literals.equalUint64]

literalEqualDef :: Definition (Literal -> Literal -> Bool)
literalEqualDef = equalityDefinition "literalEqual" $
    doc "Test whether two literals are equal" $
    match _Literal Nothing [
      eqcase _Literal _Literal_binary Literals.equalBinary,
      eqcase _Literal _Literal_boolean Literals.equalBoolean,
      eqcase _Literal _Literal_float (ref floatEqualDef),
      eqcase _Literal _Literal_integer (ref integerEqualDef),
      eqcase _Literal _Literal_string Literals.equalString]

termEqualDef :: Definition (Term a -> Term a -> Bool)
termEqualDef = equalityDefinition "termEqual" $
    doc "Recursively test whether two terms are equal" $
    function termA (Types.function termA Types.boolean) $
    match _Term Nothing [
      Case _Term_annotated   --> todo, -- TODO
      Case _Term_application --> todo, -- TODO
      Case _Term_function    --> todo, -- TODO
      Case _Term_let         --> todo, -- TODO
      Case _Term_list        --> todo, -- TODO
      eqcase _Term _Term_literal (ref literalEqualDef),
      Case _Term_map         --> todo, -- TODO
      Case _Term_optional    --> todo, -- TODO
      Case _Term_product     --> todo, -- TODO
      Case _Term_record      --> todo, -- TODO
      Case _Term_set         --> todo, -- TODO
      Case _Term_stream      --> todo, -- TODO
      Case _Term_sum         --> todo, -- TODO
      Case _Term_union       --> todo, -- TODO
      Case _Term_variable    --> todo, -- TODO
      Case _Term_wrap        --> todo] -- TODO
  where
    termA = Types.apply (TypeVariable _Term) (Types.var "a")
    todo = constant $ constant $ Datum Terms.false
