module Hydra.Sources.Equality where

import Hydra.Kernel
import Hydra.Sources.Core
import Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Lib.Equality as Equality
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Maps as Maps
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
     el termEqualDef,
     el typeEqualDef]

eqcase tname fname eq = Case fname --> lambda "x" $
  match tname (Just Terms.false)
    [Case fname --> eq @@ var "x"]

floatEqualDef :: Definition (FloatValue -> FloatValue -> Bool)
floatEqualDef = equalityDefinition "floatEqual" $
  match _FloatValue Nothing [
    eqcase _FloatValue _FloatValue_bigfloat Equality.equalBigfloat,
    eqcase _FloatValue _FloatValue_float32 Equality.equalFloat32,
    eqcase _FloatValue _FloatValue_float64 Equality.equalFloat64]

integerEqualDef :: Definition (IntegerValue -> IntegerValue -> Bool)
integerEqualDef = equalityDefinition "integerEqual" $
  match _IntegerValue Nothing [
    eqcase _IntegerValue _IntegerValue_bigint Equality.equalBigint,
    eqcase _IntegerValue _IntegerValue_int8 Equality.equalInt8,
    eqcase _IntegerValue _IntegerValue_int16 Equality.equalInt16,
    eqcase _IntegerValue _IntegerValue_int32 Equality.equalInt32,
    eqcase _IntegerValue _IntegerValue_int64 Equality.equalInt64,
    eqcase _IntegerValue _IntegerValue_uint8 Equality.equalUint8,
    eqcase _IntegerValue _IntegerValue_uint16 Equality.equalUint16,
    eqcase _IntegerValue _IntegerValue_uint32 Equality.equalUint32,
    eqcase _IntegerValue _IntegerValue_uint64 Equality.equalUint64]

literalEqualDef :: Definition (Literal -> Literal -> Bool)
literalEqualDef = equalityDefinition "literalEqual" $
    doc "Test whether two literals are equal" $
    match _Literal Nothing [
      eqcase _Literal _Literal_binary Equality.equalBinary,
      eqcase _Literal _Literal_boolean Equality.equalBoolean,
      eqcase _Literal _Literal_float (ref floatEqualDef),
      eqcase _Literal _Literal_integer (ref integerEqualDef),
      eqcase _Literal _Literal_string Equality.equalString]

mapEq mapf eqf = lambda "t1" $ lambda "t2" $ eqf @@ (mapf @@ var "t1") @@ (mapf @@ var "t2")

nameEq = mapEq (unwrap _Name) Equality.equalString

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

typeEqualDef :: Definition (Type a -> Type a -> Bool)
typeEqualDef = equalityDefinition "typeEqual" $
    doc "Recursively test whether two types are equal" $
    function typeA (Types.function typeA Types.boolean) $
    match _Type Nothing [
      eqcase _Type _Type_annotated todo, -- TODO
      eqcase _Type _Type_application todo, -- TODO
      eqcase _Type _Type_function todo, -- TODO
      eqcase _Type _Type_lambda todo, -- TODO
      eqcase _Type _Type_list (ref typeEqualDef),
      eqcase _Type _Type_literal todo, -- TODO
      eqcase _Type _Type_map todo, -- TODO
      eqcase _Type _Type_optional (ref typeEqualDef),
      eqcase _Type _Type_product todo, -- TODO
      eqcase _Type _Type_record todo, -- TODO
      eqcase _Type _Type_set (ref typeEqualDef),
      eqcase _Type _Type_stream todo, -- TODO
      eqcase _Type _Type_sum todo, -- TODO
      eqcase _Type _Type_union todo, -- TODO
      eqcase _Type _Type_variable todo, -- TODO
      eqcase _Type _Type_wrap todo] -- TODO
  where
    typeA = Types.apply (TypeVariable _Type) (Types.var "a")
    todo = lambda "t1" $ lambda "t2" $ Datum Terms.false
