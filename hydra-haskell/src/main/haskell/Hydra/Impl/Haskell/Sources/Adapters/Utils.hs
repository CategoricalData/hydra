module Hydra.Impl.Haskell.Sources.Adapters.Utils where

import Hydra.Common
import Hydra.Core
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Evaluation
import Hydra.Graph as Graph
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings
import Hydra.Impl.Haskell.Dsl.Lib.Literals as Literals


adapterUtilsModule :: Result (Module Meta)
adapterUtilsModule = do
  g <- graph hydraBasicsName [
    el describeFloatType,
    el describeIntegerType,
    el describeLiteralType,
    el describePrecision,
    el describeType]
  deps <- sequence [hydraBasicsModule]
  return $ Module g deps

adapterUtilsName :: GraphName
adapterUtilsName = GraphName "hydra/adapters/utils"

utils :: String -> Trm a -> El a
utils = El . fromQname adapterUtilsName


describeFloatType :: El (FloatType -> String)
describeFloatType = utils "describeFloatType" $
  doc "Display a floating-point type as a string" $
  typed (Types.function (Types.nominal _FloatType) Types.string) $
  lambda "t" $ (at describePrecision @. at floatTypePrecision .$ var "t") .++ string " floating-point numbers"

describeIntegerType :: El (IntegerType -> String)
describeIntegerType = utils "describeIntegerType" $
  doc "Display an integer type as a string" $
  typed (Types.function (Types.nominal _IntegerType) Types.string) $
  lambda "t" $ (at describePrecision @. at integerTypePrecision .$ var "t") .++ string " integers"

describeLiteralType :: El (LiteralType -> String)
describeLiteralType = utils "describeLiteralType" $
  doc "Display a literal type as a string" $
  matchSafe (Types.nominal _LiteralType) Types.string [
    Case _LiteralType_binary  .-> constant $ string "binary strings",
    Case _LiteralType_boolean .-> constant $ string "boolean values",
    Case _LiteralType_float   .-> at describeFloatType,
    Case _LiteralType_integer .-> at describeIntegerType,
    Case _LiteralType_string  .-> constant $ string "character strings"]

describePrecision :: El (Precision -> String)
describePrecision = utils "describePrecision" $
  doc "Display numeric precision as a string" $
  matchSafe (Types.nominal _Precision) Types.string [
    Case _Precision_arbitrary .-> constant $ string "arbitrary-precision",
    Case _Precision_bits      .-> lambda "bits" $ cat .$ list [
      showInt32 .$ var "bits",
      string "-bit"]]

describeType :: El (Type m -> string)
describeType = utils "describeType" $
  doc "Display a type as a string" $
  typed (Types.function (Types.universal "m" $ Types.nominal _Type) Types.string) $
  lambda "typ" $ apply
    (matchSafe typeExprM Types.string [
      Case _TypeExpr_literal   .-> at describeLiteralType,
      Case _TypeExpr_element   .-> lambda "t" $ string "elements containing " .++ (at describeType .$ var "t"),
      Case _TypeExpr_function  .-> lambda "ft" $ string "functions from "
        .++ (at describeType .$ (project functionTypeM typeM _FunctionType_domain .$ var "ft"))
        .++ string " to "
        .++ (at describeType .$ (project functionTypeM typeM _FunctionType_codomain .$ var "ft")),
      Case _TypeExpr_list      .-> lambda "t" $ string "lists of " .++ (at describeType .$ var "t"),
      Case _TypeExpr_map       .-> lambda "mt" $ string "maps from "
        .++ (at describeType .$ (project mapTypeM typeM _MapType_keys .$ var "mt"))
        .++ string " to "
        .++ (at describeType .$ (project mapTypeM typeM _MapType_values  .$ var "mt")),
      Case _TypeExpr_nominal   .-> lambda "name" $ string "alias for " .++ (denom _Name .$ var "name"),
      Case _TypeExpr_optional  .-> lambda "ot" $ string "optional " .++ (at describeType .$ var "ot"),
      Case _TypeExpr_record    .->constant $ string "records of a particular set of fields",
      Case _TypeExpr_set       .-> lambda "st" $ string "sets of " .++ (at describeType .$ var "st"),
      Case _TypeExpr_union     .-> constant $ string "unions of a particular set of fields",
      Case _TypeExpr_universal .-> constant $ string "polymorphic terms",
      Case _TypeExpr_variable  .-> constant $ string "unspecified/parametric terms"])
    (project
      (Types.universal "m" $ Types.nominal _Type)
      (Types.universal "m" $ Types.nominal _TypeExpr)
      _Type_expr .$ var "typ")
  where
    typeExprM = Types.universal "m" $ Types.nominal _TypeExpr
    functionTypeM = Types.universal "m" $ Types.nominal _FunctionType
    typeM = Types.universal "m" $ Types.nominal _Type
    mapTypeM = Types.universal "m" $ Types.nominal _MapType

--idAdapter :: Element Meta
--idAdapter = standardFunction adapterUtilsName "idAdapter"
--  "An identity adapter for a given type"
--  (Types.nominal _Type) (TypeExprUniversal (UniversalType "m" $ ())) $
--  lambda "t" $ r_ _Adapter [
--    Field _Adapter_isLossy (booleanValue False),
--    Field _Adapter_source (var "t"),
--    Field _Adapter_target (var "t"),
--    Field _Adapter_step (at idStep)]
