module Hydra.Impl.Haskell.Sources.Adapters.Utils where

import Hydra.Common
import Hydra.Core
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Evaluation
import qualified Hydra.Graph as Graph
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Dsl.Lib.Literals as Literals

import Prelude hiding ((++))


adapterUtilsModule :: Result (Graph.Module Meta)
adapterUtilsModule = do
  g <- graph adapterUtilsName [
    el describeFloatType,
    el describeIntegerType,
    el describeLiteralType,
    el describePrecision,
    el describeType]
  deps <- sequence [hydraBasicsModule]
  return $ Graph.Module g deps

adapterUtilsName :: Graph.GraphName
adapterUtilsName = Graph.GraphName "hydra/adapters/utils"

utils :: String -> Data a -> Element a
utils = Element . fromQname adapterUtilsName


describeFloatType :: Element (FloatType -> String)
describeFloatType = utils "describeFloatType" $
  doc "Display a floating-point type as a string" $
  function (Types.nominal _FloatType) Types.string $
  lambda "t" $ (ref describePrecision <.> ref floatTypePrecision @@ var "t") ++ string " floating-point numbers"

describeIntegerType :: Element (IntegerType -> String)
describeIntegerType = utils "describeIntegerType" $
  doc "Display an integer type as a string" $
  function (Types.nominal _IntegerType) Types.string $
  lambda "t" $ (ref describePrecision <.> ref integerTypePrecision @@ var "t")
    ++ string " integers"

describeLiteralType :: Element (LiteralType -> String)
describeLiteralType = utils "describeLiteralType" $
  doc "Display a literal type as a string" $
  match (Types.nominal _LiteralType) Types.string [
    Case _LiteralType_binary  --> constant $ string "binary strings",
    Case _LiteralType_boolean --> constant $ string "boolean values",
    Case _LiteralType_float   --> ref describeFloatType,
    Case _LiteralType_integer --> ref describeIntegerType,
    Case _LiteralType_string  --> constant $ string "character strings"]

describePrecision :: Element (Precision -> String)
describePrecision = utils "describePrecision" $
  doc "Display numeric precision as a string" $
  match (Types.nominal _Precision) Types.string [
    Case _Precision_arbitrary --> constant $ string "arbitrary-precision",
    Case _Precision_bits      --> lambda "bits" $
      showInt32 @@ var "bits" ++ string "-bit"]

describeType :: Element (Type m -> string)
describeType = utils "describeType" $
  doc "Display a type as a string" $
  function (Types.universal "m" $ Types.nominal _Type) Types.string $
  lambda "typ" $ apply
    (match typeExprM Types.string [
      Case _TypeExpr_application --> constant $ string "instances of an application type",
      Case _TypeExpr_literal     --> ref describeLiteralType,
      Case _TypeExpr_element     --> lambda "t" $ string "elements containing " ++ (ref describeType @@ var "t"),
      Case _TypeExpr_function    --> lambda "ft" $ string "functions from "
        ++ (ref describeType @@ (project functionTypeM typeM _FunctionType_domain @@ var "ft"))
        ++ string " to "
        ++ (ref describeType @@ (project functionTypeM typeM _FunctionType_codomain @@ var "ft")),
      Case _TypeExpr_lambda      --> constant $ string "polymorphic terms",
      Case _TypeExpr_list        --> lambda "t" $ string "lists of " ++ (ref describeType @@ var "t"),
      Case _TypeExpr_map         --> lambda "mt" $ string "maps from "
        ++ (ref describeType @@ (project mapTypeM typeM _MapType_keys @@ var "mt"))
        ++ string " to "
        ++ (ref describeType @@ (project mapTypeM typeM _MapType_values  @@ var "mt")),
      Case _TypeExpr_nominal     --> lambda "name" $ string "alias for " ++ (denom _Name @@ var "name"),
      Case _TypeExpr_optional    --> lambda "ot" $ string "optional " ++ (ref describeType @@ var "ot"),
      Case _TypeExpr_record      --> constant $ string "records of a particular set of fields",
      Case _TypeExpr_set         --> lambda "st" $ string "sets of " ++ (ref describeType @@ var "st"),
      Case _TypeExpr_union       --> constant $ string "unions of a particular set of fields",
      Case _TypeExpr_variable    --> constant $ string "unspecified/parametric terms"])
    (project
      (Types.universal "m" $ Types.nominal _Type)
      (Types.universal "m" $ Types.nominal _TypeExpr)
      _Type_expr @@ var "typ")
  where
    typeExprM = Types.universal "m" $ Types.nominal _TypeExpr
    functionTypeM = Types.universal "m" $ Types.nominal _FunctionType
    typeM = Types.universal "m" $ Types.nominal _Type
    mapTypeM = Types.universal "m" $ Types.nominal _MapType

--idAdapter :: Element Meta
--idAdapter = standardFunction adapterUtilsName "idAdapter"
--  "An identity adapter for a given type"
--  (Types.nominal _Type) (TypeExprLambda (TypeLambda "m" $ ())) $
--  lambda "t" $ r_ _Adapter [
--    Field _Adapter_isLossy (boolean False),
--    Field _Adapter_source (var "t"),
--    Field _Adapter_target (var "t"),
--    Field _Adapter_step (ref idStep)]
