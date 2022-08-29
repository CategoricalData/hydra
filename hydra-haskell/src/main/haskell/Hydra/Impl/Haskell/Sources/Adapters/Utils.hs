module Hydra.Impl.Haskell.Sources.Adapters.Utils where

import Hydra.Common
import Hydra.Core
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Sources.Basics
import Hydra.Evaluation
import Hydra.Graph
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Dsl.Lib.Literals as Literals
import Hydra.Monads

import Prelude hiding ((++))


adapterUtilsModule :: GraphFlow Meta (Module Meta)
adapterUtilsModule = do
  g <- Standard.graph adapterUtilsName [
    el describeFloatType,
    el describeIntegerType,
    el describeLiteralType,
    el describePrecision,
    el describeType]
  deps <- sequence [hydraBasicsModule]
  return $ Module g deps

adapterUtilsName :: GraphName
adapterUtilsName = GraphName "hydra/adapters/utils"

utils :: String -> Datum a -> Definition a
utils = Definition . fromQname adapterUtilsName

describeFloatType :: Definition (FloatType -> String)
describeFloatType = utils "describeFloatType" $
  doc "Display a floating-point type as a string" $
  function (Types.nominal _FloatType) Types.string $
  lambda "t" $ (ref describePrecision <.> ref floatTypePrecision @@ var "t") ++ string " floating-point numbers"

describeIntegerType :: Definition (IntegerType -> String)
describeIntegerType = utils "describeIntegerType" $
  doc "Display an integer type as a string" $
  function (Types.nominal _IntegerType) Types.string $
  lambda "t" $ (ref describePrecision <.> ref integerTypePrecision @@ var "t")
    ++ string " integers"

describeLiteralType :: Definition (LiteralType -> String)
describeLiteralType = utils "describeLiteralType" $
  doc "Display a literal type as a string" $
  match _LiteralType Types.string [
    Case _LiteralType_binary  --> constant $ string "binary strings",
    Case _LiteralType_boolean --> constant $ string "boolean values",
    Case _LiteralType_float   --> ref describeFloatType,
    Case _LiteralType_integer --> ref describeIntegerType,
    Case _LiteralType_string  --> constant $ string "character strings"]

describePrecision :: Definition (Precision -> String)
describePrecision = utils "describePrecision" $
  doc "Display numeric precision as a string" $
  match _Precision Types.string [
    Case _Precision_arbitrary --> constant $ string "arbitrary-precision",
    Case _Precision_bits      --> lambda "bits" $
      showInt32 @@ var "bits" ++ string "-bit"]

describeType :: Definition (Type m -> string)
describeType = utils "describeType" $
  doc "Display a type as a string" $
  function (Types.apply (Types.nominal _Type) (Types.variable "m")) Types.string $
  lambda "typ" $ apply
    (match _Type Types.string [
      Case _Type_annotated   --> lambda "a" $ string "annotated " ++ (ref describeType @@
        (project _Annotated typeM _Annotated_subject @@ var "a")),
      Case _Type_application --> constant $ string "instances of an application type",
      Case _Type_literal     --> ref describeLiteralType,
      Case _Type_element     --> lambda "t" $ string "elements containing " ++ (ref describeType @@ var "t"),
      Case _Type_function    --> lambda "ft" $ string "functions from "
        ++ (ref describeType @@ (project _FunctionType typeM _FunctionType_domain @@ var "ft"))
        ++ string " to "
        ++ (ref describeType @@ (project _FunctionType typeM _FunctionType_codomain @@ var "ft")),
      Case _Type_lambda      --> constant $ string "polymorphic terms",
      Case _Type_list        --> lambda "t" $ string "lists of " ++ (ref describeType @@ var "t"),
      Case _Type_map         --> lambda "mt" $ string "maps from "
        ++ (ref describeType @@ (project _MapType typeM _MapType_keys @@ var "mt"))
        ++ string " to "
        ++ (ref describeType @@ (project _MapType typeM _MapType_values  @@ var "mt")),
      Case _Type_nominal     --> lambda "name" $ string "alias for " ++ (denom _Name @@ var "name"),
      Case _Type_optional    --> lambda "ot" $ string "optional " ++ (ref describeType @@ var "ot"),
      Case _Type_record      --> constant $ string "records of a particular set of fields",
      Case _Type_set         --> lambda "st" $ string "sets of " ++ (ref describeType @@ var "st"),
      Case _Type_union       --> constant $ string "unions of a particular set of fields",
      Case _Type_variable    --> constant $ string "unspecified/parametric terms"])
    (var "typ")
  where
    annotatedTypeM = Types.apply (Types.apply (Types.nominal _Annotated) (Types.apply (Types.nominal _Type) (Types.variable "m"))) (Types.variable "m")
    functionTypeM = Types.apply (Types.nominal _FunctionType) (Types.variable "m")
    typeM = Types.apply (Types.nominal _Type) (Types.variable "m")
    mapTypeM = Types.apply (Types.nominal _MapType) (Types.variable "m")
