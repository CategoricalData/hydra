module Hydra.Sources.Adapters.Utils where

import Hydra.Kernel
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Basics
import qualified Hydra.Dsl.Standard as Standard
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Literals as Literals

import Prelude hiding ((++))


utilsNs = Namespace "hydra/adapters/utils"

adapterUtilsModule :: Module Kv
adapterUtilsModule = Module utilsNs elements [hydraBasicsModule] $
    Just "Utilities for use in transformations"
  where
   elements = [
     el describeFloatTypeSource,
     el describeIntegerTypeSource,
     el describeLiteralTypeSource,
     el describePrecisionSource,
     el describeTypeSource]

utils :: String -> Datum a -> Definition a
utils = Definition . fromQname utilsNs

describeFloatTypeSource :: Definition (FloatType -> String)
describeFloatTypeSource = utils "describeFloatType" $
  doc "Display a floating-point type as a string" $
  function (Types.wrap _FloatType) Types.string $
  lambda "t" $ (ref describePrecisionSource <.> ref floatTypePrecisionSource @@ var "t") ++ string " floating-point numbers"

describeIntegerTypeSource :: Definition (IntegerType -> String)
describeIntegerTypeSource = utils "describeIntegerType" $
  doc "Display an integer type as a string" $
  function (Types.wrap _IntegerType) Types.string $
  lambda "t" $ (ref describePrecisionSource <.> ref integerTypePrecisionSource @@ var "t")
    ++ string " integers"

describeLiteralTypeSource :: Definition (LiteralType -> String)
describeLiteralTypeSource = utils "describeLiteralType" $
  doc "Display a literal type as a string" $
  match _LiteralType Types.string [
    Case _LiteralType_binary  --> constant $ string "binary strings",
    Case _LiteralType_boolean --> constant $ string "boolean values",
    Case _LiteralType_float   --> ref describeFloatTypeSource,
    Case _LiteralType_integer --> ref describeIntegerTypeSource,
    Case _LiteralType_string  --> constant $ string "character strings"]

describePrecisionSource :: Definition (Precision -> String)
describePrecisionSource = utils "describePrecision" $
  doc "Display numeric precision as a string" $
  match _Precision Types.string [
    Case _Precision_arbitrary --> constant $ string "arbitrary-precision",
    Case _Precision_bits      --> lambda "bits" $
      showInt32 @@ var "bits" ++ string "-bit"]

describeTypeSource :: Definition (Type m -> string)
describeTypeSource = utils "describeType" $
  doc "Display a type as a string" $
  function (Types.apply (Types.wrap _Type) (Types.variable "m")) Types.string $
  lambda "typ" $ apply
    (match _Type Types.string [
      Case _Type_annotated   --> lambda "a" $ string "annotated " ++ (ref describeTypeSource @@
        (project _Annotated typeM _Annotated_subject @@ var "a")),
      Case _Type_application --> constant $ string "instances of an application type",
      Case _Type_literal     --> ref describeLiteralTypeSource,
      Case _Type_element     --> lambda "t" $ string "elements containing " ++ (ref describeTypeSource @@ var "t"),
      Case _Type_function    --> lambda "ft" $ string "functions from "
        ++ (ref describeTypeSource @@ (project _FunctionType typeM _FunctionType_domain @@ var "ft"))
        ++ string " to "
        ++ (ref describeTypeSource @@ (project _FunctionType typeM _FunctionType_codomain @@ var "ft")),
      Case _Type_lambda      --> constant $ string "polymorphic terms",
      Case _Type_list        --> lambda "t" $ string "lists of " ++ (ref describeTypeSource @@ var "t"),
      Case _Type_map         --> lambda "mt" $ string "maps from "
        ++ (ref describeTypeSource @@ (project _MapType typeM _MapType_keys @@ var "mt"))
        ++ string " to "
        ++ (ref describeTypeSource @@ (project _MapType typeM _MapType_values  @@ var "mt")),
      Case _Type_wrap     --> lambda "name" $ string "alias for " ++ (denom _Name @@ var "name"),
      Case _Type_optional    --> lambda "ot" $ string "optional " ++ (ref describeTypeSource @@ var "ot"),
      Case _Type_product     --> constant $ string "tuples",
      Case _Type_record      --> constant $ string "records",
      Case _Type_set         --> lambda "st" $ string "sets of " ++ (ref describeTypeSource @@ var "st"),
      Case _Type_stream      --> lambda "t" $ string "streams of " ++ (ref describeTypeSource @@ var "t"),
      Case _Type_sum         --> constant $ string "variant tuples",
      Case _Type_union       --> constant $ string "unions",
      Case _Type_variable    --> constant $ string "unspecified/parametric terms"])
    (var "typ")
  where
    annotatedTypeM = Types.apply (Types.apply (Types.wrap _Annotated) (Types.apply (Types.wrap _Type) (Types.variable "m"))) (Types.variable "m")
    functionTypeM = Types.apply (Types.wrap _FunctionType) (Types.variable "m")
    typeM = Types.apply (Types.wrap _Type) (Types.variable "m")
    mapTypeM = Types.apply (Types.wrap _MapType) (Types.variable "m")
