{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Printing where

import Hydra.Kernel
import Hydra.Sources.Basics
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Lib.Literals as Literals
import qualified Hydra.Dsl.Types as Types
import qualified Hydra.Dsl.Annotations as Ann

import Prelude hiding ((++))


hydraPrintingModule :: Module Kv
hydraPrintingModule = Module (Namespace "hydra/printing") elements [hydraBasicsModule] $
    Just "Utilities for use in transformations"
  where
   elements = [
     el describeFloatTypeDef,
     el describeIntegerTypeDef,
     el describeLiteralTypeDef,
     el describePrecisionDef,
     el describeTypeDef
     ]

printingDefinition :: String -> Datum a -> Definition a
printingDefinition = Definition . fromQname (moduleNamespace hydraPrintingModule)


describeFloatTypeDef :: Definition (FloatType -> String)
describeFloatTypeDef = printingDefinition "describeFloatType" $
  doc "Display a floating-point type as a string" $
  lambda "t" $ (ref describePrecisionDef <.> ref floatTypePrecisionDef @@ var "t") ++ string " floating-point numbers"

describeIntegerTypeDef :: Definition (IntegerType -> String)
describeIntegerTypeDef = printingDefinition "describeIntegerType" $
  doc "Display an integer type as a string" $
  lambda "t" $ (ref describePrecisionDef <.> ref integerTypePrecisionDef @@ var "t")
    ++ string " integers"

describeLiteralTypeDef :: Definition (LiteralType -> String)
describeLiteralTypeDef = printingDefinition "describeLiteralType" $
  doc "Display a literal type as a string" $
  match _LiteralType Nothing [
    Case _LiteralType_binary  --> constant $ string "binary strings",
    Case _LiteralType_boolean --> constant $ string "boolean values",
    Case _LiteralType_float   --> ref describeFloatTypeDef,
    Case _LiteralType_integer --> ref describeIntegerTypeDef,
    Case _LiteralType_string  --> constant $ string "character strings"]

describePrecisionDef :: Definition (Precision -> String)
describePrecisionDef = printingDefinition "describePrecision" $
  doc "Display numeric precision as a string" $
  match _Precision Nothing [
    Case _Precision_arbitrary --> constant $ string "arbitrary-precision",
    Case _Precision_bits      --> lambda "bits" $ showInt32 @@ var "bits" ++ string "-bit"]

describeTypeDef :: Definition (Type a -> String)
describeTypeDef = printingDefinition "describeType" $
  doc "Display a type as a string" $
  function (Types.apply (TypeVariable _Type) (Types.var "a")) Types.string $
    match _Type Nothing [
      Case _Type_annotated   --> lambda "a" $ string "annotated " ++ (ref describeTypeDef @@
        (project _Annotated _Annotated_subject @@ var "a")),
      Case _Type_application --> constant $ string "instances of an application type",
      Case _Type_literal     --> ref describeLiteralTypeDef,
      Case _Type_element     --> lambda "t" $ string "elements containing " ++ (ref describeTypeDef @@ var "t"),
      Case _Type_function    --> lambda "ft" $ string "functions from "
        ++ (ref describeTypeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft"))
        ++ string " to "
        ++ (ref describeTypeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft")),
      Case _Type_lambda      --> constant $ string "polymorphic terms",
      Case _Type_list        --> lambda "t" $ string "lists of " ++ (ref describeTypeDef @@ var "t"),
      Case _Type_map         --> lambda "mt" $ string "maps from "
        ++ (ref describeTypeDef @@ (project _MapType _MapType_keys @@ var "mt"))
        ++ string " to "
        ++ (ref describeTypeDef @@ (project _MapType _MapType_values  @@ var "mt")),
      Case _Type_wrap        --> lambda "name" $ string "alias for " ++ (unwrap _Name @@ var "name"),
      Case _Type_optional    --> lambda "ot" $ string "optional " ++ (ref describeTypeDef @@ var "ot"),
      Case _Type_product     --> constant $ string "tuples",
      Case _Type_record      --> constant $ string "records",
      Case _Type_set         --> lambda "st" $ string "sets of " ++ (ref describeTypeDef @@ var "st"),
      Case _Type_stream      --> lambda "t" $ string "streams of " ++ (ref describeTypeDef @@ var "t"),
      Case _Type_sum         --> constant $ string "variant tuples",
      Case _Type_union       --> constant $ string "unions",
      Case _Type_variable    --> constant $ string "instances of a named type"]
