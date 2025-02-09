{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier2.Printing where

-- Standard Tier-2 imports
import           Prelude hiding ((++))
import qualified Data.List                 as L
import qualified Data.Map                  as M
import qualified Data.Set                  as S
import qualified Data.Maybe                as Y
import           Hydra.Dsl.Base            as Base
import qualified Hydra.Dsl.Core            as Core
import qualified Hydra.Dsl.Graph           as Graph
import qualified Hydra.Dsl.Lib.Equality    as Equality
import qualified Hydra.Dsl.Lib.Flows       as Flows
import qualified Hydra.Dsl.Lib.Io          as Io
import qualified Hydra.Dsl.Lib.Lists       as Lists
import qualified Hydra.Dsl.Lib.Literals    as Literals
import qualified Hydra.Dsl.Lib.Logic       as Logic
import qualified Hydra.Dsl.Lib.Maps        as Maps
import qualified Hydra.Dsl.Lib.Math        as Math
import qualified Hydra.Dsl.Lib.Optionals   as Optionals
import qualified Hydra.Dsl.Lib.Sets        as Sets
import           Hydra.Dsl.Lib.Strings     as Strings
import qualified Hydra.Dsl.Module          as Module
import qualified Hydra.Dsl.Terms           as Terms
import qualified Hydra.Dsl.Types           as Types
import           Hydra.Sources.Tier1.All

import Hydra.Sources.Tier2.Variants


hydraPrintingModule :: Module
hydraPrintingModule = Module (Namespace "hydra/printing") elements
    [hydraVariantsModule]
    [hydraCoreModule] $
    Just "Utilities for use in transformations"
  where
   elements = [
     el describeFloatTypeDef,
     el describeIntegerTypeDef,
     el describeLiteralTypeDef,
     el describePrecisionDef,
     el describeTypeDef]

printingDefinition :: String -> TTerm a -> TElement a
printingDefinition = definitionInModule hydraPrintingModule


describeFloatTypeDef :: TElement (FloatType -> String)
describeFloatTypeDef = printingDefinition "describeFloatType" $
  doc "Display a floating-point type as a string" $
  function floatTypeT tString $
  lambda "t" $ (ref describePrecisionDef <.> ref floatTypePrecisionDef @@ var "t") ++ string " floating-point numbers"

describeIntegerTypeDef :: TElement (IntegerType -> String)
describeIntegerTypeDef = printingDefinition "describeIntegerType" $
  doc "Display an integer type as a string" $
  function integerTypeT tString $
  lambda "t" $ (ref describePrecisionDef <.> ref integerTypePrecisionDef @@ var "t")
    ++ string " integers"

describeLiteralTypeDef :: TElement (LiteralType -> String)
describeLiteralTypeDef = printingDefinition "describeLiteralType" $
  doc "Display a literal type as a string" $
  function literalTypeT tString $
  match _LiteralType Nothing [
    TCase _LiteralType_binary  --> constant $ string "binary strings",
    TCase _LiteralType_boolean --> constant $ string "boolean values",
    TCase _LiteralType_float   --> ref describeFloatTypeDef,
    TCase _LiteralType_integer --> ref describeIntegerTypeDef,
    TCase _LiteralType_string  --> constant $ string "character strings"]

describePrecisionDef :: TElement (Precision -> String)
describePrecisionDef = printingDefinition "describePrecision" $
  doc "Display numeric precision as a string" $
  function precisionT tString $
  match _Precision Nothing [
    TCase _Precision_arbitrary --> constant $ string "arbitrary-precision",
    TCase _Precision_bits      --> lambda "bits" $ Literals.showInt32 @@ var "bits" ++ string "-bit"]

describeTypeDef :: TElement (Type -> String)
describeTypeDef = printingDefinition "describeType" $
  doc "Display a type as a string" $
  function typeT tString $
    match _Type Nothing [
      TCase _Type_annotated   --> lambda "a" $ string "annotated " ++ (ref describeTypeDef @@
        (project _AnnotatedType _AnnotatedType_subject @@ var "a")),
      TCase _Type_application --> constant $ string "instances of an application type",
      TCase _Type_literal     --> ref describeLiteralTypeDef,
      TCase _Type_function    --> lambda "ft" $ string "functions from "
        ++ (ref describeTypeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft"))
        ++ string " to "
        ++ (ref describeTypeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft")),
      TCase _Type_lambda      --> constant $ string "polymorphic terms",
      TCase _Type_list        --> lambda "t" $ string "lists of " ++ (ref describeTypeDef @@ var "t"),
      TCase _Type_map         --> lambda "mt" $ string "maps from "
        ++ (ref describeTypeDef @@ (project _MapType _MapType_keys @@ var "mt"))
        ++ string " to "
        ++ (ref describeTypeDef @@ (project _MapType _MapType_values  @@ var "mt")),
      TCase _Type_optional    --> lambda "ot" $ string "optional " ++ (ref describeTypeDef @@ var "ot"),
      TCase _Type_product     --> constant $ string "tuples",
      TCase _Type_record      --> constant $ string "records",
      TCase _Type_set         --> lambda "st" $ string "sets of " ++ (ref describeTypeDef @@ var "st"),
      TCase _Type_sum         --> constant $ string "variant tuples",
      TCase _Type_union       --> constant $ string "unions",
      TCase _Type_variable    --> constant $ string "instances of a named type",
      TCase _Type_wrap        --> lambda "n" $ string "wrapper for "
        ++ (ref describeTypeDef @@ (project _WrappedType _WrappedType_object @@ var "n"))]
