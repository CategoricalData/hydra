{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Terms.Describe.Core where

-- Standard imports for term-level kernel modules
import Hydra.Kernel
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Accessors     as Accessors
import qualified Hydra.Dsl.Ast           as Ast
import qualified Hydra.Dsl.Coders        as Coders
import qualified Hydra.Dsl.Compute       as Compute
import qualified Hydra.Dsl.Core          as Core
import qualified Hydra.Dsl.Grammar       as Grammar
import qualified Hydra.Dsl.Graph         as Graph
import qualified Hydra.Dsl.Json          as Json
import qualified Hydra.Dsl.Lib.Chars     as Chars
import qualified Hydra.Dsl.Lib.Equality  as Equality
import qualified Hydra.Dsl.Lib.Flows     as Flows
import qualified Hydra.Dsl.Lib.Lists     as Lists
import qualified Hydra.Dsl.Lib.Literals  as Literals
import qualified Hydra.Dsl.Lib.Logic     as Logic
import qualified Hydra.Dsl.Lib.Maps      as Maps
import qualified Hydra.Dsl.Lib.Math      as Math
import qualified Hydra.Dsl.Lib.Maybes    as Maybes
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
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++))
import qualified Data.Int                as I
import qualified Data.List               as L
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Maybe              as Y

import qualified Hydra.Sources.Kernel.Terms.Describe.Mantle as DescribeMantle
import qualified Hydra.Sources.Kernel.Terms.Variants as Variants


module_ :: Module
module_ = Module (Namespace "hydra.describe.core") elements
    [DescribeMantle.module_, Variants.module_]
    kernelTypesModules $
    Just "Natural-language descriptions for hydra.core types"
  where
   elements = [
     el floatTypeDef,
     el integerTypeDef,
     el literalTypeDef,
     el typeDef]

define :: String -> TTerm a -> TBinding a
define = definitionInModule module_

floatTypeDef :: TBinding (FloatType -> String)
floatTypeDef = define "floatType" $
  doc "Display a floating-point type as a string" $
  lambda "t" $ (ref DescribeMantle.precisionDef <.> ref Variants.floatTypePrecisionDef @@ var "t") ++ string " floating-point number"

integerTypeDef :: TBinding (IntegerType -> String)
integerTypeDef = define "integerType" $
  doc "Display an integer type as a string" $
  lambda "t" $ (ref DescribeMantle.precisionDef <.> ref Variants.integerTypePrecisionDef @@ var "t")
    ++ string " integer"

literalTypeDef :: TBinding (LiteralType -> String)
literalTypeDef = define "literalType" $
  doc "Display a literal type as a string" $
  match _LiteralType Nothing [
    _LiteralType_binary>>: constant $ string "binary string",
    _LiteralType_boolean>>: constant $ string "boolean value",
    _LiteralType_float>>: ref floatTypeDef,
    _LiteralType_integer>>: ref integerTypeDef,
    _LiteralType_string>>: constant $ string "character string"]

typeDef :: TBinding (Type -> String)
typeDef = define "type" $
  doc "Display a type as a string" $
  match _Type Nothing [
    _Type_annotated>>: lambda "a" $ string "annotated " ++ (ref typeDef @@
      (project _AnnotatedType _AnnotatedType_body @@ var "a")),
    _Type_application>>: lambda "at" $ Strings.cat $ list [
      ref typeDef @@ (Core.applicationTypeFunction $ var "at"),
      string " applied to ",
      ref typeDef @@ (Core.applicationTypeArgument $ var "at")],
    _Type_literal>>: ref literalTypeDef,
    _Type_function>>: lambda "ft" $ string "function from "
      ++ (ref typeDef @@ (project _FunctionType _FunctionType_domain @@ var "ft"))
      ++ string " to "
      ++ (ref typeDef @@ (project _FunctionType _FunctionType_codomain @@ var "ft")),
    _Type_forall>>: lambda "fat" $ Strings.cat2 (string "polymorphic ") (ref typeDef @@ (Core.forallTypeBody $ var "fat")),
    _Type_list>>: lambda "t" $ string "list of " ++ (ref typeDef @@ var "t"),
    _Type_map>>: lambda "mt" $ string "map from "
      ++ (ref typeDef @@ (project _MapType _MapType_keys @@ var "mt"))
      ++ string " to "
      ++ (ref typeDef @@ (project _MapType _MapType_values  @@ var "mt")),
    _Type_maybe>>: lambda "ot" $ string "maybe " ++ (ref typeDef @@ var "ot"),
    _Type_product>>: constant $ string "tuple",
    _Type_record>>: constant $ string "record",
    _Type_set>>: lambda "st" $ string "set of " ++ (ref typeDef @@ var "st"),
    _Type_sum>>: constant $ string "variant tuple",
    _Type_union>>: constant $ string "union",
    _Type_unit>>: constant $ string "unit",
    _Type_variable>>: constant $ string "instance of a named type",
    _Type_wrap>>: lambda "n" $ string "wrapper for "
      ++ (ref typeDef @@ (project _WrappedType _WrappedType_body @@ var "n"))]
