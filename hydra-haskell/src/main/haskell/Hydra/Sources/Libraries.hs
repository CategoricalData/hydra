{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Libraries where

import Hydra.Kernel
import Hydra.Dsl.Prims as Prims
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types

import qualified Hydra.Lib.Flows as Flows
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Optionals as Optionals
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings


_hydra_lib_flows :: Namespace
_hydra_lib_flows = Namespace "hydra/lib/flows"

_flows_apply :: Name
_flows_apply = qname _hydra_lib_flows "apply"

_flows_bind :: Name
_flows_bind = qname _hydra_lib_flows "bind"

_flows_map :: Name
_flows_map = qname _hydra_lib_flows "map"

_flows_pure :: Name
_flows_pure = qname _hydra_lib_flows "pure"

_hydra_lib_io :: Namespace
_hydra_lib_io = Namespace "hydra/lib/io"

_io_showTerm :: Name
_io_showTerm = qname _hydra_lib_io "showTerm"

_io_showType :: Name
_io_showType = qname _hydra_lib_io "showType"

_hydra_lib_lists :: Namespace
_hydra_lib_lists = Namespace "hydra/lib/lists"

_lists_apply :: Name
_lists_apply = qname _hydra_lib_lists "apply"

_lists_bind :: Name
_lists_bind = qname _hydra_lib_lists "bind"

_lists_concat :: Name
_lists_concat = qname _hydra_lib_lists "concat"

_lists_head :: Name
_lists_head = qname _hydra_lib_lists "head"

_lists_intercalate :: Name
_lists_intercalate = qname _hydra_lib_lists "intercalate"

_lists_intersperse :: Name
_lists_intersperse = qname _hydra_lib_lists "intersperse"

_lists_last :: Name
_lists_last = qname _hydra_lib_lists "last"

_lists_length :: Name
_lists_length = qname _hydra_lib_lists "length"

_lists_map :: Name
_lists_map = qname _hydra_lib_lists "map"

_lists_pure :: Name
_lists_pure = qname _hydra_lib_lists "pure"

_hydra_lib_literals :: Namespace
_hydra_lib_literals = Namespace "hydra/lib/literals"

_literals_showInt32 :: Name
_literals_showInt32 = qname _hydra_lib_literals "showInt32"

_literals_showString :: Name
_literals_showString = qname _hydra_lib_literals "showString"

_hydra_lib_maps :: Namespace
_hydra_lib_maps = Namespace "hydra/lib/maps"

_maps_map :: Name
_maps_map = qname _hydra_lib_maps "map"

_maps_size :: Name
_maps_size = qname _hydra_lib_maps "size"

_hydra_lib_math :: Namespace
_hydra_lib_math = Namespace "hydra/lib/math"

_math_add :: Name
_math_add = qname _hydra_lib_math "add"

_math_div :: Name
_math_div = qname _hydra_lib_math "div"

_math_mod :: Name
_math_mod = qname _hydra_lib_math "mod"

_math_mul :: Name
_math_mul = qname _hydra_lib_math "mul"

_math_neg :: Name
_math_neg = qname _hydra_lib_math "neg"

_math_rem :: Name
_math_rem = qname _hydra_lib_math "rem"

_math_sub :: Name
_math_sub = qname _hydra_lib_math "sub"

_hydra_lib_optionals :: Namespace
_hydra_lib_optionals = Namespace "hydra/lib/optionals"

_optionals_apply :: Name
_optionals_apply = qname _hydra_lib_optionals "apply"

_optionals_bind :: Name
_optionals_bind = qname _hydra_lib_optionals "bind"

_optionals_map :: Name
_optionals_map = qname _hydra_lib_optionals "map"

_optionals_pure :: Name
_optionals_pure = qname _hydra_lib_optionals "pure"

_hydra_lib_sets :: Namespace
_hydra_lib_sets = Namespace "hydra/lib/sets"

_sets_insert :: Name
_sets_insert = qname _hydra_lib_sets "add"

_sets_contains :: Name
_sets_contains = qname _hydra_lib_sets "contains"

_sets_empty :: Name
_sets_empty = qname _hydra_lib_sets "empty"

_sets_fromList :: Name
_sets_fromList = qname _hydra_lib_sets "fromList"

_sets_isEmpty :: Name
_sets_isEmpty = qname _hydra_lib_sets "isEmpty"

_sets_map :: Name
_sets_map = qname _hydra_lib_sets "map"

_sets_remove :: Name
_sets_remove = qname _hydra_lib_sets "remove"

_sets_singleton :: Name
_sets_singleton = qname _hydra_lib_sets "pure"

_sets_size :: Name
_sets_size = qname _hydra_lib_sets "size"

_sets_toList :: Name
_sets_toList = qname _hydra_lib_sets "toList"

_hydra_lib_strings :: Namespace
_hydra_lib_strings = Namespace "hydra/lib/strings"

_strings_cat :: Name
_strings_cat = qname _hydra_lib_strings "cat"

_strings_length :: Name
_strings_length = qname _hydra_lib_strings "length"

_strings_splitOn :: Name
_strings_splitOn = qname _hydra_lib_strings "splitOn"

_strings_toLower :: Name
_strings_toLower = qname _hydra_lib_strings "toLower"

_strings_toUpper :: Name
_strings_toUpper = qname _hydra_lib_strings "toUpper"

--hydraIoPrimitives = [
--  prim1 _io_showTerm (variable "a) string
--  ]

hydraLibFlowsPrimitives :: (Ord a, Show a) => [Primitive a]
hydraLibFlowsPrimitives = [
  prim2 _flows_apply (flow (variable "s") (function "x" "y")) (flow (variable "s") (variable "x")) (flow (variable "s") (variable "y")) Flows.apply,
  prim2 _flows_bind (flow (variable "s") (variable "x")) (function "x" (flow "s" "y")) (flow (variable "s") (variable "y")) Flows.bind,
  prim2 _flows_map (function "x" "y") (flow (variable "s") (variable "x")) (flow (variable "s") (variable "y")) Flows.map,
  prim1 _flows_pure (variable "x") (flow (variable "s") (variable "x")) Flows.pure]

hydraLibListsPrimitives :: (Ord a, Show a) => [Primitive a]
hydraLibListsPrimitives = [
  prim2 _lists_apply (list $ function "x" "y") (list $ variable "x") (list $ variable "y") Lists.apply,
  prim2 _lists_bind (list $ variable "x") (function "x" (list "y")) (list $ variable "y") Lists.bind,
  prim1 _lists_concat (list $ list $ variable "x") (list $ variable "x") Lists.concat,
  prim1 _lists_head (list $ variable "x") (variable "x") Lists.head,
  prim2 _lists_intercalate (list $ variable "x") (list $ list $ variable "x") (list $ variable "x") Lists.intercalate,
  prim2 _lists_intersperse (variable "x") (list $ variable "x") (list $ variable "x") Lists.intersperse,
  prim1 _lists_last (list $ variable "x") (variable "x") Lists.last,
  prim1 _lists_length (list $ variable "x") int32 Lists.length,
  prim2 _lists_map (function "x" "y") (list $ variable "x") (list $ variable "y") Lists.map,
  prim1 _lists_pure "x" (list $ variable "x") Lists.pure]

hydraLibLiteralsPrimitives :: Show a => [Primitive a]
hydraLibLiteralsPrimitives = [
  prim1 _literals_showInt32 int32 string Literals.showInt32,
  prim1 _literals_showString string string Literals.showString]

hydraLibMapsPrimitives :: (Ord a, Show a) => [Primitive a]
hydraLibMapsPrimitives = [
  prim2 _optionals_map
    (function "v1" "v2")
    (Prims.map (variable "k") (variable "v1"))
    (Prims.map (variable "k") (variable "v2"))
    Maps.map,
  prim1 _sets_size (set $ variable "x") int32 Sets.size]

hydraLibMathInt32Primitives :: Show a => [Primitive a]
hydraLibMathInt32Primitives = [
  prim2 _math_add int32 int32 int32 Math.add,
  prim2 _math_div int32 int32 int32 Math.div,
  prim2 _math_mod int32 int32 int32 Math.mod,
  prim2 _math_mul int32 int32 int32 Math.mul,
  prim1 _math_neg int32 int32 Math.neg,
  prim2 _math_rem int32 int32 int32 Math.rem,
  prim2 _math_sub int32 int32 int32 Math.sub]

hydraLibOptionalsPrimitives :: (Ord a, Show a) => [Primitive a]
hydraLibOptionalsPrimitives = [
  prim2 _optionals_apply (optional $ function "x" "y") (optional $ variable "x") (optional $ variable "y") Optionals.apply,
  prim2 _optionals_bind (optional $ variable "x") (function "x" (optional "y")) (optional $ variable "y") Optionals.bind,
  prim2 _optionals_map (function "x" "y") (optional $ variable "x") (optional $ variable "y") Optionals.map,
  prim1 _optionals_pure (variable "x") (optional $ variable "x") Optionals.pure]

hydraLibSetsPrimitives :: (Ord a, Show a) => [Primitive a]
hydraLibSetsPrimitives = [
  prim2 _sets_contains (variable "x") (set $ variable "x") boolean Sets.contains,
  prim0 _sets_empty (set $ variable "x") Sets.empty,
  prim1 _sets_fromList (list $ variable "x") (set $ variable "x") Sets.fromList,
  prim2 _sets_insert (variable "x") (set $ variable "x") (set $ variable "x") Sets.insert,
  prim1 _sets_isEmpty (set $ variable "x") boolean Sets.isEmpty,
  prim2 _sets_map (function "x" "y") (set $ variable "x") (set $ variable "y") Sets.map,
  prim2 _sets_remove (variable "x") (set $ variable "x") (set $ variable "x") Sets.remove,
  prim1 _sets_singleton (variable "x") (set $ variable "x") Sets.singleton,
  prim1 _sets_size (set $ variable "x") int32 Sets.size,
  prim1 _sets_toList (set $ variable "x") (list $ variable "x") Sets.toList]

hydraLibStringsPrimitives :: Show a => [Primitive a]
hydraLibStringsPrimitives = [
  prim1 _strings_cat (list string) string Strings.cat,
  prim1 _strings_length string int32 Strings.length,
  prim2 _strings_splitOn string string (list string) Strings.splitOn,
  prim1 _strings_toLower string string Strings.toLower,
  prim1 _strings_toUpper string string Strings.toUpper]

standardPrimitives :: (Ord a, Show a) => [Primitive a]
standardPrimitives =
     hydraLibFlowsPrimitives
  ++ hydraLibListsPrimitives
  ++ hydraLibLiteralsPrimitives
  ++ hydraLibMapsPrimitives
  ++ hydraLibMathInt32Primitives
  ++ hydraLibOptionalsPrimitives
  ++ hydraLibSetsPrimitives
  ++ hydraLibStringsPrimitives
