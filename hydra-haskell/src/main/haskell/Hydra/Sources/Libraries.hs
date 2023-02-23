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
--  unaryPrimitive _io_showTerm (variable "a) string
--  ]

hydraLibFlowsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibFlowsPrimitives = [
  binaryPrimitive _flows_apply (flow (variable "s") (function (variable "a") (variable "b"))) (flow (variable "s") (variable "a")) (flow (variable "s") (variable "b")) Flows.apply,
  binaryPrimitive _flows_bind (flow (variable "s") (variable "a")) (function (variable "a") (flow (variable "s") (variable "b"))) (flow (variable "s") (variable "b")) Flows.bind,
  binaryPrimitive _flows_map (function (variable "a") (variable "b")) (flow (variable "s") (variable "a")) (flow (variable "s") (variable "b")) Flows.map,
  unaryPrimitive _flows_pure (variable "a") (flow (variable "s") (variable "a")) Flows.pure]

hydraLibListsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibListsPrimitives = [
  binaryPrimitive _lists_apply (list $ function (variable "a") (variable "b")) (list $ variable "a") (list $ variable "b") Lists.apply,
  binaryPrimitive _lists_bind (list $ variable "a") (function (variable "a") (list $ variable "b")) (list $ variable "b") Lists.bind,
  unaryPrimitive _lists_concat (list $ list $ variable "a") (list $ variable "a") Lists.concat,
  unaryPrimitive _lists_head (list $ variable "a") (variable "a") Lists.head,
  binaryPrimitive _lists_intercalate (list $ variable "a") (list $ list $ variable "a") (list $ variable "a") Lists.intercalate,
  binaryPrimitive _lists_intersperse (variable "a") (list $ variable "a") (list $ variable "a") Lists.intersperse,
  unaryPrimitive _lists_last (list $ variable "a") (variable "a") Lists.last,
  unaryPrimitive _lists_length (list $ variable "a") int32 Lists.length,
  binaryPrimitive _lists_map (function (variable "a") (variable "b")) (list $ variable "a") (list $ variable "b") Lists.map,
  unaryPrimitive _lists_pure (variable "a") (list $ variable "a") Lists.pure]

hydraLibLiteralsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibLiteralsPrimitives = [
  unaryPrimitive _literals_showInt32 int32 string Literals.showInt32,
  unaryPrimitive _literals_showString string string Literals.showString]

hydraLibMapsPrimitives :: (Ord m, Show m) => [PrimitiveFunction m]
hydraLibMapsPrimitives = [
  binaryPrimitive _optionals_map
    (function (variable "v1") (variable "v2"))
    (Prims.map (variable "k") (variable "v1"))
    (Prims.map (variable "k") (variable "v2"))
    Maps.map,
  unaryPrimitive _sets_size (set $ variable "a") int32 Sets.size]

hydraLibMathInt32Primitives :: Show m => [PrimitiveFunction m]
hydraLibMathInt32Primitives = [
  binaryPrimitive _math_add int32 int32 int32 Math.add,
  binaryPrimitive _math_div int32 int32 int32 Math.div,
  binaryPrimitive _math_mod int32 int32 int32 Math.mod,
  binaryPrimitive _math_mul int32 int32 int32 Math.mul,
  unaryPrimitive _math_neg int32 int32 Math.neg,
  binaryPrimitive _math_rem int32 int32 int32 Math.rem,
  binaryPrimitive _math_sub int32 int32 int32 Math.sub]

hydraLibOptionalsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibOptionalsPrimitives = [
  binaryPrimitive _optionals_apply (optional $ function (variable "a") (variable "b")) (optional $ variable "a") (optional $ variable "b") Optionals.apply,
  binaryPrimitive _optionals_bind (optional $ variable "a") (function (variable "a") (optional $ variable "b")) (optional $ variable "b") Optionals.bind,
  binaryPrimitive _optionals_map (function (variable "a") (variable "b")) (optional $ variable "a") (optional $ variable "b") Optionals.map,
  unaryPrimitive _optionals_pure (variable "a") (optional $ variable "a") Optionals.pure]

hydraLibSetsPrimitives :: (Ord m, Show m) => [PrimitiveFunction m]
hydraLibSetsPrimitives = [
  binaryPrimitive _sets_contains (variable "a") (set $ variable "a") boolean Sets.contains,
  nullaryPrimitive _sets_empty (set $ variable "a") Sets.empty,
  unaryPrimitive _sets_fromList (list $ variable "a") (set $ variable "a") Sets.fromList,
  binaryPrimitive _sets_insert (variable "a") (set $ variable "a") (set $ variable "a") Sets.insert,
  unaryPrimitive _sets_isEmpty (set $ variable "a") boolean Sets.isEmpty,
  binaryPrimitive _sets_map (function (variable "a") (variable "b")) (set $ variable "a") (set $ variable "b") Sets.map,
  binaryPrimitive _sets_remove (variable "a") (set $ variable "a") (set $ variable "a") Sets.remove,
  unaryPrimitive _sets_singleton (variable "a") (set $ variable "a") Sets.singleton,
  unaryPrimitive _sets_size (set $ variable "a") int32 Sets.size,
  unaryPrimitive _sets_toList (set $ variable "a") (list $ variable "a") Sets.toList]

hydraLibStringsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibStringsPrimitives = [
  unaryPrimitive _strings_cat (list string) string Strings.cat,
  unaryPrimitive _strings_length string int32 Strings.length,
  binaryPrimitive _strings_splitOn string string (list string) Strings.splitOn,
  unaryPrimitive _strings_toLower string string Strings.toLower,
  unaryPrimitive _strings_toUpper string string Strings.toUpper]

standardPrimitives :: (Ord m, Show m) => [PrimitiveFunction m]
standardPrimitives =
     hydraLibFlowsPrimitives
  ++ hydraLibListsPrimitives
  ++ hydraLibLiteralsPrimitives
  ++ hydraLibMapsPrimitives
  ++ hydraLibMathInt32Primitives
  ++ hydraLibOptionalsPrimitives
  ++ hydraLibSetsPrimitives
  ++ hydraLibStringsPrimitives
