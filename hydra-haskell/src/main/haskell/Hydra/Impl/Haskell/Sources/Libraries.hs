module Hydra.Impl.Haskell.Sources.Libraries where

import Hydra.Basics
import Hydra.Core
import Hydra.Graph
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Prims
import Hydra.Impl.Haskell.Dsl.Terms
import qualified Hydra.Impl.Haskell.Dsl.Types as Types

import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Strings as Strings


_hydra_lib_io :: GraphName
_hydra_lib_io = GraphName "hydra/lib/io"

_io_showTerm :: Name
_io_showTerm = qname _hydra_lib_io "showTerm"

_io_showType :: Name
_io_showType = qname _hydra_lib_io "showType"

_hydra_lib_lists :: GraphName
_hydra_lib_lists = GraphName "hydra/lib/lists"

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

_hydra_lib_literals :: GraphName
_hydra_lib_literals = GraphName "hydra/lib/literals"

_literals_showInt32 :: Name
_literals_showInt32 = qname _hydra_lib_literals "showInt32"

_literals_showString :: Name
_literals_showString = qname _hydra_lib_literals "showString"

_hydra_lib_math :: GraphName
_hydra_lib_math = GraphName "hydra/lib/math"

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

_hydra_lib_sets :: GraphName
_hydra_lib_sets = GraphName "hydra/lib/sets"

_sets_add :: Name
_sets_add = qname _hydra_lib_sets "add"

_sets_contains :: Name
_sets_contains = qname _hydra_lib_sets "contains"

_sets_isEmpty :: Name
_sets_isEmpty = qname _hydra_lib_sets "isEmpty"

_sets_remove :: Name
_sets_remove = qname _hydra_lib_sets "remove"

_hydra_lib_strings :: GraphName
_hydra_lib_strings = GraphName "hydra/lib/strings"

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
--  prim1 _io_showTerm termInput stringOutput 
--  ]
  
hydraLibListsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibListsPrimitives = [
  prim1 _lists_concat (listInput (Types.list $ Types.variable "a") expectListPoly) (listOutputPoly "a") Lists.concat,
  prim1 _lists_head (listInputPoly "a") (outputPoly "a") Lists.head,
  prim2 _lists_intercalate (listInputPoly "a") (listInput (Types.variable "a") expectListPoly) (listOutputPoly "a") Lists.intercalate,
  prim2 _lists_intersperse (inputPoly "a") (listInputPoly "a") (listOutputPoly "a") Lists.intersperse,
  prim1 _lists_last (listInputPoly "a") (outputPoly "a") Lists.last,
  prim1 _lists_length (listInputPoly "a") int32Output Lists.length
--  ,
--  PrimitiveFunction _lists_map
--    (FunctionType
--      (functionType (Types.variable "a") (Types.variable "b"))
--      (functionType (Types.list $ Types.variable "a") (Types.list $ Types.variable "b")))
--    $ \args -> do
--      expectNArgs 2 args
--      a1 <- expectString $ L.head args
--      a2 <- expectListPoly $ args !! 1
--
  ]

hydraLibLiteralsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibLiteralsPrimitives = [
    prim1 _literals_showInt32 int32Input stringOutput Literals.showInt32,
    prim1 _literals_showString stringInput stringOutput Literals.showString]

hydraLibMathInt32Primitives :: Show m => [PrimitiveFunction m]
hydraLibMathInt32Primitives = [
    prim2 _math_add int32Input int32Input int32Output Math.add,
    prim2 _math_div int32Input int32Input int32Output Math.div,
    prim2 _math_mod int32Input int32Input int32Output Math.mod,
    prim2 _math_mul int32Input int32Input int32Output Math.mul,
    prim1 _math_neg int32Input int32Output Math.neg,
    prim2 _math_rem int32Input int32Input int32Output Math.rem,
    prim2 _math_sub int32Input int32Input int32Output Math.sub]

hydraLibStringsPrimitives :: Show m => [PrimitiveFunction m]
hydraLibStringsPrimitives = [
    prim1 _strings_cat (listInput Types.string expectString) stringOutput Strings.cat,
    prim1 _strings_length stringInput int32Output Strings.length,
    prim2 _strings_splitOn stringInput stringInput stringListOutput Strings.splitOn,
    prim1 _strings_toLower stringInput stringOutput Strings.toLower,
    prim1 _strings_toUpper stringInput stringOutput Strings.toUpper]

standardPrimitives :: Show m => [PrimitiveFunction m]
standardPrimitives =
         hydraLibListsPrimitives
      ++ hydraLibLiteralsPrimitives
      ++ hydraLibMathInt32Primitives
      ++ hydraLibStringsPrimitives
