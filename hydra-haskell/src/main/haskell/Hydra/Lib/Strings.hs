module Hydra.Lib.Strings where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims
import Hydra.Impl.Haskell.Dsl.Terms

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


_hydra_lib_strings :: Name
_hydra_lib_strings = "hydra/lib/strings"

_cat :: Name
_cat = qname _hydra_lib_strings "cat"

_length :: Name
_length = qname _hydra_lib_strings "length"

_splitOn :: Name
_splitOn = qname _hydra_lib_strings "splitOn"

_toLower :: Name
_toLower = qname _hydra_lib_strings "toLower"

_toUpper :: Name
_toUpper = qname _hydra_lib_strings "toUpper"

hsCat :: String -> String -> String
hsCat x y = x ++ y

hsLength :: String -> Int
hsLength = L.length

hsSplitOn :: String -> String -> [String]
hsSplitOn = LS.splitOn

hsToLower :: String -> String
hsToLower = fmap C.toLower

hsToUpper :: String -> String
hsToUpper = fmap C.toUpper

hydraLibStringsPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibStringsPrimitives = [
    prim2 _cat stringInput stringInput stringOutput hsCat,
    prim1 _length stringInput int32Output hsLength,
    prim2 _splitOn stringInput stringInput stringListOutput hsSplitOn,
    prim1 _toLower stringInput stringOutput hsToLower,
    prim1 _toUpper stringInput stringOutput hsToUpper]
