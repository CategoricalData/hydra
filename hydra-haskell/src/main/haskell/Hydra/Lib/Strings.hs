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

hsCat :: [String] -> String
hsCat = L.concat

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
    prim1 _strings_cat (listInput stringType expectString) stringOutput hsCat,
    prim1 _strings_length stringInput int32Output hsLength,
    prim2 _strings_splitOn stringInput stringInput stringListOutput hsSplitOn,
    prim1 _strings_toLower stringInput stringOutput hsToLower,
    prim1 _strings_toUpper stringInput stringOutput hsToUpper]
