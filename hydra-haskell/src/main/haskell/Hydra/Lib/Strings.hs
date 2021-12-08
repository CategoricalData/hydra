module Hydra.Lib.Strings where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


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

_hydra_lib_strings :: Name
_hydra_lib_strings = "hydra/lib/strings"

hydraLibStringsPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibStringsPrimitives = [
    prim2 _hydra_lib_strings "cat" stringInput stringInput stringOutput hsCat,
    prim1 _hydra_lib_strings "length" stringInput int32Output hsLength,
    prim2 _hydra_lib_strings "splitOn" stringInput stringInput stringListOutput hsSplitOn,
    prim1 _hydra_lib_strings "toLower" stringInput stringOutput hsToLower,
    prim1 _hydra_lib_strings "toUpper" stringInput stringOutput hsToUpper]
