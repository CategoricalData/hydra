module Hydra.Lib.Strings where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.Prims

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


hsCat :: String -> String -> Result String
hsCat x y = pure $ x ++ y

hsLength :: String -> Result Int
hsLength = pure . L.length

hsSplitOn :: String -> String -> Result [String]
hsSplitOn x y = pure $ LS.splitOn x y

hsToLower :: String -> Result String
hsToLower = pure . fmap C.toLower

hsToUpper :: String -> Result String
hsToUpper = pure . fmap C.toUpper

_hydra_lib_strings :: Name
_hydra_lib_strings = "hydra/lib/strings"

hydraLibStringsPrimitives :: (Default a, Show a) => [PrimitiveFunction a]
hydraLibStringsPrimitives = [
    prim2 _hydra_lib_strings "cat" stringInput stringInput stringOutput hsCat,
    prim1 _hydra_lib_strings "length" stringInput int32Output hsLength,
    prim2 _hydra_lib_strings "splitOn" stringInput stringInput stringListOutput hsSplitOn,
    prim1 _hydra_lib_strings "toLower" stringInput stringOutput hsToLower,
    prim1 _hydra_lib_strings "toUpper" stringInput stringOutput hsToUpper]
