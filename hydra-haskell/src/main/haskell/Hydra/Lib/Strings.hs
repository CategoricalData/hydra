module Hydra.Lib.Strings where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.List.Split as LS


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
