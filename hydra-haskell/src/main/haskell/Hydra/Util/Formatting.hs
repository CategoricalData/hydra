module Hydra.Util.Formatting where

import qualified Data.Char as C


capitalize :: String -> String
capitalize s = case s of
  [] -> []
  (h:r) -> C.toUpper h : r

decapitalize :: String -> String
decapitalize s = case s of
  [] -> []
  (h:r) -> C.toLower h : r
