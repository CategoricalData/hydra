-- Note: this is an automatically generated file. Do not edit.

-- | String representations of hydra.util types

module Hydra.Show.Util where

import qualified Hydra.Util as Util
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Show a case convention as a string
caseConvention :: (Util.CaseConvention -> String)
caseConvention c = ((\x -> case x of
  Util.CaseConventionLowerSnake -> "lower_snake_case"
  Util.CaseConventionUpperSnake -> "UPPER_SNAKE_CASE"
  Util.CaseConventionCamel -> "camelCase"
  Util.CaseConventionPascal -> "PascalCase") c)
