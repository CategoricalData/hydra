-- | String formatting types and functions.

module Hydra.Formatting where

import qualified Hydra.Core as Core
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Strings as Strings
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data CaseConvention = 
  CaseConventionCamel  |
  CaseConventionPascal  |
  CaseConventionLowerSnake  |
  CaseConventionUpperSnake 
  deriving (Eq, Ord, Read, Show)

_CaseConvention = (Core.Name "hydra/formatting.CaseConvention")

_CaseConvention_Camel = (Core.Name "Camel")

_CaseConvention_Pascal = (Core.Name "Pascal")

_CaseConvention_LowerSnake = (Core.Name "LowerSnake")

_CaseConvention_UpperSnake = (Core.Name "UpperSnake")

-- | Capitalize the first letter of a string
capitalize :: (String -> String)
capitalize = (mapFirstLetter Strings.toUpper)

-- | Decapitalize the first letter of a string
decapitalize :: (String -> String)
decapitalize = (mapFirstLetter Strings.toLower)

-- | A helper which maps the first letter of a string to another string
mapFirstLetter :: ((String -> String) -> String -> String)
mapFirstLetter mapping s =  
  let firstLetter = (mapping (Strings.fromList (Lists.pure (Lists.head list)))) 
      list = (Strings.toList s)
  in (Logic.ifElse s (Strings.cat2 firstLetter (Strings.fromList (Lists.tail list))) (Strings.isEmpty s))