module Hydra.Errors where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

-- An optional value qualified with a (possibly empty) list of warnings
data Qualified m 
  = Qualified {
    qualifiedValue :: (Maybe m),
    qualifiedWarnings :: [String]}
  deriving (Eq, Ord, Read, Show)

_Qualified = "hydra/errors.Qualified"

_Qualified_value = "value"

_Qualified_warnings = "warnings"