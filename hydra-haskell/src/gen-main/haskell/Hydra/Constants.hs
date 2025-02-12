-- | A module for tier-0 constants.

module Hydra.Constants where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

ignoredVariable :: String
ignoredVariable = "_"

key_classes :: Core.Name
key_classes = (Core.Name "classes")

key_deprecated :: Core.Name
key_deprecated = (Core.Name "_deprecated")

key_description :: Core.Name
key_description = (Core.Name "description")

key_exclude :: Core.Name
key_exclude = (Core.Name "exclude")

key_maxLength :: Core.Name
key_maxLength = (Core.Name "_maxLength")

key_minLength :: Core.Name
key_minLength = (Core.Name "_minLength")

key_preserveFieldName :: Core.Name
key_preserveFieldName = (Core.Name "_preserveFieldName")

key_type :: Core.Name
key_type = (Core.Name "type")

-- | A placeholder name for row types as they are being constructed
placeholderName :: Core.Name
placeholderName = (Core.Name "Placeholder")

maxTraceDepth :: Int
maxTraceDepth = 50