-- Note: this is an automatically generated file. Do not edit.

-- | Type classes

module Hydra.Classes where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | Any of a small number of built-in type classes
data TypeClass =
  TypeClassEquality  |
  TypeClassOrdering
  deriving (Eq, Ord, Read, Show)

_TypeClass = Core.Name "hydra.classes.TypeClass"

_TypeClass_equality = Core.Name "equality"

_TypeClass_ordering = Core.Name "ordering"
