{-# LANGUAGE FlexibleInstances #-}

-- | Type class for implicit coercion to Type
-- This allows Type, Binding, and String to be used where a Type is expected

module Hydra.Dsl.AsType where

import Hydra.Core
import Hydra.Module


-- | Type class for implicit coercion to Type
-- This allows Type, Binding, and String to be used where a Type is expected
class AsType a where
  asType :: a -> Type

instance AsType Type where
  asType = id

instance AsType Binding where
  asType b = TypeVariable (bindingName b)

instance AsType String where
  asType s = TypeVariable (Name s)
