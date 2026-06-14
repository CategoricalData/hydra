{-# LANGUAGE FlexibleInstances #-}

-- | Type class for implicit coercion to Type
-- This allows Type, TypeDefinition, and String to be used where a Type is expected

module Hydra.Dsl.AsType where

import Hydra.Core
import Hydra.Packaging


-- | Type class for implicit coercion to Type
-- This allows Type, TypeDefinition, and String to be used where a Type is expected
class AsType a where
  asType :: a -> Type

instance AsType Type where
  asType = id

-- | A TypeDefinition is referenced as a type variable bearing its name.
instance AsType TypeDefinition where
  asType = TypeVariable . typeDefinitionName

instance AsType String where
  asType s = TypeVariable (Name s)
