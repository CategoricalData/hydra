{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Type class for implicit coercion to TTerm
-- This allows TTerm, TBinding, and String to be used where a TTerm is expected,
-- while preserving phantom type information.

module Hydra.Dsl.AsTerm where

import Hydra.Core
import Hydra.Module
import Hydra.Phantoms


-- | Type class for implicit coercion to TTerm, preserving phantom types
-- TTerm passes through unchanged, TBinding becomes a variable reference
-- The functional dependency ensures that the input type determines the phantom type
class AsTerm f a | f -> a where
  asTerm :: f -> TTerm a

instance AsTerm (TTerm a) a where
  asTerm = id

instance AsTerm (TBinding a) a where
  asTerm (TBinding name _) = TTerm (TermVariable name)

instance AsTerm String String where
  asTerm s = TTerm $ TermLiteral $ LiteralString s

instance AsTerm Int Int where
  asTerm i = TTerm $ TermLiteral $ LiteralInteger $ IntegerValueInt32 i
