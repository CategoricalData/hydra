{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Type class for implicit coercion to TTerm
-- This allows TTerm, TBinding, and String to be used where a TTerm is expected,
-- while preserving phantom type information.

module Hydra.Dsl.AsTerm where

import Hydra.Core
import Hydra.Packaging
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

instance AsTerm (TTermDefinition a) a where
  asTerm (TTermDefinition name _) = TTerm (TermVariable name)

instance AsTerm String String where
  asTerm s = TTerm $ TermLiteral $ LiteralString s

instance AsTerm Int Int where
  asTerm i = TTerm $ TermLiteral $ LiteralInteger $ IntegerValueInt32 i


-- | Type class for types from which a kernel-level 'Name' can be statically extracted.
-- Used by Phantoms primitives like 'inject', 'match', 'project', 'record', 'unwrap', 'wrap'
-- to accept either a bare 'Name' or any name-typed binding/definition where the 'Name'
-- is known at construction time. Distinct from 'AsTerm' (which can lift arbitrary
-- @TTerm Name@ values, but those don't yield a static 'Name').
class AsName n where
  asName :: n -> Name

instance AsName Name where
  asName = id

instance AsName (TBinding Name) where
  asName (TBinding name _) = name

instance AsName (TTermDefinition Name) where
  asName (TTermDefinition name _) = name
