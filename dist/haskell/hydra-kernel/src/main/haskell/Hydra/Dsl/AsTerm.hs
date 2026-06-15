{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Type class for implicit coercion to TypedTerm
-- This allows TypedTerm, TypedBinding, and String to be used where a TypedTerm is expected,
-- while preserving phantom type information.

module Hydra.Dsl.AsTerm where

import Hydra.Core
import Hydra.Packaging
import Hydra.Typed


-- | Type class for implicit coercion to TypedTerm, preserving phantom types
-- TypedTerm passes through unchanged, TypedBinding becomes a variable reference
-- The functional dependency ensures that the input type determines the phantom type
class AsTerm f a | f -> a where
  asTerm :: f -> TypedTerm a

instance AsTerm (TypedTerm a) a where
  asTerm = id

instance AsTerm (TypedBinding a) a where
  asTerm (TypedBinding name _) = TypedTerm (TermVariable name)

instance AsTerm (TypedTermDefinition a) a where
  asTerm (TypedTermDefinition name _) = TypedTerm (TermVariable name)

instance AsTerm String String where
  asTerm s = TypedTerm $ TermLiteral $ LiteralString s

instance AsTerm Int Int where
  asTerm i = TypedTerm $ TermLiteral $ LiteralInteger $ IntegerValueInt32 i


-- | Type class for types from which a kernel-level 'Name' can be statically extracted.
-- Used by Phantoms primitives like 'inject', 'match', 'project', 'record', 'unwrap', 'wrap'
-- to accept either a bare 'Name' or any name-typed binding/definition where the 'Name'
-- is known at construction time. Distinct from 'AsTerm' (which can lift arbitrary
-- @TypedTerm Name@ values, but those don't yield a static 'Name').
class AsName n where
  asName :: n -> Name

instance AsName Name where
  asName = id

instance AsName (TypedBinding Name) where
  asName (TypedBinding name _) = name

instance AsName (TypedTermDefinition Name) where
  asName (TypedTermDefinition name _) = name
