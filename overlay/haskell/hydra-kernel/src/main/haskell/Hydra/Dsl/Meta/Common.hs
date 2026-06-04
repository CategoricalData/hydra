{-# LANGUAGE FlexibleInstances #-}

-- | Common IsString instances for Term and Type, enabling OverloadedStrings

module Hydra.Dsl.Meta.Common where

import Hydra.Core
import Hydra.Typed

import Data.String(IsString(..))


instance IsString Type where fromString = TypeVariable . Name
instance IsString Term where fromString = TermLiteral . LiteralString
instance IsString (TypedTerm a) where fromString = TypedTerm . TermLiteral . LiteralString
