{-# LANGUAGE FlexibleInstances #-}

module Hydra.Dsl.Common where

import Hydra.Core
import Hydra.Phantoms

import Data.String(IsString(..))


instance IsString Type where fromString = TypeVariable . Name
instance IsString Term where fromString = TermLiteral . LiteralString
instance IsString (TTerm a) where fromString = TTerm . TermLiteral . LiteralString
