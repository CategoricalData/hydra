{-# LANGUAGE FlexibleInstances #-}

module Hydra.Dsl.Common where

import Hydra.Core

import Data.String(IsString(..))


instance IsString Type where fromString = TypeVariable . Name
instance IsString Term where fromString = TermLiteral . LiteralString
