module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core


newtype Case a = Case FieldName

data Definition a = Definition {
  definitionName :: Name,
  definitionDatum :: Datum a} deriving (Eq, Ord, Read, Show)

data Reference a = Reference

newtype Datum a = Datum {unDatum :: Term Meta} deriving (Eq, Ord, Read, Show)
