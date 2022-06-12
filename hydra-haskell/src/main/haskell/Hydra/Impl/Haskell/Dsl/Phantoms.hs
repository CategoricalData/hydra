module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core


newtype Case a = Case FieldName

data Element a = Element {
  elementName :: Name,
  elementData :: Data a} deriving (Eq, Ord, Read, Show)

data Ref a = Ref

newtype Data a = Data {unData :: Term Meta} deriving (Eq, Ord, Read, Show)
