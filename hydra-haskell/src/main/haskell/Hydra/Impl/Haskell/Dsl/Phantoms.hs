module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core


newtype Case a = Case FieldName

data El a = El {
  elName :: Name,
  elTerm :: Trm a} deriving Show

data Ref a = Ref

newtype Trm a = Trm {unTrm :: Term Meta} deriving (Eq, Ord, Read, Show)

newtype Var a = Var String
