module Hydra.Impl.Haskell.Dsl.Phantoms where

import Hydra.Core
import Hydra.Graph


data Any = Any

newtype Trm a = Trm {unTrm :: Term Meta} deriving (Eq, Ord, Read, Show)

data El a = El {
  elGraph :: GraphName,
  elName :: String,
  elTerm :: Trm a} deriving Show

newtype Fld = Fld {unFld :: Field Meta} deriving (Eq, Ord, Read, Show)

data Record = Record

data Ref a = Ref

data Union = Union

newtype Var a = Var String
