-- Note: this is an automatically generated file. Do not edit.

-- | Phantom types for use with Hydra DSLs

module Hydra.Phantoms where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An association of a named term (element) with a phantom type
data TBinding a = 
  TBinding {
    -- | The name of the term
    tBindingName :: Core.Name,
    -- | The term with its phantom type
    tBindingTerm :: (TTerm a)}
  deriving (Eq, Ord, Read, Show)

_TBinding = (Core.Name "hydra.phantoms.TBinding")

_TBinding_name = (Core.Name "name")

_TBinding_term = (Core.Name "term")

-- | An association of a term with a phantom type
newtype TTerm a = 
  TTerm {
    unTTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TTerm = (Core.Name "hydra.phantoms.TTerm")
