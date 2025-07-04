-- | Phantom types for use with Hydra DSLs

module Hydra.Phantoms where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | An association with a named term (element) with a phantom type
data TElement a = 
  TElement {
    tElementName :: Core.Name,
    tElementTerm :: (TTerm a)}
  deriving (Eq, Ord, Read, Show)

_TElement = (Core.Name "hydra.phantoms.TElement")

_TElement_name = (Core.Name "name")

_TElement_term = (Core.Name "term")

-- | An association of a term with a phantom type
newtype TTerm a = 
  TTerm {
    unTTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TTerm = (Core.Name "hydra.phantoms.TTerm")
