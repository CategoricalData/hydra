-- Note: this is an automatically generated file. Do not edit.

-- | Phantom types for use with Hydra DSLs

module Hydra.Phantoms where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)

-- | An association of a named term (element) with a phantom type
data TBinding a =
  TBinding {
    -- | The name of the term
    tBindingName :: Core.Name,
    -- | The term with its phantom type
    tBindingTerm :: (TTerm a)}
  deriving (Eq, Ord, Read, Show)

_TBinding = Core.Name "hydra.phantoms.TBinding"

_TBinding_name = Core.Name "name"

_TBinding_term = Core.Name "term"

-- | An association of a term with a phantom type
newtype TTerm a =
  TTerm {
    unTTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TTerm = Core.Name "hydra.phantoms.TTerm"

-- | An association of a term definition with a phantom type
data TTermDefinition a =
  TTermDefinition {
    -- | The name of the term
    tTermDefinitionName :: Core.Name,
    -- | The term with its phantom type
    tTermDefinitionTerm :: (TTerm a)}
  deriving (Eq, Ord, Read, Show)

_TTermDefinition = Core.Name "hydra.phantoms.TTermDefinition"

_TTermDefinition_name = Core.Name "name"

_TTermDefinition_term = Core.Name "term"
