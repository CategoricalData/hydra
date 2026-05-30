-- Note: this is an automatically generated file. Do not edit.
-- | Typed (phantom) wrappers for use with Hydra DSLs

module Hydra.Typed where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | An association of a named term (element) with a phantom type
data TypedBinding a =
  TypedBinding {
    -- | The name of the term
    typedBindingName :: Core.Name,
    -- | The term with its phantom type
    typedBindingTerm :: (TypedTerm a)}
  deriving (Eq, Ord, Read, Show)
_TypedBinding = Core.Name "hydra.typed.TypedBinding"
_TypedBinding_name = Core.Name "name"
_TypedBinding_term = Core.Name "term"
-- | An association of a term with a phantom type
newtype TypedTerm a =
  TypedTerm {
    unTypedTerm :: Core.Term}
  deriving (Eq, Ord, Read, Show)
_TypedTerm = Core.Name "hydra.typed.TypedTerm"
-- | An association of a term definition with a phantom type
data TypedTermDefinition a =
  TypedTermDefinition {
    -- | The name of the term
    typedTermDefinitionName :: Core.Name,
    -- | The term with its phantom type
    typedTermDefinitionTerm :: (TypedTerm a)}
  deriving (Eq, Ord, Read, Show)
_TypedTermDefinition = Core.Name "hydra.typed.TypedTermDefinition"
_TypedTermDefinition_name = Core.Name "name"
_TypedTermDefinition_term = Core.Name "term"
