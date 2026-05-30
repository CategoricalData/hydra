-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.typed

module Hydra.Encode.Typed where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.typed.TypedBinding
typedBinding :: t0 -> Typed.TypedBinding t1 -> Core.Term
typedBinding a x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typed.TypedBinding"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Typed.typedBindingName x))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (typedTerm a (Typed.typedBindingTerm x))}]})
-- | Encoder for hydra.typed.TypedTerm
typedTerm :: t0 -> Typed.TypedTerm t1 -> Core.Term
typedTerm a x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.typed.TypedTerm"),
      Core.wrappedTermBody = (EncodeCore.term (Typed.unTypedTerm x))})
-- | Encoder for hydra.typed.TypedTermDefinition
typedTermDefinition :: t0 -> Typed.TypedTermDefinition t1 -> Core.Term
typedTermDefinition a x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.typed.TypedTermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Typed.typedTermDefinitionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (typedTerm a (Typed.typedTermDefinitionTerm x))}]})
