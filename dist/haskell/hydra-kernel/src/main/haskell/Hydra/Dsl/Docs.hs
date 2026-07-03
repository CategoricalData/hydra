-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.docs

module Hydra.Dsl.Docs where

import qualified Hydra.Core as Core
import qualified Hydra.Docs as Docs
import qualified Hydra.Dsl.Packaging as DslPackaging
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

-- | DSL injection for the ref variant of hydra.docs.DocSegment
docSegmentRef :: Typed.TypedTerm Packaging.EntityReference -> Typed.TypedTerm Docs.DocSegment
docSegmentRef x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.docs.DocSegment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ref"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))

-- | DSL injection for the text variant of hydra.docs.DocSegment
docSegmentText :: Typed.TypedTerm String -> Typed.TypedTerm Docs.DocSegment
docSegmentText x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.docs.DocSegment"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "text"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
