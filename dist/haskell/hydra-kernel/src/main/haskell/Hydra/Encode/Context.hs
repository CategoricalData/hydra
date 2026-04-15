-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.context

module Hydra.Encode.Context where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

context :: Context.Context -> Core.Term
context x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (Context.contextTrace x))},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (Context.contextMessages x))},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap EncodeCore.name EncodeCore.term m)) (Context.contextOther x))}]})

inContext :: (t0 -> Core.Term) -> Context.InContext t0 -> Core.Term
inContext e x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.InContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (e (Context.inContextObject x))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (context (Context.inContextContext x))}]})
