-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.context

module Hydra.Dsl.Context where
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
-- | DSL constructor for hydra.context.Context
context :: Phantoms.TTerm [String] -> Phantoms.TTerm [String] -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Context.Context
context trace messages other =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Phantoms.unTTerm trace)},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Phantoms.unTTerm messages)},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Phantoms.unTTerm other)}]}))
-- | DSL accessor for the messages field of hydra.context.Context
contextMessages :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String]
contextMessages x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "messages")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the other field of hydra.context.Context
contextOther :: Phantoms.TTerm Context.Context -> Phantoms.TTerm (M.Map Core.Name Core.Term)
contextOther x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "other")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the trace field of hydra.context.Context
contextTrace :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String]
contextTrace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "trace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the messages field of hydra.context.Context
contextWithMessages :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String] -> Phantoms.TTerm Context.Context
contextWithMessages original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "trace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "other")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the other field of hydra.context.Context
contextWithOther :: Phantoms.TTerm Context.Context -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Context.Context
contextWithOther original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "trace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "messages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the trace field of hydra.context.Context
contextWithTrace :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String] -> Phantoms.TTerm Context.Context
contextWithTrace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "messages")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "other")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.context.InContext
inContext :: Phantoms.TTerm e -> Phantoms.TTerm Context.Context -> Phantoms.TTerm (Context.InContext e)
inContext object context =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.InContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm object)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm context)}]}))
-- | DSL accessor for the context field of hydra.context.InContext
inContextContext :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm Context.Context
inContextContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
        Core.projectionField = (Core.Name "context")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the object field of hydra.context.InContext
inContextObject :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm e
inContextObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
        Core.projectionField = (Core.Name "object")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the context field of hydra.context.InContext
inContextWithContext :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm Context.Context -> Phantoms.TTerm (Context.InContext e)
inContextWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.InContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
              Core.projectionField = (Core.Name "object")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the object field of hydra.context.InContext
inContextWithObject :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm e -> Phantoms.TTerm (Context.InContext e)
inContextWithObject original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.InContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
              Core.projectionField = (Core.Name "context")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
