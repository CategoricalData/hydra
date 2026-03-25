-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.context

module Hydra.Dsl.Context where

import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

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

contextMessages :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String]
contextMessages x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "messages")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextOther :: Phantoms.TTerm Context.Context -> Phantoms.TTerm (M.Map Core.Name Core.Term)
contextOther x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "other")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextTrace :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String]
contextTrace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.Context"),
        Core.projectionField = (Core.Name "trace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

contextWithMessages :: Phantoms.TTerm Context.Context -> Phantoms.TTerm [String] -> Phantoms.TTerm Context.Context
contextWithMessages original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "trace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "other")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

contextWithOther :: Phantoms.TTerm Context.Context -> Phantoms.TTerm (M.Map Core.Name Core.Term) -> Phantoms.TTerm Context.Context
contextWithOther original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.Context"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "trace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "trace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "messages"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "messages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "messages")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "other"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.Context"),
              Core.projectionField = (Core.Name "other")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

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

inContextContext :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm Context.Context
inContextContext x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
        Core.projectionField = (Core.Name "context")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inContextObject :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm e
inContextObject x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
        Core.projectionField = (Core.Name "object")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

inContextWithContext :: Phantoms.TTerm (Context.InContext e) -> Phantoms.TTerm Context.Context -> Phantoms.TTerm (Context.InContext e)
inContextWithContext original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.context.InContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "object"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
              Core.projectionField = (Core.Name "object")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "context"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

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
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.context.InContext"),
              Core.projectionField = (Core.Name "context")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
