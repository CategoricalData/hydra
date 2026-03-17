-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.protobuf.sourceContext

module Hydra.Dsl.Ext.Protobuf.SourceContext where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Protobuf.SourceContext as SourceContext
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

sourceContext :: (Phantoms.TTerm String -> Phantoms.TTerm SourceContext.SourceContext)
sourceContext fileName = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.protobuf.sourceContext.SourceContext"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fileName"),
      Core.fieldTerm = (Phantoms.unTTerm fileName)}]})))

sourceContextFileName :: (Phantoms.TTerm SourceContext.SourceContext -> Phantoms.TTerm String)
sourceContextFileName x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.protobuf.sourceContext.SourceContext"),
    Core.projectionField = (Core.Name "fileName")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

sourceContextWithFileName :: (Phantoms.TTerm SourceContext.SourceContext -> Phantoms.TTerm String -> Phantoms.TTerm SourceContext.SourceContext)
sourceContextWithFileName original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.protobuf.sourceContext.SourceContext"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "fileName"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))
