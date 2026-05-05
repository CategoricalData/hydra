-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.coders

module Hydra.Dsl.Coders where
import qualified Hydra.Coders as Coders
import qualified Hydra.Context as Context
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.coders.Adapter
adapter :: Phantoms.TTerm Bool -> Phantoms.TTerm t1 -> Phantoms.TTerm t2 -> Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2)
adapter isLossy source target coder =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm isLossy)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Phantoms.unTTerm coder)}]}))
-- | DSL accessor for the coder field of hydra.coders.Adapter
adapterCoder :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Coders.Coder v1 v2)
adapterCoder x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionField = (Core.Name "coder")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.coders.AdapterContext
adapterContext :: Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Coders.Language -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Phantoms.TTerm Coders.AdapterContext
adapterContext graph language adapters =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm language)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Phantoms.unTTerm adapters)}]}))
-- | DSL accessor for the adapters field of hydra.coders.AdapterContext
adapterContextAdapters :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term))
adapterContextAdapters x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "adapters")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the graph field of hydra.coders.AdapterContext
adapterContextGraph :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Graph.Graph
adapterContextGraph x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "graph")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the language field of hydra.coders.AdapterContext
adapterContextLanguage :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Coders.Language
adapterContextLanguage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionField = (Core.Name "language")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the adapters field of hydra.coders.AdapterContext
adapterContextWithAdapters :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithAdapters original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "language")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the graph field of hydra.coders.AdapterContext
adapterContextWithGraph :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Graph.Graph -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithGraph original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "language")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "adapters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the language field of hydra.coders.AdapterContext
adapterContextWithLanguage :: Phantoms.TTerm Coders.AdapterContext -> Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.AdapterContext
adapterContextWithLanguage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "graph")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionField = (Core.Name "adapters")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL accessor for the isLossy field of hydra.coders.Adapter
adapterIsLossy :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm Bool
adapterIsLossy x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionField = (Core.Name "isLossy")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the source field of hydra.coders.Adapter
adapterSource :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t1
adapterSource x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionField = (Core.Name "source")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the target field of hydra.coders.Adapter
adapterTarget :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t2
adapterTarget x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionField = (Core.Name "target")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the coder field of hydra.coders.Adapter
adapterWithCoder :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithCoder original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the isLossy field of hydra.coders.Adapter
adapterWithIsLossy :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm Bool -> Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithIsLossy original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "coder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the source field of hydra.coders.Adapter
adapterWithSource :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t1 -> Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithSource original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "target")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "coder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the target field of hydra.coders.Adapter
adapterWithTarget :: Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm t2 -> Phantoms.TTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithTarget original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "isLossy")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "source")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionField = (Core.Name "coder")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.coders.Bicoder
bicoder :: Phantoms.TTerm (t1 -> Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Coders.Adapter t2 t1 v2 v1) -> Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2)
bicoder encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.coders.Bicoder
bicoderDecode :: Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Coders.Adapter t2 t1 v2 v1)
bicoderDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
        Core.projectionField = (Core.Name "decode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the encode field of hydra.coders.Bicoder
bicoderEncode :: Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t1 -> Coders.Adapter t1 t2 v1 v2)
bicoderEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
        Core.projectionField = (Core.Name "encode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decode field of hydra.coders.Bicoder
bicoderWithDecode :: Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t2 -> Coders.Adapter t2 t1 v2 v1) -> Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2)
bicoderWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
              Core.projectionField = (Core.Name "encode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.coders.Bicoder
bicoderWithEncode :: Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2) -> Phantoms.TTerm (t1 -> Coders.Adapter t1 t2 v1 v2) -> Phantoms.TTerm (Coders.Bicoder t1 t2 v1 v2)
bicoderWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
              Core.projectionField = (Core.Name "decode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.coders.Coder
coder :: Phantoms.TTerm (Context.Context -> v1 -> Either Errors.Error v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either Errors.Error v1) -> Phantoms.TTerm (Coders.Coder v1 v2)
coder encode decode =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.coders.Coder
coderDecode :: Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either Errors.Error v1)
coderDecode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
        Core.projectionField = (Core.Name "decode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL injection for the decode variant of hydra.coders.CoderDirection
coderDirectionDecode :: Phantoms.TTerm Coders.CoderDirection
coderDirectionDecode =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the encode variant of hydra.coders.CoderDirection
coderDirectionEncode :: Phantoms.TTerm Coders.CoderDirection
coderDirectionEncode =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "encode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the encode field of hydra.coders.Coder
coderEncode :: Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v1 -> Either Errors.Error v2)
coderEncode x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
        Core.projectionField = (Core.Name "encode")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the decode field of hydra.coders.Coder
coderWithDecode :: Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v2 -> Either Errors.Error v1) -> Phantoms.TTerm (Coders.Coder v1 v2)
coderWithDecode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
              Core.projectionField = (Core.Name "encode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.coders.Coder
coderWithEncode :: Phantoms.TTerm (Coders.Coder v1 v2) -> Phantoms.TTerm (Context.Context -> v1 -> Either Errors.Error v2) -> Phantoms.TTerm (Coders.Coder v1 v2)
coderWithEncode original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
              Core.projectionField = (Core.Name "decode")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.coders.Language
language :: Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm Coders.Language
language name constraints =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm constraints)}]}))
-- | DSL accessor for the constraints field of hydra.coders.Language
languageConstraints :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraints x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionField = (Core.Name "constraints")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.coders.LanguageConstraints
languageConstraints2 :: Phantoms.TTerm (S.Set Variants.EliminationVariant) -> Phantoms.TTerm (S.Set Variants.LiteralVariant) -> Phantoms.TTerm (S.Set Core.FloatType) -> Phantoms.TTerm (S.Set Variants.FunctionVariant) -> Phantoms.TTerm (S.Set Core.IntegerType) -> Phantoms.TTerm (S.Set Variants.TermVariant) -> Phantoms.TTerm (S.Set Variants.TypeVariant) -> Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraints2 eliminationVariants literalVariants floatTypes functionVariants integerTypes termVariants typeVariants types =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Phantoms.unTTerm eliminationVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Phantoms.unTTerm literalVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Phantoms.unTTerm floatTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Phantoms.unTTerm functionVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Phantoms.unTTerm integerTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Phantoms.unTTerm termVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Phantoms.unTTerm typeVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm types)}]}))
-- | DSL accessor for the eliminationVariants field of hydra.coders.LanguageConstraints
languageConstraintsEliminationVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.EliminationVariant)
languageConstraintsEliminationVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "eliminationVariants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the floatTypes field of hydra.coders.LanguageConstraints
languageConstraintsFloatTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.FloatType)
languageConstraintsFloatTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "floatTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the functionVariants field of hydra.coders.LanguageConstraints
languageConstraintsFunctionVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.FunctionVariant)
languageConstraintsFunctionVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "functionVariants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the integerTypes field of hydra.coders.LanguageConstraints
languageConstraintsIntegerTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.IntegerType)
languageConstraintsIntegerTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "integerTypes")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the literalVariants field of hydra.coders.LanguageConstraints
languageConstraintsLiteralVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.LiteralVariant)
languageConstraintsLiteralVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "literalVariants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the termVariants field of hydra.coders.LanguageConstraints
languageConstraintsTermVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TermVariant)
languageConstraintsTermVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "termVariants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeVariants field of hydra.coders.LanguageConstraints
languageConstraintsTypeVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TypeVariant)
languageConstraintsTypeVariants x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "typeVariants")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the types field of hydra.coders.LanguageConstraints
languageConstraintsTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (Core.Type -> Bool)
languageConstraintsTypes x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionField = (Core.Name "types")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the eliminationVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithEliminationVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.EliminationVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithEliminationVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the floatTypes field of hydra.coders.LanguageConstraints
languageConstraintsWithFloatTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.FloatType) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithFloatTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the functionVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithFunctionVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.FunctionVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithFunctionVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the integerTypes field of hydra.coders.LanguageConstraints
languageConstraintsWithIntegerTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Core.IntegerType) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithIntegerTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the literalVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithLiteralVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.LiteralVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithLiteralVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the termVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithTermVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TermVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTermVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithTypeVariants :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (S.Set Variants.TypeVariant) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTypeVariants original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "types")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the types field of hydra.coders.LanguageConstraints
languageConstraintsWithTypes :: Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm (Core.Type -> Bool) -> Phantoms.TTerm Coders.LanguageConstraints
languageConstraintsWithTypes original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "eliminationVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "eliminationVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "functionVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "functionVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "termVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionField = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the name field of hydra.coders.Language
languageName :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageName
languageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.coders.LanguageName wrapper
languageName2 :: Phantoms.TTerm String -> Phantoms.TTerm Coders.LanguageName
languageName2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL updater for the constraints field of hydra.coders.Language
languageWithConstraints :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageConstraints -> Phantoms.TTerm Coders.Language
languageWithConstraints original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the name field of hydra.coders.Language
languageWithName :: Phantoms.TTerm Coders.Language -> Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm Coders.Language
languageWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionField = (Core.Name "constraints")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL injection for the post variant of hydra.coders.TraversalOrder
traversalOrderPost :: Phantoms.TTerm Coders.TraversalOrder
traversalOrderPost =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "post"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pre variant of hydra.coders.TraversalOrder
traversalOrderPre :: Phantoms.TTerm Coders.TraversalOrder
traversalOrderPre =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pre"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.coders.LanguageName
unLanguageName :: Phantoms.TTerm Coders.LanguageName -> Phantoms.TTerm String
unLanguageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coders.LanguageName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
