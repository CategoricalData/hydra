-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.coders

module Hydra.Dsl.Coders where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Errors as DslErrors
import qualified Hydra.Dsl.Graph as DslGraph
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Dsl.Util as DslUtil
import qualified Hydra.Dsl.Variants as DslVariants
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | DSL constructor for hydra.coders.Adapter
adapter :: Typed.TypedTerm Bool -> Typed.TypedTerm t1 -> Typed.TypedTerm t2 -> Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2)
adapter isLossy source target coder =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Typed.unTypedTerm isLossy)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm source)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm target)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Typed.unTypedTerm coder)}]}))
-- | DSL accessor for the coder field of hydra.coders.Adapter
adapterCoder :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm (Coders.Coder v1 v2)
adapterCoder x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionFieldName = (Core.Name "coder")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coders.AdapterContext
adapterContext :: Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Coders.Language -> Typed.TypedTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Typed.TypedTerm Coders.AdapterContext
adapterContext graph language adapters =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm graph)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Typed.unTypedTerm language)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Typed.unTypedTerm adapters)}]}))
-- | DSL accessor for the adapters field of hydra.coders.AdapterContext
adapterContextAdapters :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term))
adapterContextAdapters x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionFieldName = (Core.Name "adapters")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the graph field of hydra.coders.AdapterContext
adapterContextGraph :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm Graph.Graph
adapterContextGraph x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionFieldName = (Core.Name "graph")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the language field of hydra.coders.AdapterContext
adapterContextLanguage :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm Coders.Language
adapterContextLanguage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
        Core.projectionFieldName = (Core.Name "language")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the adapters field of hydra.coders.AdapterContext
adapterContextWithAdapters :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm (M.Map Core.Name (Coders.Adapter Core.Type Core.Type Core.Term Core.Term)) -> Typed.TypedTerm Coders.AdapterContext
adapterContextWithAdapters original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "language")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the graph field of hydra.coders.AdapterContext
adapterContextWithGraph :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm Graph.Graph -> Typed.TypedTerm Coders.AdapterContext
adapterContextWithGraph original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "language")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "adapters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the language field of hydra.coders.AdapterContext
adapterContextWithLanguage :: Typed.TypedTerm Coders.AdapterContext -> Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.AdapterContext
adapterContextWithLanguage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.AdapterContext"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "graph"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "graph")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "language"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "adapters"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.AdapterContext"),
              Core.projectionFieldName = (Core.Name "adapters")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL accessor for the isLossy field of hydra.coders.Adapter
adapterIsLossy :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm Bool
adapterIsLossy x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionFieldName = (Core.Name "isLossy")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the source field of hydra.coders.Adapter
adapterSource :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm t1
adapterSource x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionFieldName = (Core.Name "source")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the target field of hydra.coders.Adapter
adapterTarget :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm t2
adapterTarget x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
        Core.projectionFieldName = (Core.Name "target")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the coder field of hydra.coders.Adapter
adapterWithCoder :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithCoder original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "isLossy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the isLossy field of hydra.coders.Adapter
adapterWithIsLossy :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm Bool -> Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithIsLossy original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "coder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the source field of hydra.coders.Adapter
adapterWithSource :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm t1 -> Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithSource original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "isLossy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "target")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "coder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the target field of hydra.coders.Adapter
adapterWithTarget :: Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm t2 -> Typed.TypedTerm (Coders.Adapter t1 t2 v1 v2)
adapterWithTarget original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Adapter"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "isLossy"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "isLossy")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "source")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "coder"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Adapter"),
              Core.projectionFieldName = (Core.Name "coder")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coders.Bicoder
bicoder :: Typed.TypedTerm (t1 -> Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm (t2 -> Coders.Adapter t2 t1 v2 v1) -> Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2)
bicoder encode decode =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.coders.Bicoder
bicoderDecode :: Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2) -> Typed.TypedTerm (t2 -> Coders.Adapter t2 t1 v2 v1)
bicoderDecode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
        Core.projectionFieldName = (Core.Name "decode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the encode field of hydra.coders.Bicoder
bicoderEncode :: Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2) -> Typed.TypedTerm (t1 -> Coders.Adapter t1 t2 v1 v2)
bicoderEncode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
        Core.projectionFieldName = (Core.Name "encode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decode field of hydra.coders.Bicoder
bicoderWithDecode :: Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2) -> Typed.TypedTerm (t2 -> Coders.Adapter t2 t1 v2 v1) -> Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2)
bicoderWithDecode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.coders.Bicoder
bicoderWithEncode :: Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2) -> Typed.TypedTerm (t1 -> Coders.Adapter t1 t2 v1 v2) -> Typed.TypedTerm (Coders.Bicoder t1 t2 v1 v2)
bicoderWithEncode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Bicoder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Bicoder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coders.CaseConventions
caseConventions :: Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventions constant directory enumValue field file module_ term termVariable type_ typeVariable =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Typed.unTypedTerm constant)},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Typed.unTypedTerm directory)},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Typed.unTypedTerm enumValue)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm field)},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Typed.unTypedTerm file)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Typed.unTypedTerm termVariable)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Typed.unTypedTerm typeVariable)}]}))
-- | DSL accessor for the constant field of hydra.coders.CaseConventions
caseConventionsConstant :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsConstant x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "constant")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the directory field of hydra.coders.CaseConventions
caseConventionsDirectory :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsDirectory x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "directory")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the enumValue field of hydra.coders.CaseConventions
caseConventionsEnumValue :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsEnumValue x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "enumValue")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the field field of hydra.coders.CaseConventions
caseConventionsField :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsField x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "field")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the file field of hydra.coders.CaseConventions
caseConventionsFile :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsFile x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "file")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the module field of hydra.coders.CaseConventions
caseConventionsModule :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.coders.CaseConventions
caseConventionsTerm :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the termVariable field of hydra.coders.CaseConventions
caseConventionsTermVariable :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsTermVariable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "termVariable")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the type field of hydra.coders.CaseConventions
caseConventionsType :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsType x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVariable field of hydra.coders.CaseConventions
caseConventionsTypeVariable :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention
caseConventionsTypeVariable x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
        Core.projectionFieldName = (Core.Name "typeVariable")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the constant field of hydra.coders.CaseConventions
caseConventionsWithConstant :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithConstant original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the directory field of hydra.coders.CaseConventions
caseConventionsWithDirectory :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithDirectory original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the enumValue field of hydra.coders.CaseConventions
caseConventionsWithEnumValue :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithEnumValue original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the field field of hydra.coders.CaseConventions
caseConventionsWithField :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithField original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the file field of hydra.coders.CaseConventions
caseConventionsWithFile :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithFile original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the module field of hydra.coders.CaseConventions
caseConventionsWithModule :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the term field of hydra.coders.CaseConventions
caseConventionsWithTerm :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the termVariable field of hydra.coders.CaseConventions
caseConventionsWithTermVariable :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithTermVariable original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the type field of hydra.coders.CaseConventions
caseConventionsWithType :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithType original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "typeVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeVariable field of hydra.coders.CaseConventions
caseConventionsWithTypeVariable :: Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.CaseConvention -> Typed.TypedTerm Coders.CaseConventions
caseConventionsWithTypeVariable original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.CaseConventions"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "constant"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "constant")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directory"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "directory")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "enumValue"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "enumValue")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "field"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "field")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "file"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "file")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariable"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "termVariable")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.CaseConventions"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariable"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.coders.Coder
coder :: Typed.TypedTerm (Typing.InferenceContext -> v1 -> Either Errors.Error v2) -> Typed.TypedTerm (Typing.InferenceContext -> v2 -> Either Errors.Error v1) -> Typed.TypedTerm (Coders.Coder v1 v2)
coder encode decode =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm encode)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm decode)}]}))
-- | DSL accessor for the decode field of hydra.coders.Coder
coderDecode :: Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Typing.InferenceContext -> v2 -> Either Errors.Error v1)
coderDecode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
        Core.projectionFieldName = (Core.Name "decode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the decode variant of hydra.coders.CoderDirection
coderDirectionDecode :: Typed.TypedTerm Coders.CoderDirection
coderDirectionDecode =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "decode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the encode variant of hydra.coders.CoderDirection
coderDirectionEncode :: Typed.TypedTerm Coders.CoderDirection
coderDirectionEncode =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.CoderDirection"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "encode"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the encode field of hydra.coders.Coder
coderEncode :: Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Typing.InferenceContext -> v1 -> Either Errors.Error v2)
coderEncode x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
        Core.projectionFieldName = (Core.Name "encode")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the decode field of hydra.coders.Coder
coderWithDecode :: Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Typing.InferenceContext -> v2 -> Either Errors.Error v1) -> Typed.TypedTerm (Coders.Coder v1 v2)
coderWithDecode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
              Core.projectionFieldName = (Core.Name "encode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the encode field of hydra.coders.Coder
coderWithEncode :: Typed.TypedTerm (Coders.Coder v1 v2) -> Typed.TypedTerm (Typing.InferenceContext -> v1 -> Either Errors.Error v2) -> Typed.TypedTerm (Coders.Coder v1 v2)
coderWithEncode original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Coder"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "encode"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "decode"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Coder"),
              Core.projectionFieldName = (Core.Name "decode")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.coders.Language
language :: Typed.TypedTerm Coders.LanguageName -> Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Coders.LanguageFeature) -> Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Util.FileExtension -> Typed.TypedTerm Coders.Language
language name constraints supportedFeatures caseConventions defaultFileExtension =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm constraints)},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Typed.unTypedTerm supportedFeatures)},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Typed.unTypedTerm caseConventions)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Typed.unTypedTerm defaultFileExtension)}]}))
-- | DSL accessor for the caseConventions field of hydra.coders.Language
languageCaseConventions :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.CaseConventions
languageCaseConventions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionFieldName = (Core.Name "caseConventions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the constraints field of hydra.coders.Language
languageConstraints :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraints x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionFieldName = (Core.Name "constraints")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.coders.LanguageConstraints
languageConstraints2 :: Typed.TypedTerm (S.Set Variants.LiteralVariant) -> Typed.TypedTerm (S.Set Core.FloatType) -> Typed.TypedTerm (S.Set Core.IntegerType) -> Typed.TypedTerm (S.Set Variants.TermVariant) -> Typed.TypedTerm (S.Set Variants.TypeVariant) -> Typed.TypedTerm (Core.Type -> Bool) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraints2 literalVariants floatTypes integerTypes termVariants typeVariants types =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Typed.unTypedTerm literalVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Typed.unTypedTerm floatTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Typed.unTypedTerm integerTypes)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Typed.unTypedTerm termVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Typed.unTypedTerm typeVariants)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm types)}]}))
-- | DSL accessor for the floatTypes field of hydra.coders.LanguageConstraints
languageConstraintsFloatTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Core.FloatType)
languageConstraintsFloatTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "floatTypes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the integerTypes field of hydra.coders.LanguageConstraints
languageConstraintsIntegerTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Core.IntegerType)
languageConstraintsIntegerTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "integerTypes")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the literalVariants field of hydra.coders.LanguageConstraints
languageConstraintsLiteralVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.LiteralVariant)
languageConstraintsLiteralVariants x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "literalVariants")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the termVariants field of hydra.coders.LanguageConstraints
languageConstraintsTermVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.TermVariant)
languageConstraintsTermVariants x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "termVariants")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeVariants field of hydra.coders.LanguageConstraints
languageConstraintsTypeVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.TypeVariant)
languageConstraintsTypeVariants x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "typeVariants")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the types field of hydra.coders.LanguageConstraints
languageConstraintsTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (Core.Type -> Bool)
languageConstraintsTypes x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
        Core.projectionFieldName = (Core.Name "types")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the floatTypes field of hydra.coders.LanguageConstraints
languageConstraintsWithFloatTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Core.FloatType) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithFloatTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "termVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the integerTypes field of hydra.coders.LanguageConstraints
languageConstraintsWithIntegerTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Core.IntegerType) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithIntegerTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "termVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the literalVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithLiteralVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.LiteralVariant) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithLiteralVariants original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "termVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the termVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithTermVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.TermVariant) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithTermVariants original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeVariants field of hydra.coders.LanguageConstraints
languageConstraintsWithTypeVariants :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (S.Set Variants.TypeVariant) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithTypeVariants original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "termVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "types")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the types field of hydra.coders.LanguageConstraints
languageConstraintsWithTypes :: Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm (Core.Type -> Bool) -> Typed.TypedTerm Coders.LanguageConstraints
languageConstraintsWithTypes original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "literalVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "literalVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "floatTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "floatTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "integerTypes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "integerTypes")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "termVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeVariants"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.LanguageConstraints"),
              Core.projectionFieldName = (Core.Name "typeVariants")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "types"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the defaultFileExtension field of hydra.coders.Language
languageDefaultFileExtension :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Util.FileExtension
languageDefaultFileExtension x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionFieldName = (Core.Name "defaultFileExtension")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL injection for the nestedCaseStatements variant of hydra.coders.LanguageFeature
languageFeatureNestedCaseStatements :: Typed.TypedTerm Coders.LanguageFeature
languageFeatureNestedCaseStatements =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedCaseStatements"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the nestedPolymorphicLetBindings variant of hydra.coders.LanguageFeature
languageFeatureNestedPolymorphicLetBindings :: Typed.TypedTerm Coders.LanguageFeature
languageFeatureNestedPolymorphicLetBindings =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nestedPolymorphicLetBindings"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the partialApplication variant of hydra.coders.LanguageFeature
languageFeaturePartialApplication :: Typed.TypedTerm Coders.LanguageFeature
languageFeaturePartialApplication =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.LanguageFeature"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "partialApplication"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the name field of hydra.coders.Language
languageName :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.LanguageName
languageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.coders.LanguageName wrapper
languageName2 :: Typed.TypedTerm String -> Typed.TypedTerm Coders.LanguageName
languageName2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.coders.LanguageName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL accessor for the supportedFeatures field of hydra.coders.Language
languageSupportedFeatures :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm (S.Set Coders.LanguageFeature)
languageSupportedFeatures x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
        Core.projectionFieldName = (Core.Name "supportedFeatures")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the caseConventions field of hydra.coders.Language
languageWithCaseConventions :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.CaseConventions -> Typed.TypedTerm Coders.Language
languageWithCaseConventions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "supportedFeatures")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "defaultFileExtension")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the constraints field of hydra.coders.Language
languageWithConstraints :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.LanguageConstraints -> Typed.TypedTerm Coders.Language
languageWithConstraints original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "supportedFeatures")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "caseConventions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "defaultFileExtension")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the defaultFileExtension field of hydra.coders.Language
languageWithDefaultFileExtension :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Util.FileExtension -> Typed.TypedTerm Coders.Language
languageWithDefaultFileExtension original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "supportedFeatures")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "caseConventions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.coders.Language
languageWithName :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm Coders.LanguageName -> Typed.TypedTerm Coders.Language
languageWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "supportedFeatures")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "caseConventions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "defaultFileExtension")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the supportedFeatures field of hydra.coders.Language
languageWithSupportedFeatures :: Typed.TypedTerm Coders.Language -> Typed.TypedTerm (S.Set Coders.LanguageFeature) -> Typed.TypedTerm Coders.Language
languageWithSupportedFeatures original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.coders.Language"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "constraints"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "constraints")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "supportedFeatures"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "caseConventions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "caseConventions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultFileExtension"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.coders.Language"),
              Core.projectionFieldName = (Core.Name "defaultFileExtension")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the post variant of hydra.coders.TraversalOrder
traversalOrderPost :: Typed.TypedTerm Coders.TraversalOrder
traversalOrderPost =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "post"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL injection for the pre variant of hydra.coders.TraversalOrder
traversalOrderPre :: Typed.TypedTerm Coders.TraversalOrder
traversalOrderPre =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.coders.TraversalOrder"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "pre"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL accessor for the body of hydra.coders.LanguageName
unLanguageName :: Typed.TypedTerm Coders.LanguageName -> Typed.TypedTerm String
unLanguageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.coders.LanguageName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
