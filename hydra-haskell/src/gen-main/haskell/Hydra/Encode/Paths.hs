-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.paths

module Hydra.Encode.Paths where

import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Paths as Paths
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

subtermEdge :: Paths.SubtermEdge -> Core.Term
subtermEdge x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (subtermNode (Paths.subtermEdgeSource x))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (subtermPath (Paths.subtermEdgePath x))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (subtermNode (Paths.subtermEdgeTarget x))}]})

subtermGraph :: Paths.SubtermGraph -> Core.Term
subtermGraph x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map subtermNode xs)) (Paths.subtermGraphNodes x))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map subtermEdge xs)) (Paths.subtermGraphEdges x))}]})

subtermNode :: Paths.SubtermNode -> Core.Term
subtermNode x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtermNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Paths.subtermNodeName x))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Paths.subtermNodeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Paths.subtermNodeId x))}]})

subtermPath :: Paths.SubtermPath -> Core.Term
subtermPath x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtermPath"),
      Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map subtermStep xs)) (Paths.unSubtermPath x))})

subtermStep :: Paths.SubtermStep -> Core.Term
subtermStep x =
    case x of
      Paths.SubtermStepAnnotatedBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "annotatedBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepApplicationFunction -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "applicationFunction"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepApplicationArgument -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "applicationArgument"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepLambdaBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "lambdaBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepUnionCasesDefault -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unionCasesDefault"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepUnionCasesBranch v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unionCasesBranch"),
          Core.fieldTerm = (Core_.name v0)}})
      Paths.SubtermStepLetBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "letBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepLetBinding v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "letBinding"),
          Core.fieldTerm = (Core_.name v0)}})
      Paths.SubtermStepListElement v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "listElement"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Paths.SubtermStepMapKey v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "mapKey"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Paths.SubtermStepMapValue v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "mapValue"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Paths.SubtermStepMaybeTerm -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "maybeTerm"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepProductTerm v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "productTerm"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Paths.SubtermStepRecordField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "recordField"),
          Core.fieldTerm = (Core_.name v0)}})
      Paths.SubtermStepSetElement v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "setElement"),
          Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v0)))}})
      Paths.SubtermStepSumTerm -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "sumTerm"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepTypeLambdaBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeLambdaBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepTypeApplicationTerm -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "typeApplicationTerm"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepInjectionTerm -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "injectionTerm"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtermStepWrappedTerm -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtermStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrappedTerm"),
          Core.fieldTerm = Core.TermUnit}})

subtypeEdge :: Paths.SubtypeEdge -> Core.Term
subtypeEdge x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeEdge"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "source"),
          Core.fieldTerm = (subtypeNode (Paths.subtypeEdgeSource x))},
        Core.Field {
          Core.fieldName = (Core.Name "path"),
          Core.fieldTerm = (subtypePath (Paths.subtypeEdgePath x))},
        Core.Field {
          Core.fieldName = (Core.Name "target"),
          Core.fieldTerm = (subtypeNode (Paths.subtypeEdgeTarget x))}]})

subtypeGraph :: Paths.SubtypeGraph -> Core.Term
subtypeGraph x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeGraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "nodes"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map subtypeNode xs)) (Paths.subtypeGraphNodes x))},
        Core.Field {
          Core.fieldName = (Core.Name "edges"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map subtypeEdge xs)) (Paths.subtypeGraphEdges x))}]})

subtypeNode :: Paths.SubtypeNode -> Core.Term
subtypeNode x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.paths.SubtypeNode"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core_.name (Paths.subtypeNodeName x))},
        Core.Field {
          Core.fieldName = (Core.Name "label"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Paths.subtypeNodeLabel x))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Paths.subtypeNodeId x))}]})

subtypePath :: Paths.SubtypePath -> Core.Term
subtypePath x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.paths.SubtypePath"),
      Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map subtypeStep xs)) (Paths.unSubtypePath x))})

subtypeStep :: Paths.SubtypeStep -> Core.Term
subtypeStep x =
    case x of
      Paths.SubtypeStepAnnotatedBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "annotatedBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepApplicationFunction -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "applicationFunction"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepApplicationArgument -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "applicationArgument"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepEitherLeft -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "eitherLeft"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepEitherRight -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "eitherRight"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepForallBody -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "forallBody"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepFunctionDomain -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "functionDomain"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepFunctionCodomain -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "functionCodomain"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepListElement -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "listElement"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepMapKeys -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "mapKeys"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepMapValues -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "mapValues"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepMaybeElement -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "maybeElement"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepPairFirst -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pairFirst"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepPairSecond -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "pairSecond"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepRecordField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "recordField"),
          Core.fieldTerm = (Core_.name v0)}})
      Paths.SubtypeStepSetElement -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "setElement"),
          Core.fieldTerm = Core.TermUnit}})
      Paths.SubtypeStepUnionField v0 -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "unionField"),
          Core.fieldTerm = (Core_.name v0)}})
      Paths.SubtypeStepWrappedType -> Core.TermUnion (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.paths.SubtypeStep"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "wrappedType"),
          Core.fieldTerm = Core.TermUnit}})
