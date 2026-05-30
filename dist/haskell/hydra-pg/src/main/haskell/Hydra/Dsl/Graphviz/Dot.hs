-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.graphviz.dot

module Hydra.Dsl.Graphviz.Dot where

import qualified Hydra.Core as Core
import qualified Hydra.Graphviz.Dot as Dot
import qualified Hydra.Typed as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

attrList :: Phantoms.TypedTerm [[Dot.EqualityPair]] -> Phantoms.TypedTerm Dot.AttrList
attrList x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphviz.dot.AttrList"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

attrStmt :: Phantoms.TypedTerm Dot.AttrType -> Phantoms.TypedTerm Dot.AttrList -> Phantoms.TypedTerm Dot.AttrStmt
attrStmt type_ attributes =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm type_)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm attributes)}]}))

attrStmtAttributes :: Phantoms.TypedTerm Dot.AttrStmt -> Phantoms.TypedTerm Dot.AttrList
attrStmtAttributes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
        Core.projectionFieldName = (Core.Name "attributes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

attrStmtType :: Phantoms.TypedTerm Dot.AttrStmt -> Phantoms.TypedTerm Dot.AttrType
attrStmtType x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
        Core.projectionFieldName = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

attrStmtWithAttributes :: Phantoms.TypedTerm Dot.AttrStmt -> Phantoms.TypedTerm Dot.AttrList -> Phantoms.TypedTerm Dot.AttrStmt
attrStmtWithAttributes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
              Core.projectionFieldName = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

attrStmtWithType :: Phantoms.TypedTerm Dot.AttrStmt -> Phantoms.TypedTerm Dot.AttrType -> Phantoms.TypedTerm Dot.AttrStmt
attrStmtWithType original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.AttrStmt"),
              Core.projectionFieldName = (Core.Name "attributes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

attrTypeEdge :: Phantoms.TypedTerm Dot.AttrType
attrTypeEdge =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.AttrType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = Core.TermUnit}}))

attrTypeGraph :: Phantoms.TypedTerm Dot.AttrType
attrTypeGraph =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.AttrType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "graph"),
        Core.fieldTerm = Core.TermUnit}}))

attrTypeNode :: Phantoms.TypedTerm Dot.AttrType
attrTypeNode =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.AttrType"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtC :: Phantoms.TypedTerm Dot.CompassPt
compassPtC =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "c"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtE :: Phantoms.TypedTerm Dot.CompassPt
compassPtE =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "e"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtN :: Phantoms.TypedTerm Dot.CompassPt
compassPtN =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "n"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtNe :: Phantoms.TypedTerm Dot.CompassPt
compassPtNe =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "ne"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtNone :: Phantoms.TypedTerm Dot.CompassPt
compassPtNone =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "none"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtNw :: Phantoms.TypedTerm Dot.CompassPt
compassPtNw =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "nw"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtS :: Phantoms.TypedTerm Dot.CompassPt
compassPtS =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "s"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtSe :: Phantoms.TypedTerm Dot.CompassPt
compassPtSe =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "se"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtSw :: Phantoms.TypedTerm Dot.CompassPt
compassPtSw =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "sw"),
        Core.fieldTerm = Core.TermUnit}}))

compassPtW :: Phantoms.TypedTerm Dot.CompassPt
compassPtW =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.CompassPt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "w"),
        Core.fieldTerm = Core.TermUnit}}))

edgeStmt :: Phantoms.TypedTerm Dot.NodeOrSubgraph -> Phantoms.TypedTerm [Dot.NodeOrSubgraph] -> Phantoms.TypedTerm (Maybe Dot.AttrList) -> Phantoms.TypedTerm Dot.EdgeStmt
edgeStmt left right attributes =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm attributes)}]}))

edgeStmtAttributes :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm (Maybe Dot.AttrList)
edgeStmtAttributes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
        Core.projectionFieldName = (Core.Name "attributes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeStmtLeft :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm Dot.NodeOrSubgraph
edgeStmtLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeStmtRight :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm [Dot.NodeOrSubgraph]
edgeStmtRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

edgeStmtWithAttributes :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm (Maybe Dot.AttrList) -> Phantoms.TypedTerm Dot.EdgeStmt
edgeStmtWithAttributes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

edgeStmtWithLeft :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm Dot.NodeOrSubgraph -> Phantoms.TypedTerm Dot.EdgeStmt
edgeStmtWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "attributes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

edgeStmtWithRight :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm [Dot.NodeOrSubgraph] -> Phantoms.TypedTerm Dot.EdgeStmt
edgeStmtWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EdgeStmt"),
              Core.projectionFieldName = (Core.Name "attributes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

equalityPair :: Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm Dot.EqualityPair
equalityPair left right =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm left)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm right)}]}))

equalityPairLeft :: Phantoms.TypedTerm Dot.EqualityPair -> Phantoms.TypedTerm Dot.Id
equalityPairLeft x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
        Core.projectionFieldName = (Core.Name "left")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

equalityPairRight :: Phantoms.TypedTerm Dot.EqualityPair -> Phantoms.TypedTerm Dot.Id
equalityPairRight x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
        Core.projectionFieldName = (Core.Name "right")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

equalityPairWithLeft :: Phantoms.TypedTerm Dot.EqualityPair -> Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm Dot.EqualityPair
equalityPairWithLeft original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
              Core.projectionFieldName = (Core.Name "right")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

equalityPairWithRight :: Phantoms.TypedTerm Dot.EqualityPair -> Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm Dot.EqualityPair
equalityPairWithRight original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "left"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.EqualityPair"),
              Core.projectionFieldName = (Core.Name "left")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "right"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

graph :: Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm (Maybe Dot.Id) -> Phantoms.TypedTerm [Dot.Stmt] -> Phantoms.TypedTerm Dot.Graph
graph strict directed id statements =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "strict"),
          Core.fieldTerm = (Phantoms.unTypedTerm strict)},
        Core.Field {
          Core.fieldName = (Core.Name "directed"),
          Core.fieldTerm = (Phantoms.unTypedTerm directed)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTypedTerm statements)}]}))

graphDirected :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm Bool
graphDirected x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
        Core.projectionFieldName = (Core.Name "directed")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphId :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm (Maybe Dot.Id)
graphId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphStatements :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm [Dot.Stmt]
graphStatements x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphStrict :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm Bool
graphStrict x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
        Core.projectionFieldName = (Core.Name "strict")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

graphWithDirected :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Dot.Graph
graphWithDirected original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "strict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "strict")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directed"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphWithId :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm (Maybe Dot.Id) -> Phantoms.TypedTerm Dot.Graph
graphWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "strict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "strict")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "directed")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

graphWithStatements :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm [Dot.Stmt] -> Phantoms.TypedTerm Dot.Graph
graphWithStatements original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "strict"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "strict")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "directed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "directed")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

graphWithStrict :: Phantoms.TypedTerm Dot.Graph -> Phantoms.TypedTerm Bool -> Phantoms.TypedTerm Dot.Graph
graphWithStrict original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "strict"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "directed"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "directed")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Graph"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

id :: Phantoms.TypedTerm String -> Phantoms.TypedTerm Dot.Id
id x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphviz.dot.Id"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

nodeId :: Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm (Maybe Dot.Port) -> Phantoms.TypedTerm Dot.NodeId
nodeId id port =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "port"),
          Core.fieldTerm = (Phantoms.unTypedTerm port)}]}))

nodeIdId :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm Dot.Id
nodeIdId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodeIdPort :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm (Maybe Dot.Port)
nodeIdPort x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
        Core.projectionFieldName = (Core.Name "port")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodeIdWithId :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm Dot.NodeId
nodeIdWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "port"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
              Core.projectionFieldName = (Core.Name "port")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

nodeIdWithPort :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm (Maybe Dot.Port) -> Phantoms.TypedTerm Dot.NodeId
nodeIdWithPort original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeId"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "port"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nodeOrSubgraphNode :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm Dot.NodeOrSubgraph
nodeOrSubgraphNode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.NodeOrSubgraph"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nodeOrSubgraphSubgraph :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm Dot.NodeOrSubgraph
nodeOrSubgraphSubgraph x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.NodeOrSubgraph"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subgraph"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

nodeStmt :: Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm (Maybe Dot.AttrList) -> Phantoms.TypedTerm Dot.NodeStmt
nodeStmt id attributes =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm attributes)}]}))

nodeStmtAttributes :: Phantoms.TypedTerm Dot.NodeStmt -> Phantoms.TypedTerm (Maybe Dot.AttrList)
nodeStmtAttributes x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
        Core.projectionFieldName = (Core.Name "attributes")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodeStmtId :: Phantoms.TypedTerm Dot.NodeStmt -> Phantoms.TypedTerm Dot.NodeId
nodeStmtId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

nodeStmtWithAttributes :: Phantoms.TypedTerm Dot.NodeStmt -> Phantoms.TypedTerm (Maybe Dot.AttrList) -> Phantoms.TypedTerm Dot.NodeStmt
nodeStmtWithAttributes original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

nodeStmtWithId :: Phantoms.TypedTerm Dot.NodeStmt -> Phantoms.TypedTerm Dot.NodeId -> Phantoms.TypedTerm Dot.NodeStmt
nodeStmtWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "attributes"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.NodeStmt"),
              Core.projectionFieldName = (Core.Name "attributes")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

port :: Phantoms.TypedTerm (Maybe Dot.Id) -> Phantoms.TypedTerm (Maybe Dot.CompassPt) -> Phantoms.TypedTerm Dot.Port
port id position =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Port"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm id)},
        Core.Field {
          Core.fieldName = (Core.Name "position"),
          Core.fieldTerm = (Phantoms.unTypedTerm position)}]}))

portId :: Phantoms.TypedTerm Dot.Port -> Phantoms.TypedTerm (Maybe Dot.Id)
portId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Port"),
        Core.projectionFieldName = (Core.Name "id")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

portPosition :: Phantoms.TypedTerm Dot.Port -> Phantoms.TypedTerm (Maybe Dot.CompassPt)
portPosition x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Port"),
        Core.projectionFieldName = (Core.Name "position")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

portWithId :: Phantoms.TypedTerm Dot.Port -> Phantoms.TypedTerm (Maybe Dot.Id) -> Phantoms.TypedTerm Dot.Port
portWithId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Port"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "position"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Port"),
              Core.projectionFieldName = (Core.Name "position")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

portWithPosition :: Phantoms.TypedTerm Dot.Port -> Phantoms.TypedTerm (Maybe Dot.CompassPt) -> Phantoms.TypedTerm Dot.Port
portWithPosition original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Port"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "id"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Port"),
              Core.projectionFieldName = (Core.Name "id")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "position"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

stmtAttr :: Phantoms.TypedTerm Dot.AttrStmt -> Phantoms.TypedTerm Dot.Stmt
stmtAttr x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.Stmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "attr"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stmtEdge :: Phantoms.TypedTerm Dot.EdgeStmt -> Phantoms.TypedTerm Dot.Stmt
stmtEdge x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.Stmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "edge"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stmtEquals :: Phantoms.TypedTerm Dot.EqualityPair -> Phantoms.TypedTerm Dot.Stmt
stmtEquals x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.Stmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "equals"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stmtNode :: Phantoms.TypedTerm Dot.NodeStmt -> Phantoms.TypedTerm Dot.Stmt
stmtNode x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.Stmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "node"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

stmtSubgraph :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm Dot.Stmt
stmtSubgraph x =
    Phantoms.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.graphviz.dot.Stmt"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "subgraph"),
        Core.fieldTerm = (Phantoms.unTypedTerm x)}}))

subgraph :: Phantoms.TypedTerm (Maybe Dot.SubgraphId) -> Phantoms.TypedTerm [Dot.Stmt] -> Phantoms.TypedTerm Dot.Subgraph
subgraph subgraphId statements =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subgraphId"),
          Core.fieldTerm = (Phantoms.unTypedTerm subgraphId)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTypedTerm statements)}]}))

subgraphId :: Phantoms.TypedTerm (Maybe Dot.Id) -> Phantoms.TypedTerm Dot.SubgraphId
subgraphId x =
    Phantoms.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.graphviz.dot.SubgraphId"),
      Core.wrappedTermBody = (Phantoms.unTypedTerm x)}))

subgraphStatements :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm [Dot.Stmt]
subgraphStatements x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
        Core.projectionFieldName = (Core.Name "statements")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

subgraphSubgraphId :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm (Maybe Dot.SubgraphId)
subgraphSubgraphId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
        Core.projectionFieldName = (Core.Name "subgraphId")})),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

subgraphWithStatements :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm [Dot.Stmt] -> Phantoms.TypedTerm Dot.Subgraph
subgraphWithStatements original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subgraphId"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
              Core.projectionFieldName = (Core.Name "subgraphId")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)}]}))

subgraphWithSubgraphId :: Phantoms.TypedTerm Dot.Subgraph -> Phantoms.TypedTerm (Maybe Dot.SubgraphId) -> Phantoms.TypedTerm Dot.Subgraph
subgraphWithSubgraphId original newVal =
    Phantoms.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "subgraphId"),
          Core.fieldTerm = (Phantoms.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "statements"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.graphviz.dot.Subgraph"),
              Core.projectionFieldName = (Core.Name "statements")})),
            Core.applicationArgument = (Phantoms.unTypedTerm original)}))}]}))

unAttrList :: Phantoms.TypedTerm Dot.AttrList -> Phantoms.TypedTerm [[Dot.EqualityPair]]
unAttrList x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphviz.dot.AttrList")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unId :: Phantoms.TypedTerm Dot.Id -> Phantoms.TypedTerm String
unId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphviz.dot.Id")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))

unSubgraphId :: Phantoms.TypedTerm Dot.SubgraphId -> Phantoms.TypedTerm (Maybe Dot.Id)
unSubgraphId x =
    Phantoms.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.graphviz.dot.SubgraphId")),
      Core.applicationArgument = (Phantoms.unTypedTerm x)}))
