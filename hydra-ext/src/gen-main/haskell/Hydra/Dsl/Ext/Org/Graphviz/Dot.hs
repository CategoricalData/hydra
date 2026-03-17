-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.ext.org.graphviz.dot

module Hydra.Dsl.Ext.Org.Graphviz.Dot where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Org.Graphviz.Dot as Dot
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

id :: (Phantoms.TTerm String -> Phantoms.TTerm Dot.Id)
id x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Id"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unId :: (Phantoms.TTerm Dot.Id -> Phantoms.TTerm String)
unId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.graphviz.dot.Id")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graph :: (Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Dot.Id) -> Phantoms.TTerm [Dot.Stmt] -> Phantoms.TTerm Dot.Graph)
graph strict directed id statements = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "strict"),
      Core.fieldTerm = (Phantoms.unTTerm strict)},
    Core.Field {
      Core.fieldName = (Core.Name "directed"),
      Core.fieldTerm = (Phantoms.unTTerm directed)},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm id)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm statements)}]})))

graphStrict :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm Bool)
graphStrict x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
    Core.projectionField = (Core.Name "strict")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphDirected :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm Bool)
graphDirected x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
    Core.projectionField = (Core.Name "directed")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphId :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm (Maybe Dot.Id))
graphId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
    Core.projectionField = (Core.Name "id")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphStatements :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm [Dot.Stmt])
graphStatements x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
    Core.projectionField = (Core.Name "statements")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

graphWithStrict :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm Bool -> Phantoms.TTerm Dot.Graph)
graphWithStrict original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "strict"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "directed"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "directed")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithDirected :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm Bool -> Phantoms.TTerm Dot.Graph)
graphWithDirected original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "strict"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "strict")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "directed"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithId :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm (Maybe Dot.Id) -> Phantoms.TTerm Dot.Graph)
graphWithId original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "strict"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "strict")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "directed"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "directed")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

graphWithStatements :: (Phantoms.TTerm Dot.Graph -> Phantoms.TTerm [Dot.Stmt] -> Phantoms.TTerm Dot.Graph)
graphWithStatements original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "strict"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "strict")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "directed"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "directed")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Graph"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

stmtNode :: (Phantoms.TTerm Dot.NodeStmt -> Phantoms.TTerm Dot.Stmt)
stmtNode x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Stmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "node"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stmtEdge :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm Dot.Stmt)
stmtEdge x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Stmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edge"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stmtAttr :: (Phantoms.TTerm Dot.AttrStmt -> Phantoms.TTerm Dot.Stmt)
stmtAttr x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Stmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "attr"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stmtEquals :: (Phantoms.TTerm Dot.EqualityPair -> Phantoms.TTerm Dot.Stmt)
stmtEquals x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Stmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "equals"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

stmtSubgraph :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm Dot.Stmt)
stmtSubgraph x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Stmt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subgraph"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

equalityPair :: (Phantoms.TTerm Dot.Id -> Phantoms.TTerm Dot.Id -> Phantoms.TTerm Dot.EqualityPair)
equalityPair left right = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm left)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm right)}]})))

equalityPairLeft :: (Phantoms.TTerm Dot.EqualityPair -> Phantoms.TTerm Dot.Id)
equalityPairLeft x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
    Core.projectionField = (Core.Name "left")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

equalityPairRight :: (Phantoms.TTerm Dot.EqualityPair -> Phantoms.TTerm Dot.Id)
equalityPairRight x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
    Core.projectionField = (Core.Name "right")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

equalityPairWithLeft :: (Phantoms.TTerm Dot.EqualityPair -> Phantoms.TTerm Dot.Id -> Phantoms.TTerm Dot.EqualityPair)
equalityPairWithLeft original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

equalityPairWithRight :: (Phantoms.TTerm Dot.EqualityPair -> Phantoms.TTerm Dot.Id -> Phantoms.TTerm Dot.EqualityPair)
equalityPairWithRight original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EqualityPair"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

attrStmt :: (Phantoms.TTerm Dot.AttrType -> Phantoms.TTerm Dot.AttrList -> Phantoms.TTerm Dot.AttrStmt)
attrStmt type_ attributes = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm type_)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm attributes)}]})))

attrStmtType :: (Phantoms.TTerm Dot.AttrStmt -> Phantoms.TTerm Dot.AttrType)
attrStmtType x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
    Core.projectionField = (Core.Name "type")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

attrStmtAttributes :: (Phantoms.TTerm Dot.AttrStmt -> Phantoms.TTerm Dot.AttrList)
attrStmtAttributes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
    Core.projectionField = (Core.Name "attributes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

attrStmtWithType :: (Phantoms.TTerm Dot.AttrStmt -> Phantoms.TTerm Dot.AttrType -> Phantoms.TTerm Dot.AttrStmt)
attrStmtWithType original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
          Core.projectionField = (Core.Name "attributes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

attrStmtWithAttributes :: (Phantoms.TTerm Dot.AttrStmt -> Phantoms.TTerm Dot.AttrList -> Phantoms.TTerm Dot.AttrStmt)
attrStmtWithAttributes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "type"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrStmt"),
          Core.projectionField = (Core.Name "type")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

attrTypeGraph :: (Phantoms.TTerm Dot.AttrType)
attrTypeGraph = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "graph"),
    Core.fieldTerm = Core.TermUnit}})))

attrTypeNode :: (Phantoms.TTerm Dot.AttrType)
attrTypeNode = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "node"),
    Core.fieldTerm = Core.TermUnit}})))

attrTypeEdge :: (Phantoms.TTerm Dot.AttrType)
attrTypeEdge = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrType"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "edge"),
    Core.fieldTerm = Core.TermUnit}})))

attrList :: (Phantoms.TTerm [[Dot.EqualityPair]] -> Phantoms.TTerm Dot.AttrList)
attrList x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.graphviz.dot.AttrList"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unAttrList :: (Phantoms.TTerm Dot.AttrList -> Phantoms.TTerm [[Dot.EqualityPair]])
unAttrList x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.graphviz.dot.AttrList")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeStmt :: (Phantoms.TTerm Dot.NodeOrSubgraph -> Phantoms.TTerm [Dot.NodeOrSubgraph] -> Phantoms.TTerm (Maybe Dot.AttrList) -> Phantoms.TTerm Dot.EdgeStmt)
edgeStmt left right attributes = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm left)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm right)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm attributes)}]})))

edgeStmtLeft :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm Dot.NodeOrSubgraph)
edgeStmtLeft x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
    Core.projectionField = (Core.Name "left")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeStmtRight :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm [Dot.NodeOrSubgraph])
edgeStmtRight x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
    Core.projectionField = (Core.Name "right")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeStmtAttributes :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm (Maybe Dot.AttrList))
edgeStmtAttributes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
    Core.projectionField = (Core.Name "attributes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

edgeStmtWithLeft :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm Dot.NodeOrSubgraph -> Phantoms.TTerm Dot.EdgeStmt)
edgeStmtWithLeft original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "attributes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

edgeStmtWithRight :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm [Dot.NodeOrSubgraph] -> Phantoms.TTerm Dot.EdgeStmt)
edgeStmtWithRight original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "attributes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

edgeStmtWithAttributes :: (Phantoms.TTerm Dot.EdgeStmt -> Phantoms.TTerm (Maybe Dot.AttrList) -> Phantoms.TTerm Dot.EdgeStmt)
edgeStmtWithAttributes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "left"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "left")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "right"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.EdgeStmt"),
          Core.projectionField = (Core.Name "right")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

nodeOrSubgraphNode :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm Dot.NodeOrSubgraph)
nodeOrSubgraphNode x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeOrSubgraph"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "node"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeOrSubgraphSubgraph :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm Dot.NodeOrSubgraph)
nodeOrSubgraphSubgraph x = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeOrSubgraph"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "subgraph"),
    Core.fieldTerm = (Phantoms.unTTerm x)}})))

nodeStmt :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm (Maybe Dot.AttrList) -> Phantoms.TTerm Dot.NodeStmt)
nodeStmt id attributes = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm id)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm attributes)}]})))

nodeStmtId :: (Phantoms.TTerm Dot.NodeStmt -> Phantoms.TTerm Dot.NodeId)
nodeStmtId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
    Core.projectionField = (Core.Name "id")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

nodeStmtAttributes :: (Phantoms.TTerm Dot.NodeStmt -> Phantoms.TTerm (Maybe Dot.AttrList))
nodeStmtAttributes x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
    Core.projectionField = (Core.Name "attributes")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

nodeStmtWithId :: (Phantoms.TTerm Dot.NodeStmt -> Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm Dot.NodeStmt)
nodeStmtWithId original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
          Core.projectionField = (Core.Name "attributes")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

nodeStmtWithAttributes :: (Phantoms.TTerm Dot.NodeStmt -> Phantoms.TTerm (Maybe Dot.AttrList) -> Phantoms.TTerm Dot.NodeStmt)
nodeStmtWithAttributes original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeStmt"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "attributes"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

nodeId :: (Phantoms.TTerm Dot.Id -> Phantoms.TTerm (Maybe Dot.Port) -> Phantoms.TTerm Dot.NodeId)
nodeId id port = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm id)},
    Core.Field {
      Core.fieldName = (Core.Name "port"),
      Core.fieldTerm = (Phantoms.unTTerm port)}]})))

nodeIdId :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm Dot.Id)
nodeIdId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
    Core.projectionField = (Core.Name "id")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

nodeIdPort :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm (Maybe Dot.Port))
nodeIdPort x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
    Core.projectionField = (Core.Name "port")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

nodeIdWithId :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm Dot.Id -> Phantoms.TTerm Dot.NodeId)
nodeIdWithId original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "port"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
          Core.projectionField = (Core.Name "port")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

nodeIdWithPort :: (Phantoms.TTerm Dot.NodeId -> Phantoms.TTerm (Maybe Dot.Port) -> Phantoms.TTerm Dot.NodeId)
nodeIdWithPort original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.NodeId"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "port"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

port :: (Phantoms.TTerm (Maybe Dot.Id) -> Phantoms.TTerm (Maybe Dot.CompassPt) -> Phantoms.TTerm Dot.Port)
port id position = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm id)},
    Core.Field {
      Core.fieldName = (Core.Name "position"),
      Core.fieldTerm = (Phantoms.unTTerm position)}]})))

portId :: (Phantoms.TTerm Dot.Port -> Phantoms.TTerm (Maybe Dot.Id))
portId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
    Core.projectionField = (Core.Name "id")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

portPosition :: (Phantoms.TTerm Dot.Port -> Phantoms.TTerm (Maybe Dot.CompassPt))
portPosition x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
    Core.projectionField = (Core.Name "position")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

portWithId :: (Phantoms.TTerm Dot.Port -> Phantoms.TTerm (Maybe Dot.Id) -> Phantoms.TTerm Dot.Port)
portWithId original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "position"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
          Core.projectionField = (Core.Name "position")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

portWithPosition :: (Phantoms.TTerm Dot.Port -> Phantoms.TTerm (Maybe Dot.CompassPt) -> Phantoms.TTerm Dot.Port)
portWithPosition original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Port"),
          Core.projectionField = (Core.Name "id")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "position"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

subgraph :: (Phantoms.TTerm (Maybe Dot.SubgraphId) -> Phantoms.TTerm [Dot.Stmt] -> Phantoms.TTerm Dot.Subgraph)
subgraph subgraphId statements = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subgraphId"),
      Core.fieldTerm = (Phantoms.unTTerm subgraphId)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm statements)}]})))

subgraphSubgraphId :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm (Maybe Dot.SubgraphId))
subgraphSubgraphId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
    Core.projectionField = (Core.Name "subgraphId")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

subgraphStatements :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm [Dot.Stmt])
subgraphStatements x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
    Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
    Core.projectionField = (Core.Name "statements")})))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

subgraphWithSubgraphId :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm (Maybe Dot.SubgraphId) -> Phantoms.TTerm Dot.Subgraph)
subgraphWithSubgraphId original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subgraphId"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
          Core.projectionField = (Core.Name "statements")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))}]})))

subgraphWithStatements :: (Phantoms.TTerm Dot.Subgraph -> Phantoms.TTerm [Dot.Stmt] -> Phantoms.TTerm Dot.Subgraph)
subgraphWithStatements original newVal = (Phantoms.TTerm (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "subgraphId"),
      Core.fieldTerm = (Core.TermApplication (Core.Application {
        Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
          Core.projectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.Subgraph"),
          Core.projectionField = (Core.Name "subgraphId")})))),
        Core.applicationArgument = (Phantoms.unTTerm original)}))},
    Core.Field {
      Core.fieldName = (Core.Name "statements"),
      Core.fieldTerm = (Phantoms.unTTerm newVal)}]})))

subgraphId :: (Phantoms.TTerm (Maybe Dot.Id) -> Phantoms.TTerm Dot.SubgraphId)
subgraphId x = (Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.ext.org.graphviz.dot.SubgraphId"),
  Core.wrappedTermBody = (Phantoms.unTTerm x)})))

unSubgraphId :: (Phantoms.TTerm Dot.SubgraphId -> Phantoms.TTerm (Maybe Dot.Id))
unSubgraphId x = (Phantoms.TTerm (Core.TermApplication (Core.Application {
  Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.ext.org.graphviz.dot.SubgraphId")))),
  Core.applicationArgument = (Phantoms.unTTerm x)})))

compassPtN :: (Phantoms.TTerm Dot.CompassPt)
compassPtN = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "n"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtNe :: (Phantoms.TTerm Dot.CompassPt)
compassPtNe = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "ne"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtE :: (Phantoms.TTerm Dot.CompassPt)
compassPtE = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "e"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtSe :: (Phantoms.TTerm Dot.CompassPt)
compassPtSe = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "se"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtS :: (Phantoms.TTerm Dot.CompassPt)
compassPtS = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "s"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtSw :: (Phantoms.TTerm Dot.CompassPt)
compassPtSw = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "sw"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtW :: (Phantoms.TTerm Dot.CompassPt)
compassPtW = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "w"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtNw :: (Phantoms.TTerm Dot.CompassPt)
compassPtNw = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "nw"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtC :: (Phantoms.TTerm Dot.CompassPt)
compassPtC = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "c"),
    Core.fieldTerm = Core.TermUnit}})))

compassPtNone :: (Phantoms.TTerm Dot.CompassPt)
compassPtNone = (Phantoms.TTerm (Core.TermUnion (Core.Injection {
  Core.injectionTypeName = (Core.Name "hydra.ext.org.graphviz.dot.CompassPt"),
  Core.injectionField = Core.Field {
    Core.fieldName = (Core.Name "none"),
    Core.fieldTerm = Core.TermUnit}})))
