-- Note: this is an automatically generated file. Do not edit.

-- | Term encoders for hydra.accessors

module Hydra.Encode.Accessors where

import qualified Hydra.Accessors as Accessors
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as Core_
import qualified Hydra.Lib.Lists as Lists
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

accessorEdge :: (Accessors.AccessorEdge -> Core.Term)
accessorEdge x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.accessors.AccessorEdge"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "source"),
      Core.fieldTerm = (accessorNode (Accessors.accessorEdgeSource x))},
    Core.Field {
      Core.fieldName = (Core.Name "path"),
      Core.fieldTerm = (accessorPath (Accessors.accessorEdgePath x))},
    Core.Field {
      Core.fieldName = (Core.Name "target"),
      Core.fieldTerm = (accessorNode (Accessors.accessorEdgeTarget x))}]}))

accessorGraph :: (Accessors.AccessorGraph -> Core.Term)
accessorGraph x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.accessors.AccessorGraph"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "nodes"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map accessorNode xs)) (Accessors.accessorGraphNodes x))},
    Core.Field {
      Core.fieldName = (Core.Name "edges"),
      Core.fieldTerm = ((\xs -> Core.TermList (Lists.map accessorEdge xs)) (Accessors.accessorGraphEdges x))}]}))

accessorNode :: (Accessors.AccessorNode -> Core.Term)
accessorNode x = (Core.TermRecord (Core.Record {
  Core.recordTypeName = (Core.Name "hydra.accessors.AccessorNode"),
  Core.recordFields = [
    Core.Field {
      Core.fieldName = (Core.Name "name"),
      Core.fieldTerm = (Core_.name (Accessors.accessorNodeName x))},
    Core.Field {
      Core.fieldName = (Core.Name "label"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Accessors.accessorNodeLabel x))},
    Core.Field {
      Core.fieldName = (Core.Name "id"),
      Core.fieldTerm = ((\x -> Core.TermLiteral (Core.LiteralString x)) (Accessors.accessorNodeId x))}]}))

accessorPath :: (Accessors.AccessorPath -> Core.Term)
accessorPath x = (Core.TermWrap (Core.WrappedTerm {
  Core.wrappedTermTypeName = (Core.Name "hydra.accessors.AccessorPath"),
  Core.wrappedTermBody = ((\xs -> Core.TermList (Lists.map termAccessor xs)) (Accessors.unAccessorPath x))}))

termAccessor :: (Accessors.TermAccessor -> Core.Term)
termAccessor x = case x of
  Accessors.TermAccessorAnnotatedBody -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "annotatedBody"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorApplicationFunction -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "applicationFunction"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorApplicationArgument -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "applicationArgument"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorLambdaBody -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "lambdaBody"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorUnionCasesDefault -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unionCasesDefault"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorUnionCasesBranch v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "unionCasesBranch"),
      Core.fieldTerm = (Core_.name v1)}}))
  Accessors.TermAccessorLetBody -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "letBody"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorLetBinding v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "letBinding"),
      Core.fieldTerm = (Core_.name v1)}}))
  Accessors.TermAccessorListElement v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "listElement"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Accessors.TermAccessorMapKey v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "mapKey"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Accessors.TermAccessorMapValue v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "mapValue"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Accessors.TermAccessorMaybeTerm -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "maybeTerm"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorProductTerm v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "productTerm"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Accessors.TermAccessorRecordField v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "recordField"),
      Core.fieldTerm = (Core_.name v1)}}))
  Accessors.TermAccessorSetElement v1 -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "setElement"),
      Core.fieldTerm = (Core.TermLiteral (Core.LiteralInteger (Core.IntegerValueInt32 v1)))}}))
  Accessors.TermAccessorSumTerm -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "sumTerm"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorTypeLambdaBody -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeLambdaBody"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorTypeApplicationTerm -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "typeApplicationTerm"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorInjectionTerm -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "injectionTerm"),
      Core.fieldTerm = Core.TermUnit}}))
  Accessors.TermAccessorWrappedTerm -> (Core.TermUnion (Core.Injection {
    Core.injectionTypeName = (Core.Name "hydra.accessors.TermAccessor"),
    Core.injectionField = Core.Field {
      Core.fieldName = (Core.Name "wrappedTerm"),
      Core.fieldTerm = Core.TermUnit}}))
