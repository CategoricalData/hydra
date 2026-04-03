-- Note: this is an automatically generated file. Do not edit.

-- | DSL functions for hydra.module

module Hydra.Dsl.Module where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Module as Module
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Map as M

definitionTerm :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm Module.Definition
definitionTerm x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

definitionType :: Phantoms.TTerm Module.TypeDefinition -> Phantoms.TTerm Module.Definition
definitionType x =
    Phantoms.TTerm (Core.TermUnion (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))

fileExtension :: Phantoms.TTerm String -> Phantoms.TTerm Module.FileExtension
fileExtension x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

library :: Phantoms.TTerm Module.Namespace -> Phantoms.TTerm String -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Module.Library
library namespace prefix primitives =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm prefix)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm primitives)}]}))

libraryNamespace :: Phantoms.TTerm Module.Library -> Phantoms.TTerm Module.Namespace
libraryNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

libraryPrefix :: Phantoms.TTerm Module.Library -> Phantoms.TTerm String
libraryPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "prefix")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

libraryPrimitives :: Phantoms.TTerm Module.Library -> Phantoms.TTerm [Graph.Primitive]
libraryPrimitives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "primitives")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

libraryWithNamespace :: Phantoms.TTerm Module.Library -> Phantoms.TTerm Module.Namespace -> Phantoms.TTerm Module.Library
libraryWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "primitives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

libraryWithPrefix :: Phantoms.TTerm Module.Library -> Phantoms.TTerm String -> Phantoms.TTerm Module.Library
libraryWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "primitives")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

libraryWithPrimitives :: Phantoms.TTerm Module.Library -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Module.Library
libraryWithPrimitives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "prefix")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

module_ :: Phantoms.TTerm Module.Namespace -> Phantoms.TTerm [Module.Definition] -> Phantoms.TTerm [Module.Namespace] -> Phantoms.TTerm [Module.Namespace] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Module.Module
module_ namespace definitions termDependencies typeDependencies description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm definitions)},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm termDependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm typeDependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))

moduleDefinitions :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Definition]
moduleDefinitions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "definitions")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleDescription :: Phantoms.TTerm Module.Module -> Phantoms.TTerm (Maybe String)
moduleDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "description")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleNamespace :: Phantoms.TTerm Module.Module -> Phantoms.TTerm Module.Namespace
moduleNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleTermDependencies :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Namespace]
moduleTermDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "termDependencies")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleTypeDependencies :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Namespace]
moduleTypeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "typeDependencies")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

moduleWithDefinitions :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Definition] -> Phantoms.TTerm Module.Module
moduleWithDefinitions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithDescription :: Phantoms.TTerm Module.Module -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Module.Module
moduleWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

moduleWithNamespace :: Phantoms.TTerm Module.Module -> Phantoms.TTerm Module.Namespace -> Phantoms.TTerm Module.Module
moduleWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithTermDependencies :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Namespace] -> Phantoms.TTerm Module.Module
moduleWithTermDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

moduleWithTypeDependencies :: Phantoms.TTerm Module.Module -> Phantoms.TTerm [Module.Namespace] -> Phantoms.TTerm Module.Module
moduleWithTypeDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namespace :: Phantoms.TTerm String -> Phantoms.TTerm Module.Namespace
namespace x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Namespace"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))

namespaces :: Phantoms.TTerm (Module.Namespace, n) -> Phantoms.TTerm (M.Map Module.Namespace n) -> Phantoms.TTerm (Module.Namespaces n)
namespaces focus mapping =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Namespaces"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Phantoms.unTTerm focus)},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Phantoms.unTTerm mapping)}]}))

namespacesFocus :: Phantoms.TTerm (Module.Namespaces n) -> Phantoms.TTerm (Module.Namespace, n)
namespacesFocus x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "focus")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namespacesMapping :: Phantoms.TTerm (Module.Namespaces n) -> Phantoms.TTerm (M.Map Module.Namespace n)
namespacesMapping x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "mapping")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

namespacesWithFocus :: Phantoms.TTerm (Module.Namespaces n) -> Phantoms.TTerm (Module.Namespace, n) -> Phantoms.TTerm (Module.Namespaces n)
namespacesWithFocus original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Namespaces"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
              Core.projectionField = (Core.Name "mapping")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

namespacesWithMapping :: Phantoms.TTerm (Module.Namespaces n) -> Phantoms.TTerm (M.Map Module.Namespace n) -> Phantoms.TTerm (Module.Namespaces n)
namespacesWithMapping original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Namespaces"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
              Core.projectionField = (Core.Name "focus")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedName :: Phantoms.TTerm (Maybe Module.Namespace) -> Phantoms.TTerm String -> Phantoms.TTerm Module.QualifiedName
qualifiedName namespace local =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm local)}]}))

qualifiedNameLocal :: Phantoms.TTerm Module.QualifiedName -> Phantoms.TTerm String
qualifiedNameLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "local")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameNamespace :: Phantoms.TTerm Module.QualifiedName -> Phantoms.TTerm (Maybe Module.Namespace)
qualifiedNameNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "namespace")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

qualifiedNameWithLocal :: Phantoms.TTerm Module.QualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm Module.QualifiedName
qualifiedNameWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionField = (Core.Name "namespace")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

qualifiedNameWithNamespace :: Phantoms.TTerm Module.QualifiedName -> Phantoms.TTerm (Maybe Module.Namespace) -> Phantoms.TTerm Module.QualifiedName
qualifiedNameWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionField = (Core.Name "local")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Module.TermDefinition
termDefinition name term type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

termDefinitionName :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm Core.Name
termDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termDefinitionTerm :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm Core.Term
termDefinitionTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "term")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termDefinitionType :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme)
termDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

termDefinitionWithName :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Module.TermDefinition
termDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termDefinitionWithTerm :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Module.TermDefinition
termDefinitionWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

termDefinitionWithType :: Phantoms.TTerm Module.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Module.TermDefinition
termDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "term")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

typeDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Module.TypeDefinition
typeDefinition name type_ =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm type_)}]}))

typeDefinitionName :: Phantoms.TTerm Module.TypeDefinition -> Phantoms.TTerm Core.Name
typeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "name")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefinitionType :: Phantoms.TTerm Module.TypeDefinition -> Phantoms.TTerm Core.TypeScheme
typeDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "type")})))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

typeDefinitionWithName :: Phantoms.TTerm Module.TypeDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Module.TypeDefinition
typeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionField = (Core.Name "type")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))

typeDefinitionWithType :: Phantoms.TTerm Module.TypeDefinition -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Module.TypeDefinition
typeDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationRecord (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionField = (Core.Name "name")})))),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))

unFileExtension :: Phantoms.TTerm Module.FileExtension -> Phantoms.TTerm String
unFileExtension x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.packaging.FileExtension")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))

unNamespace :: Phantoms.TTerm Module.Namespace -> Phantoms.TTerm String
unNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermFunction (Core.FunctionElimination (Core.EliminationWrap (Core.Name "hydra.packaging.Namespace")))),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
