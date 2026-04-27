-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.packaging

module Hydra.Dsl.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
definitionTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Packaging.Definition
definitionTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
definitionType :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Packaging.Definition
definitionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
fileExtension :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.FileExtension
fileExtension x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
library :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm String -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Packaging.Library
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
libraryNamespace :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm Packaging.Namespace
libraryNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
libraryPrefix :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm String
libraryPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
libraryPrimitives :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm [Graph.Primitive]
libraryPrimitives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "primitives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
libraryWithNamespace :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Packaging.Library
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
libraryWithPrefix :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.Library
libraryWithPrefix original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "primitives")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
libraryWithPrimitives :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm [Graph.Primitive] -> Phantoms.TTerm Packaging.Library
libraryWithPrimitives original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Library"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "prefix"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
              Core.projectionField = (Core.Name "prefix")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "primitives"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
module_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm [Packaging.Definition] -> Phantoms.TTerm Packaging.Module
module_ description namespace termDependencies typeDependencies definitions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm namespace)},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm termDependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm typeDependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm definitions)}]}))
moduleDefinitions :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Definition]
moduleDefinitions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "definitions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
moduleDescription :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm (Maybe String)
moduleDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
moduleNamespace :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm Packaging.Namespace
moduleNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
moduleTermDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace]
moduleTermDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "termDependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
moduleTypeDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace]
moduleTypeDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "typeDependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
moduleWithDefinitions :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Definition] -> Phantoms.TTerm Packaging.Module
moduleWithDefinitions original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
moduleWithDescription :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Module
moduleWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
moduleWithNamespace :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm Packaging.Module
moduleWithNamespace original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
moduleWithTermDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm Packaging.Module
moduleWithTermDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "typeDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
moduleWithTypeDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm Packaging.Module
moduleWithTypeDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "termDependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
namespace :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.Namespace
namespace x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Namespace"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
namespaces :: Phantoms.TTerm (Packaging.Namespace, n) -> Phantoms.TTerm (M.Map Packaging.Namespace n) -> Phantoms.TTerm (Packaging.Namespaces n)
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
namespacesFocus :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (Packaging.Namespace, n)
namespacesFocus x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "focus")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
namespacesMapping :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (M.Map Packaging.Namespace n)
namespacesMapping x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "mapping")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
namespacesWithFocus :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (Packaging.Namespace, n) -> Phantoms.TTerm (Packaging.Namespaces n)
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
              Core.projectionField = (Core.Name "mapping")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
namespacesWithMapping :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (M.Map Packaging.Namespace n) -> Phantoms.TTerm (Packaging.Namespaces n)
namespacesWithMapping original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Namespaces"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
              Core.projectionField = (Core.Name "focus")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
package :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm [Packaging.Module] -> Phantoms.TTerm [Packaging.PackageName] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Package
package name modules dependencies description =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm modules)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm dependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)}]}))
packageDependencies :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.PackageName]
packageDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
packageDescription :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm (Maybe String)
packageDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
packageModules :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.Module]
packageModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
packageName :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm Packaging.PackageName
packageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
packageName_ :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.PackageName
packageName_ x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
packageWithDependencies :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.PackageName] -> Phantoms.TTerm Packaging.Package
packageWithDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
packageWithDescription :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Package
packageWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
packageWithModules :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.Module] -> Phantoms.TTerm Packaging.Package
packageWithModules original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
packageWithName :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm Packaging.Package
packageWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionField = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
qualifiedName :: Phantoms.TTerm (Maybe Packaging.Namespace) -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.QualifiedName
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
qualifiedNameLocal :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm String
qualifiedNameLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "local")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
qualifiedNameNamespace :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm (Maybe Packaging.Namespace)
qualifiedNameNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
qualifiedNameWithLocal :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.QualifiedName
qualifiedNameWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionField = (Core.Name "namespace")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
qualifiedNameWithNamespace :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm (Maybe Packaging.Namespace) -> Phantoms.TTerm Packaging.QualifiedName
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionField = (Core.Name "local")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
termDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Packaging.TermDefinition
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
termDefinitionName :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Name
termDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
termDefinitionTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Term
termDefinitionTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
termDefinitionType :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme)
termDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
termDefinitionWithName :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.TermDefinition
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
termDefinitionWithTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Term -> Phantoms.TTerm Packaging.TermDefinition
termDefinitionWithTerm original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
termDefinitionWithType :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Packaging.TermDefinition
termDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
typeDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Packaging.TypeDefinition
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
typeDefinitionName :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.Name
typeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
typeDefinitionType :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.TypeScheme
typeDefinitionType x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "type")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
typeDefinitionWithName :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.TypeDefinition
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
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionField = (Core.Name "type")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
typeDefinitionWithType :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Packaging.TypeDefinition
typeDefinitionWithType original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionField = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
unFileExtension :: Phantoms.TTerm Packaging.FileExtension -> Phantoms.TTerm String
unFileExtension x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.FileExtension")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
unNamespace :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm String
unNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.Namespace")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
unPackageName :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm String
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.PackageName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
