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
-- | DSL injection for the term variant of hydra.packaging.Definition
definitionTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Packaging.Definition
definitionTerm x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL injection for the type variant of hydra.packaging.Definition
definitionType :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Packaging.Definition
definitionType x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
-- | DSL constructor for the hydra.packaging.FileExtension wrapper
fileExtension :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.FileExtension
fileExtension x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.packaging.Library
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
-- | DSL accessor for the namespace field of hydra.packaging.Library
libraryNamespace :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm Packaging.Namespace
libraryNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the prefix field of hydra.packaging.Library
libraryPrefix :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm String
libraryPrefix x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "prefix")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the primitives field of hydra.packaging.Library
libraryPrimitives :: Phantoms.TTerm Packaging.Library -> Phantoms.TTerm [Graph.Primitive]
libraryPrimitives x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Library"),
        Core.projectionField = (Core.Name "primitives")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the namespace field of hydra.packaging.Library
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
-- | DSL updater for the prefix field of hydra.packaging.Library
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
-- | DSL updater for the primitives field of hydra.packaging.Library
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
-- | DSL constructor for hydra.packaging.Module
module_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm [Packaging.Definition] -> Phantoms.TTerm Packaging.Module
module_ description namespace dependencies definitions =
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
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm dependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm definitions)}]}))
-- | DSL accessor for the definitions field of hydra.packaging.Module
moduleDefinitions :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Definition]
moduleDefinitions x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "definitions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dependencies field of hydra.packaging.Module
moduleDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace]
moduleDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.packaging.Module
moduleDescription :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm (Maybe String)
moduleDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.packaging.Module
moduleNamespace :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm Packaging.Namespace
moduleNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the definitions field of hydra.packaging.Module
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
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the dependencies field of hydra.packaging.Module
moduleWithDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.Namespace] -> Phantoms.TTerm Packaging.Module
moduleWithDependencies original newVal =
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
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the description field of hydra.packaging.Module
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
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the namespace field of hydra.packaging.Module
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
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionField = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for the hydra.packaging.Namespace wrapper
namespace :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.Namespace
namespace x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Namespace"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.packaging.Namespaces
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
-- | DSL accessor for the focus field of hydra.packaging.Namespaces
namespacesFocus :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (Packaging.Namespace, n)
namespacesFocus x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "focus")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the mapping field of hydra.packaging.Namespaces
namespacesMapping :: Phantoms.TTerm (Packaging.Namespaces n) -> Phantoms.TTerm (M.Map Packaging.Namespace n)
namespacesMapping x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Namespaces"),
        Core.projectionField = (Core.Name "mapping")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the focus field of hydra.packaging.Namespaces
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
-- | DSL updater for the mapping field of hydra.packaging.Namespaces
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
-- | DSL constructor for hydra.packaging.Package
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
-- | DSL accessor for the dependencies field of hydra.packaging.Package
packageDependencies :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.PackageName]
packageDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.packaging.Package
packageDescription :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm (Maybe String)
packageDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modules field of hydra.packaging.Package
packageModules :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.Module]
packageModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.Package
packageName :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm Packaging.PackageName
packageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.packaging.PackageName wrapper
packageName2 :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.PackageName
packageName2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL updater for the dependencies field of hydra.packaging.Package
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
-- | DSL updater for the description field of hydra.packaging.Package
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
-- | DSL updater for the modules field of hydra.packaging.Package
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
-- | DSL updater for the name field of hydra.packaging.Package
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
-- | DSL constructor for hydra.packaging.QualifiedName
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
-- | DSL accessor for the local field of hydra.packaging.QualifiedName
qualifiedNameLocal :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm String
qualifiedNameLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "local")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the namespace field of hydra.packaging.QualifiedName
qualifiedNameNamespace :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm (Maybe Packaging.Namespace)
qualifiedNameNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionField = (Core.Name "namespace")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the local field of hydra.packaging.QualifiedName
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
-- | DSL updater for the namespace field of hydra.packaging.QualifiedName
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
-- | DSL constructor for hydra.packaging.TermDefinition
termDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Packaging.TermDefinition
termDefinition name term typeScheme =
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm typeScheme)}]}))
-- | DSL accessor for the name field of hydra.packaging.TermDefinition
termDefinitionName :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Name
termDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the term field of hydra.packaging.TermDefinition
termDefinitionTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Term
termDefinitionTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "term")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.packaging.TermDefinition
termDefinitionTypeScheme :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme)
termDefinitionTypeScheme x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionField = (Core.Name "typeScheme")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.packaging.TermDefinition
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the term field of hydra.packaging.TermDefinition
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionField = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeScheme field of hydra.packaging.TermDefinition
termDefinitionWithTypeScheme :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Core.TypeScheme) -> Phantoms.TTerm Packaging.TermDefinition
termDefinitionWithTypeScheme original newVal =
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL constructor for hydra.packaging.TypeDefinition
typeDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Packaging.TypeDefinition
typeDefinition name typeScheme =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm typeScheme)}]}))
-- | DSL accessor for the name field of hydra.packaging.TypeDefinition
typeDefinitionName :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.Name
typeDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.packaging.TypeDefinition
typeDefinitionTypeScheme :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.TypeScheme
typeDefinitionTypeScheme x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionField = (Core.Name "typeScheme")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.packaging.TypeDefinition
typeDefinitionWithName :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.TypeDefinition
typeDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionField = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the typeScheme field of hydra.packaging.TypeDefinition
typeDefinitionWithTypeScheme :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.TypeScheme -> Phantoms.TTerm Packaging.TypeDefinition
typeDefinitionWithTypeScheme original newVal =
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the body of hydra.packaging.FileExtension
unFileExtension :: Phantoms.TTerm Packaging.FileExtension -> Phantoms.TTerm String
unFileExtension x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.FileExtension")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.packaging.Namespace
unNamespace :: Phantoms.TTerm Packaging.Namespace -> Phantoms.TTerm String
unNamespace x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.Namespace")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.packaging.PackageName
unPackageName :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm String
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.PackageName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
