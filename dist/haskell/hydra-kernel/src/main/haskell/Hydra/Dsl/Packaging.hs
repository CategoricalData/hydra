-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.packaging

module Hydra.Dsl.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Phantoms as Phantoms
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the primitive variant of hydra.packaging.Definition
definitionPrimitive :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Packaging.Definition
definitionPrimitive x =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Phantoms.unTTerm x)}}))
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
-- | DSL constructor for hydra.packaging.Module
module_ :: Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm [Packaging.ModuleDependency] -> Phantoms.TTerm [Packaging.Definition] -> Phantoms.TTerm Packaging.Module
module_ description name dependencies definitions =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
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
        Core.projectionFieldName = (Core.Name "definitions")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the dependencies field of hydra.packaging.Module
moduleDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.ModuleDependency]
moduleDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.packaging.ModuleDependency
moduleDependency :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm (Maybe Packaging.PackageName) -> Phantoms.TTerm Packaging.ModuleDependency
moduleDependency module_ package =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm package)}]}))
-- | DSL accessor for the module field of hydra.packaging.ModuleDependency
moduleDependencyModule :: Phantoms.TTerm Packaging.ModuleDependency -> Phantoms.TTerm Packaging.ModuleName
moduleDependencyModule x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the package field of hydra.packaging.ModuleDependency
moduleDependencyPackage :: Phantoms.TTerm Packaging.ModuleDependency -> Phantoms.TTerm (Maybe Packaging.PackageName)
moduleDependencyPackage x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the module field of hydra.packaging.ModuleDependency
moduleDependencyWithModule :: Phantoms.TTerm Packaging.ModuleDependency -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Packaging.ModuleDependency
moduleDependencyWithModule original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the package field of hydra.packaging.ModuleDependency
moduleDependencyWithPackage :: Phantoms.TTerm Packaging.ModuleDependency -> Phantoms.TTerm (Maybe Packaging.PackageName) -> Phantoms.TTerm Packaging.ModuleDependency
moduleDependencyWithPackage original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the description field of hydra.packaging.Module
moduleDescription :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm (Maybe String)
moduleDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.Module
moduleName :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm Packaging.ModuleName
moduleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.packaging.ModuleName wrapper
moduleName2 :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.ModuleName
moduleName2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.ModuleName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
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
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the dependencies field of hydra.packaging.Module
moduleWithDependencies :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm [Packaging.ModuleDependency] -> Phantoms.TTerm Packaging.Module
moduleWithDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
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
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.Module
moduleWithName :: Phantoms.TTerm Packaging.Module -> Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm Packaging.Module
moduleWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.Package
package :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm [Packaging.Module] -> Phantoms.TTerm [Packaging.PackageDependency] -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.Package
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
packageDependencies :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.PackageDependency]
packageDependencies x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for hydra.packaging.PackageDependency
packageDependency :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm Packaging.PackageVersionSpecifier -> Phantoms.TTerm Packaging.PackageDependency
packageDependency name version =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm version)}]}))
-- | DSL accessor for the name field of hydra.packaging.PackageDependency
packageDependencyName :: Phantoms.TTerm Packaging.PackageDependency -> Phantoms.TTerm Packaging.PackageName
packageDependencyName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the version field of hydra.packaging.PackageDependency
packageDependencyVersion :: Phantoms.TTerm Packaging.PackageDependency -> Phantoms.TTerm Packaging.PackageVersionSpecifier
packageDependencyVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
        Core.projectionFieldName = (Core.Name "version")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the name field of hydra.packaging.PackageDependency
packageDependencyWithName :: Phantoms.TTerm Packaging.PackageDependency -> Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm Packaging.PackageDependency
packageDependencyWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the version field of hydra.packaging.PackageDependency
packageDependencyWithVersion :: Phantoms.TTerm Packaging.PackageDependency -> Phantoms.TTerm Packaging.PackageVersionSpecifier -> Phantoms.TTerm Packaging.PackageDependency
packageDependencyWithVersion original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL accessor for the description field of hydra.packaging.Package
packageDescription :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm (Maybe String)
packageDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the modules field of hydra.packaging.Package
packageModules :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.Module]
packageModules x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.Package
packageName :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm Packaging.PackageName
packageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.packaging.PackageName wrapper
packageName2 :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.PackageName
packageName2 x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
-- | DSL injection for the any variant of hydra.packaging.PackageVersionSpecifier
packageVersionSpecifierAny :: Phantoms.TTerm Packaging.PackageVersionSpecifier
packageVersionSpecifierAny =
    Phantoms.TTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.PackageVersionSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the dependencies field of hydra.packaging.Package
packageWithDependencies :: Phantoms.TTerm Packaging.Package -> Phantoms.TTerm [Packaging.PackageDependency] -> Phantoms.TTerm Packaging.Package
packageWithDependencies original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "description")})),
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "description")})),
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
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.PrimitiveDefinition
primitiveDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Typing.TermSignature -> Phantoms.TTerm String -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Bool -> Phantoms.TTerm Bool -> Phantoms.TTerm (Maybe Packaging.Version) -> Phantoms.TTerm (Maybe Packaging.Version) -> Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinition name signature description comments seeAlso isPure isTotal availableSince deprecatedSince defaultImplementation =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm comments)},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Phantoms.unTTerm seeAlso)},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Phantoms.unTTerm isPure)},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Phantoms.unTTerm isTotal)},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Phantoms.unTTerm availableSince)},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Phantoms.unTTerm deprecatedSince)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Phantoms.unTTerm defaultImplementation)}]}))
-- | DSL accessor for the availableSince field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionAvailableSince :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Packaging.Version)
primitiveDefinitionAvailableSince x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "availableSince")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the comments field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionComments :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe String)
primitiveDefinitionComments x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the defaultImplementation field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionDefaultImplementation :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Core.Term)
primitiveDefinitionDefaultImplementation x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "defaultImplementation")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the deprecatedSince field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionDeprecatedSince :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Packaging.Version)
primitiveDefinitionDeprecatedSince x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "deprecatedSince")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the description field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionDescription :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm String
primitiveDefinitionDescription x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the isPure field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionIsPure :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Bool
primitiveDefinitionIsPure x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "isPure")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the isTotal field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionIsTotal :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Bool
primitiveDefinitionIsTotal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "isTotal")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionName :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Core.Name
primitiveDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the seeAlso field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionSeeAlso :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm [Core.Name]
primitiveDefinitionSeeAlso x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "seeAlso")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionSignature :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Typing.TermSignature
primitiveDefinitionSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the availableSince field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithAvailableSince :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Packaging.Version) -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithAvailableSince original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the comments field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithComments :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe String) -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithComments original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the defaultImplementation field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithDefaultImplementation :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Core.Term) -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithDefaultImplementation original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the deprecatedSince field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithDeprecatedSince :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm (Maybe Packaging.Version) -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithDeprecatedSince original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the description field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithDescription :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithDescription original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the isPure field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithIsPure :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Bool -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithIsPure original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the isTotal field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithIsTotal :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Bool -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithIsTotal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithName :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Core.Name -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the seeAlso field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithSeeAlso :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm [Core.Name] -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithSeeAlso original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithSignature :: Phantoms.TTerm Packaging.PrimitiveDefinition -> Phantoms.TTerm Typing.TermSignature -> Phantoms.TTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.QualifiedName
qualifiedName :: Phantoms.TTerm (Maybe Packaging.ModuleName) -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.QualifiedName
qualifiedName moduleName local =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm local)}]}))
-- | DSL accessor for the local field of hydra.packaging.QualifiedName
qualifiedNameLocal :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm String
qualifiedNameLocal x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the moduleName field of hydra.packaging.QualifiedName
qualifiedNameModuleName :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm (Maybe Packaging.ModuleName)
qualifiedNameModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL updater for the local field of hydra.packaging.QualifiedName
qualifiedNameWithLocal :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm String -> Phantoms.TTerm Packaging.QualifiedName
qualifiedNameWithLocal original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.packaging.QualifiedName
qualifiedNameWithModuleName :: Phantoms.TTerm Packaging.QualifiedName -> Phantoms.TTerm (Maybe Packaging.ModuleName) -> Phantoms.TTerm Packaging.QualifiedName
qualifiedNameWithModuleName original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.TermDefinition
termDefinition :: Phantoms.TTerm Core.Name -> Phantoms.TTerm Core.Term -> Phantoms.TTerm (Maybe Typing.TermSignature) -> Phantoms.TTerm Packaging.TermDefinition
termDefinition name term signature =
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
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm signature)}]}))
-- | DSL accessor for the name field of hydra.packaging.TermDefinition
termDefinitionName :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Name
termDefinitionName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the signature field of hydra.packaging.TermDefinition
termDefinitionSignature :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Typing.TermSignature)
termDefinitionSignature x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the term field of hydra.packaging.TermDefinition
termDefinitionTerm :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm Core.Term
termDefinitionTerm x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "term")})),
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
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.packaging.TermDefinition
termDefinitionWithSignature :: Phantoms.TTerm Packaging.TermDefinition -> Phantoms.TTerm (Maybe Typing.TermSignature) -> Phantoms.TTerm Packaging.TermDefinition
termDefinitionWithSignature original newVal =
    Phantoms.TTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)}]}))
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
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Phantoms.unTTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Phantoms.unTTerm original)}))}]}))
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
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.packaging.TypeDefinition
typeDefinitionTypeScheme :: Phantoms.TTerm Packaging.TypeDefinition -> Phantoms.TTerm Core.TypeScheme
typeDefinitionTypeScheme x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionFieldName = (Core.Name "typeScheme")})),
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
              Core.projectionFieldName = (Core.Name "typeScheme")})),
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
              Core.projectionFieldName = (Core.Name "name")})),
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
-- | DSL accessor for the body of hydra.packaging.ModuleName
unModuleName :: Phantoms.TTerm Packaging.ModuleName -> Phantoms.TTerm String
unModuleName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.ModuleName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.packaging.PackageName
unPackageName :: Phantoms.TTerm Packaging.PackageName -> Phantoms.TTerm String
unPackageName x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.PackageName")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL accessor for the body of hydra.packaging.Version
unVersion :: Phantoms.TTerm Packaging.Version -> Phantoms.TTerm String
unVersion x =
    Phantoms.TTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.Version")),
      Core.applicationArgument = (Phantoms.unTTerm x)}))
-- | DSL constructor for the hydra.packaging.Version wrapper
version :: Phantoms.TTerm String -> Phantoms.TTerm Packaging.Version
version x =
    Phantoms.TTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Version"),
      Core.wrappedTermBody = (Phantoms.unTTerm x)}))
