-- Note: this is an automatically generated file. Do not edit.
-- | DSL functions for hydra.packaging

module Hydra.Dsl.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Dsl.Core as DslCore
import qualified Hydra.Dsl.Typing as DslTyping
import qualified Hydra.Packaging as Packaging
import qualified Hydra.Typed as Typed
import qualified Hydra.Typing as Typing
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | DSL injection for the primitive variant of hydra.packaging.Definition
definitionPrimitive :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Packaging.Definition
definitionPrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the primitive variant of hydra.packaging.DefinitionReference
definitionReferencePrimitive :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.DefinitionReference
definitionReferencePrimitive x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "primitive"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.packaging.DefinitionReference
definitionReferenceTerm :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.DefinitionReference
definitionReferenceTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.packaging.DefinitionReference
definitionReferenceType :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.DefinitionReference
definitionReferenceType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the term variant of hydra.packaging.Definition
definitionTerm :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm Packaging.Definition
definitionTerm x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "term"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the type variant of hydra.packaging.Definition
definitionType :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm Packaging.Definition
definitionType x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "type"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for hydra.packaging.EntityLifecycle
entityLifecycle :: Typed.TypedTerm (Maybe Packaging.Version) -> Typed.TypedTerm (Maybe Packaging.Version) -> Typed.TypedTerm Packaging.EntityLifecycle
entityLifecycle availableSince deprecatedSince =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Typed.unTypedTerm availableSince)},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Typed.unTypedTerm deprecatedSince)}]}))
-- | DSL accessor for the availableSince field of hydra.packaging.EntityLifecycle
entityLifecycleAvailableSince :: Typed.TypedTerm Packaging.EntityLifecycle -> Typed.TypedTerm (Maybe Packaging.Version)
entityLifecycleAvailableSince x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
        Core.projectionFieldName = (Core.Name "availableSince")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the deprecatedSince field of hydra.packaging.EntityLifecycle
entityLifecycleDeprecatedSince :: Typed.TypedTerm Packaging.EntityLifecycle -> Typed.TypedTerm (Maybe Packaging.Version)
entityLifecycleDeprecatedSince x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
        Core.projectionFieldName = (Core.Name "deprecatedSince")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the availableSince field of hydra.packaging.EntityLifecycle
entityLifecycleWithAvailableSince :: Typed.TypedTerm Packaging.EntityLifecycle -> Typed.TypedTerm (Maybe Packaging.Version) -> Typed.TypedTerm Packaging.EntityLifecycle
entityLifecycleWithAvailableSince original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
              Core.projectionFieldName = (Core.Name "deprecatedSince")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the deprecatedSince field of hydra.packaging.EntityLifecycle
entityLifecycleWithDeprecatedSince :: Typed.TypedTerm Packaging.EntityLifecycle -> Typed.TypedTerm (Maybe Packaging.Version) -> Typed.TypedTerm Packaging.EntityLifecycle
entityLifecycleWithDeprecatedSince original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityLifecycle"),
              Core.projectionFieldName = (Core.Name "availableSince")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL constructor for hydra.packaging.EntityMetadata
entityMetadata :: Typed.TypedTerm (Maybe String) -> Typed.TypedTerm [String] -> Typed.TypedTerm [Packaging.EntityReference] -> Typed.TypedTerm (Maybe Packaging.EntityLifecycle) -> Typed.TypedTerm Packaging.EntityMetadata
entityMetadata description comments seeAlso lifecycle =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm description)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm comments)},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Typed.unTypedTerm seeAlso)},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = (Typed.unTypedTerm lifecycle)}]}))
-- | DSL accessor for the comments field of hydra.packaging.EntityMetadata
entityMetadataComments :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm [String]
entityMetadataComments x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
        Core.projectionFieldName = (Core.Name "comments")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the description field of hydra.packaging.EntityMetadata
entityMetadataDescription :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm (Maybe String)
entityMetadataDescription x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
        Core.projectionFieldName = (Core.Name "description")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the lifecycle field of hydra.packaging.EntityMetadata
entityMetadataLifecycle :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm (Maybe Packaging.EntityLifecycle)
entityMetadataLifecycle x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
        Core.projectionFieldName = (Core.Name "lifecycle")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the seeAlso field of hydra.packaging.EntityMetadata
entityMetadataSeeAlso :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm [Packaging.EntityReference]
entityMetadataSeeAlso x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
        Core.projectionFieldName = (Core.Name "seeAlso")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the comments field of hydra.packaging.EntityMetadata
entityMetadataWithComments :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm [String] -> Typed.TypedTerm Packaging.EntityMetadata
entityMetadataWithComments original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "lifecycle")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the description field of hydra.packaging.EntityMetadata
entityMetadataWithDescription :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm (Maybe String) -> Typed.TypedTerm Packaging.EntityMetadata
entityMetadataWithDescription original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "lifecycle")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the lifecycle field of hydra.packaging.EntityMetadata
entityMetadataWithLifecycle :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm (Maybe Packaging.EntityLifecycle) -> Typed.TypedTerm Packaging.EntityMetadata
entityMetadataWithLifecycle original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "seeAlso")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the seeAlso field of hydra.packaging.EntityMetadata
entityMetadataWithSeeAlso :: Typed.TypedTerm Packaging.EntityMetadata -> Typed.TypedTerm [Packaging.EntityReference] -> Typed.TypedTerm Packaging.EntityMetadata
entityMetadataWithSeeAlso original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "description")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "comments")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
              Core.projectionFieldName = (Core.Name "lifecycle")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL injection for the definition variant of hydra.packaging.EntityReference
entityReferenceDefinition :: Typed.TypedTerm Packaging.DefinitionReference -> Typed.TypedTerm Packaging.EntityReference
entityReferenceDefinition x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "definition"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the module variant of hydra.packaging.EntityReference
entityReferenceModule :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Packaging.EntityReference
entityReferenceModule x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "module"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL injection for the package variant of hydra.packaging.EntityReference
entityReferencePackage :: Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm Packaging.EntityReference
entityReferencePackage x =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "package"),
        Core.fieldTerm = (Typed.unTypedTerm x)}}))
-- | DSL constructor for the hydra.packaging.FileExtension wrapper
fileExtension :: Typed.TypedTerm String -> Typed.TypedTerm Packaging.FileExtension
fileExtension x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.packaging.Module
module_ :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm [Packaging.ModuleDependency] -> Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm Packaging.Module
module_ name metadata dependencies definitions =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm dependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Typed.unTypedTerm definitions)}]}))
-- | DSL accessor for the definitions field of hydra.packaging.Module
moduleDefinitions :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm [Packaging.Definition]
moduleDefinitions x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "definitions")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the dependencies field of hydra.packaging.Module
moduleDependencies :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm [Packaging.ModuleDependency]
moduleDependencies x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.packaging.ModuleDependency
moduleDependency :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm (Maybe Packaging.PackageName) -> Typed.TypedTerm Packaging.ModuleDependency
moduleDependency module_ package =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm module_)},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm package)}]}))
-- | DSL accessor for the module field of hydra.packaging.ModuleDependency
moduleDependencyModule :: Typed.TypedTerm Packaging.ModuleDependency -> Typed.TypedTerm Packaging.ModuleName
moduleDependencyModule x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
        Core.projectionFieldName = (Core.Name "module")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the package field of hydra.packaging.ModuleDependency
moduleDependencyPackage :: Typed.TypedTerm Packaging.ModuleDependency -> Typed.TypedTerm (Maybe Packaging.PackageName)
moduleDependencyPackage x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
        Core.projectionFieldName = (Core.Name "package")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the module field of hydra.packaging.ModuleDependency
moduleDependencyWithModule :: Typed.TypedTerm Packaging.ModuleDependency -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Packaging.ModuleDependency
moduleDependencyWithModule original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
              Core.projectionFieldName = (Core.Name "package")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the package field of hydra.packaging.ModuleDependency
moduleDependencyWithPackage :: Typed.TypedTerm Packaging.ModuleDependency -> Typed.TypedTerm (Maybe Packaging.PackageName) -> Typed.TypedTerm Packaging.ModuleDependency
moduleDependencyWithPackage original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
              Core.projectionFieldName = (Core.Name "module")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the metadata field of hydra.packaging.Module
moduleMetadata :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm (Maybe Packaging.EntityMetadata)
moduleMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.Module
moduleName :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm Packaging.ModuleName
moduleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.packaging.ModuleName wrapper
moduleName2 :: Typed.TypedTerm String -> Typed.TypedTerm Packaging.ModuleName
moduleName2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.ModuleName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL updater for the definitions field of hydra.packaging.Module
moduleWithDefinitions :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm [Packaging.Definition] -> Typed.TypedTerm Packaging.Module
moduleWithDefinitions original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the dependencies field of hydra.packaging.Module
moduleWithDependencies :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm [Packaging.ModuleDependency] -> Typed.TypedTerm Packaging.Module
moduleWithDependencies original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.packaging.Module
moduleWithMetadata :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Packaging.Module
moduleWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.Module
moduleWithName :: Typed.TypedTerm Packaging.Module -> Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm Packaging.Module
moduleWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Module"),
              Core.projectionFieldName = (Core.Name "definitions")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.Package
package :: Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm [Packaging.PackageDependency] -> Typed.TypedTerm [Packaging.Module] -> Typed.TypedTerm Packaging.Package
package name metadata dependencies modules =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm dependencies)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm modules)}]}))
-- | DSL accessor for the dependencies field of hydra.packaging.Package
packageDependencies :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm [Packaging.PackageDependency]
packageDependencies x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "dependencies")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for hydra.packaging.PackageDependency
packageDependency :: Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm Packaging.PackageVersionSpecifier -> Typed.TypedTerm Packaging.PackageDependency
packageDependency name version =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Typed.unTypedTerm version)}]}))
-- | DSL accessor for the name field of hydra.packaging.PackageDependency
packageDependencyName :: Typed.TypedTerm Packaging.PackageDependency -> Typed.TypedTerm Packaging.PackageName
packageDependencyName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the version field of hydra.packaging.PackageDependency
packageDependencyVersion :: Typed.TypedTerm Packaging.PackageDependency -> Typed.TypedTerm Packaging.PackageVersionSpecifier
packageDependencyVersion x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
        Core.projectionFieldName = (Core.Name "version")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the name field of hydra.packaging.PackageDependency
packageDependencyWithName :: Typed.TypedTerm Packaging.PackageDependency -> Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm Packaging.PackageDependency
packageDependencyWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
              Core.projectionFieldName = (Core.Name "version")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the version field of hydra.packaging.PackageDependency
packageDependencyWithVersion :: Typed.TypedTerm Packaging.PackageDependency -> Typed.TypedTerm Packaging.PackageVersionSpecifier -> Typed.TypedTerm Packaging.PackageDependency
packageDependencyWithVersion original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PackageDependency"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the metadata field of hydra.packaging.Package
packageMetadata :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm (Maybe Packaging.EntityMetadata)
packageMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the modules field of hydra.packaging.Package
packageModules :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm [Packaging.Module]
packageModules x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "modules")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.Package
packageName :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm Packaging.PackageName
packageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.packaging.PackageName wrapper
packageName2 :: Typed.TypedTerm String -> Typed.TypedTerm Packaging.PackageName
packageName2 x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
-- | DSL injection for the any variant of hydra.packaging.PackageVersionSpecifier
packageVersionSpecifierAny :: Typed.TypedTerm Packaging.PackageVersionSpecifier
packageVersionSpecifierAny =
    Typed.TypedTerm (Core.TermInject (Core.Injection {
      Core.injectionTypeName = (Core.Name "hydra.packaging.PackageVersionSpecifier"),
      Core.injectionField = Core.Field {
        Core.fieldName = (Core.Name "any"),
        Core.fieldTerm = Core.TermUnit}}))
-- | DSL updater for the dependencies field of hydra.packaging.Package
packageWithDependencies :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm [Packaging.PackageDependency] -> Typed.TypedTerm Packaging.Package
packageWithDependencies original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.packaging.Package
packageWithMetadata :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Packaging.Package
packageWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the modules field of hydra.packaging.Package
packageWithModules :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm [Packaging.Module] -> Typed.TypedTerm Packaging.Package
packageWithModules original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the name field of hydra.packaging.Package
packageWithName :: Typed.TypedTerm Packaging.Package -> Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm Packaging.Package
packageWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "dependencies")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.Package"),
              Core.projectionFieldName = (Core.Name "modules")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.PrimitiveDefinition
primitiveDefinition :: Typed.TypedTerm Core.Name -> Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Bool -> Typed.TypedTerm Bool -> Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinition name signature metadata isPure isTotal defaultImplementation =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Typed.unTypedTerm isPure)},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Typed.unTypedTerm isTotal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Typed.unTypedTerm defaultImplementation)}]}))
-- | DSL accessor for the defaultImplementation field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionDefaultImplementation :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm (Maybe Core.Term)
primitiveDefinitionDefaultImplementation x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "defaultImplementation")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the isPure field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionIsPure :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Bool
primitiveDefinitionIsPure x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "isPure")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the isTotal field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionIsTotal :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Bool
primitiveDefinitionIsTotal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "isTotal")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the metadata field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionMetadata :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata)
primitiveDefinitionMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionName :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Core.Name
primitiveDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionSignature :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Typing.TermSignature
primitiveDefinitionSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the defaultImplementation field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithDefaultImplementation :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm (Maybe Core.Term) -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithDefaultImplementation original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the isPure field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithIsPure :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Bool -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithIsPure original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the isTotal field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithIsTotal :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Bool -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithIsTotal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the metadata field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithMetadata :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithName :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.packaging.PrimitiveDefinition
primitiveDefinitionWithSignature :: Typed.TypedTerm Packaging.PrimitiveDefinition -> Typed.TypedTerm Typing.TermSignature -> Typed.TypedTerm Packaging.PrimitiveDefinition
primitiveDefinitionWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isPure")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "isTotal")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
              Core.projectionFieldName = (Core.Name "defaultImplementation")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.QualifiedName
qualifiedName :: Typed.TypedTerm (Maybe Packaging.ModuleName) -> Typed.TypedTerm String -> Typed.TypedTerm Packaging.QualifiedName
qualifiedName moduleName local =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm moduleName)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm local)}]}))
-- | DSL accessor for the local field of hydra.packaging.QualifiedName
qualifiedNameLocal :: Typed.TypedTerm Packaging.QualifiedName -> Typed.TypedTerm String
qualifiedNameLocal x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionFieldName = (Core.Name "local")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the moduleName field of hydra.packaging.QualifiedName
qualifiedNameModuleName :: Typed.TypedTerm Packaging.QualifiedName -> Typed.TypedTerm (Maybe Packaging.ModuleName)
qualifiedNameModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
        Core.projectionFieldName = (Core.Name "moduleName")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the local field of hydra.packaging.QualifiedName
qualifiedNameWithLocal :: Typed.TypedTerm Packaging.QualifiedName -> Typed.TypedTerm String -> Typed.TypedTerm Packaging.QualifiedName
qualifiedNameWithLocal original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionFieldName = (Core.Name "moduleName")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the moduleName field of hydra.packaging.QualifiedName
qualifiedNameWithModuleName :: Typed.TypedTerm Packaging.QualifiedName -> Typed.TypedTerm (Maybe Packaging.ModuleName) -> Typed.TypedTerm Packaging.QualifiedName
qualifiedNameWithModuleName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.QualifiedName"),
              Core.projectionFieldName = (Core.Name "local")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.TermDefinition
termDefinition :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Core.Term -> Typed.TypedTerm (Maybe Typing.TermSignature) -> Typed.TypedTerm Packaging.TermDefinition
termDefinition name metadata term signature =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm term)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm signature)}]}))
-- | DSL accessor for the metadata field of hydra.packaging.TermDefinition
termDefinitionMetadata :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata)
termDefinitionMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.TermDefinition
termDefinitionName :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm Core.Name
termDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the signature field of hydra.packaging.TermDefinition
termDefinitionSignature :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm (Maybe Typing.TermSignature)
termDefinitionSignature x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "signature")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the term field of hydra.packaging.TermDefinition
termDefinitionTerm :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm Core.Term
termDefinitionTerm x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
        Core.projectionFieldName = (Core.Name "term")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the metadata field of hydra.packaging.TermDefinition
termDefinitionWithMetadata :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Packaging.TermDefinition
termDefinitionWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.TermDefinition
termDefinitionWithName :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.TermDefinition
termDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the signature field of hydra.packaging.TermDefinition
termDefinitionWithSignature :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm (Maybe Typing.TermSignature) -> Typed.TypedTerm Packaging.TermDefinition
termDefinitionWithSignature original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "term")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL updater for the term field of hydra.packaging.TermDefinition
termDefinitionWithTerm :: Typed.TypedTerm Packaging.TermDefinition -> Typed.TypedTerm Core.Term -> Typed.TypedTerm Packaging.TermDefinition
termDefinitionWithTerm original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TermDefinition"),
              Core.projectionFieldName = (Core.Name "signature")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL constructor for hydra.packaging.TypeDefinition
typeDefinition :: Typed.TypedTerm Core.Name -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm Packaging.TypeDefinition
typeDefinition name metadata typeScheme =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm name)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm metadata)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Typed.unTypedTerm typeScheme)}]}))
-- | DSL accessor for the metadata field of hydra.packaging.TypeDefinition
typeDefinitionMetadata :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata)
typeDefinitionMetadata x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionFieldName = (Core.Name "metadata")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the name field of hydra.packaging.TypeDefinition
typeDefinitionName :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm Core.Name
typeDefinitionName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionFieldName = (Core.Name "name")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the typeScheme field of hydra.packaging.TypeDefinition
typeDefinitionTypeScheme :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm Core.TypeScheme
typeDefinitionTypeScheme x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermProject (Core.Projection {
        Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
        Core.projectionFieldName = (Core.Name "typeScheme")})),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL updater for the metadata field of hydra.packaging.TypeDefinition
typeDefinitionWithMetadata :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm (Maybe Packaging.EntityMetadata) -> Typed.TypedTerm Packaging.TypeDefinition
typeDefinitionWithMetadata original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the name field of hydra.packaging.TypeDefinition
typeDefinitionWithName :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm Core.Name -> Typed.TypedTerm Packaging.TypeDefinition
typeDefinitionWithName original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "typeScheme")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))}]}))
-- | DSL updater for the typeScheme field of hydra.packaging.TypeDefinition
typeDefinitionWithTypeScheme :: Typed.TypedTerm Packaging.TypeDefinition -> Typed.TypedTerm Core.TypeScheme -> Typed.TypedTerm Packaging.TypeDefinition
typeDefinitionWithTypeScheme original newVal =
    Typed.TypedTerm (Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "name")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = (Core.TermApplication (Core.Application {
            Core.applicationFunction = (Core.TermProject (Core.Projection {
              Core.projectionTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
              Core.projectionFieldName = (Core.Name "metadata")})),
            Core.applicationArgument = (Typed.unTypedTerm original)}))},
        Core.Field {
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (Typed.unTypedTerm newVal)}]}))
-- | DSL accessor for the body of hydra.packaging.FileExtension
unFileExtension :: Typed.TypedTerm Packaging.FileExtension -> Typed.TypedTerm String
unFileExtension x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.FileExtension")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.packaging.ModuleName
unModuleName :: Typed.TypedTerm Packaging.ModuleName -> Typed.TypedTerm String
unModuleName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.ModuleName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.packaging.PackageName
unPackageName :: Typed.TypedTerm Packaging.PackageName -> Typed.TypedTerm String
unPackageName x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.PackageName")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL accessor for the body of hydra.packaging.Version
unVersion :: Typed.TypedTerm Packaging.Version -> Typed.TypedTerm String
unVersion x =
    Typed.TypedTerm (Core.TermApplication (Core.Application {
      Core.applicationFunction = (Core.TermUnwrap (Core.Name "hydra.packaging.Version")),
      Core.applicationArgument = (Typed.unTypedTerm x)}))
-- | DSL constructor for the hydra.packaging.Version wrapper
version :: Typed.TypedTerm String -> Typed.TypedTerm Packaging.Version
version x =
    Typed.TypedTerm (Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Version"),
      Core.wrappedTermBody = (Typed.unTypedTerm x)}))
