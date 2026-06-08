-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.packaging

module Hydra.Encode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Typing as Typing
import qualified Hydra.Haskell.Lib.Lists as Lists
import qualified Hydra.Haskell.Lib.Optionals as Optionals
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | Encoder for hydra.packaging.Definition
definition :: Packaging.Definition -> Core.Term
definition x =
    case x of
      Packaging.DefinitionTerm v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (termDefinition v0)}})
      Packaging.DefinitionType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (typeDefinition v0)}})
      Packaging.DefinitionPrimitive v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.Definition"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "primitive"),
          Core.fieldTerm = (primitiveDefinition v0)}})
-- | Encoder for hydra.packaging.DefinitionReference
definitionReference :: Packaging.DefinitionReference -> Core.Term
definitionReference x =
    case x of
      Packaging.DefinitionReferenceType v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = (EncodeCore.name v0)}})
      Packaging.DefinitionReferenceTerm v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (EncodeCore.name v0)}})
      Packaging.DefinitionReferencePrimitive v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.DefinitionReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "primitive"),
          Core.fieldTerm = (EncodeCore.name v0)}})
-- | Encoder for hydra.packaging.EntityMetadata
entityMetadata :: Packaging.EntityMetadata -> Core.Term
entityMetadata x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.EntityMetadata"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Packaging.entityMetadataDescription x))},
        Core.Field {
          Core.fieldName = (Core.Name "comments"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) xs)) (Packaging.entityMetadataComments x))},
        Core.Field {
          Core.fieldName = (Core.Name "seeAlso"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map entityReference xs)) (Packaging.entityMetadataSeeAlso x))},
        Core.Field {
          Core.fieldName = (Core.Name "lifecycle"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map lifecycleInfo opt)) (Packaging.entityMetadataLifecycle x))}]})
-- | Encoder for hydra.packaging.EntityReference
entityReference :: Packaging.EntityReference -> Core.Term
entityReference x =
    case x of
      Packaging.EntityReferencePackage v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = (packageName v0)}})
      Packaging.EntityReferenceModule v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (moduleName v0)}})
      Packaging.EntityReferenceDefinition v0 -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.EntityReference"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "definition"),
          Core.fieldTerm = (definitionReference v0)}})
-- | Encoder for hydra.packaging.LifecycleInfo
lifecycleInfo :: Packaging.LifecycleInfo -> Core.Term
lifecycleInfo x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.LifecycleInfo"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "availableSince"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map version opt)) (Packaging.lifecycleInfoAvailableSince x))},
        Core.Field {
          Core.fieldName = (Core.Name "deprecatedSince"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map version opt)) (Packaging.lifecycleInfoDeprecatedSince x))}]})
-- | Encoder for hydra.packaging.Module
module_ :: Packaging.Module -> Core.Term
module_ x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (moduleName (Packaging.moduleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map entityMetadata opt)) (Packaging.moduleMetadata x))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map moduleDependency xs)) (Packaging.moduleDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map definition xs)) (Packaging.moduleDefinitions x))}]})
-- | Encoder for hydra.packaging.ModuleDependency
moduleDependency :: Packaging.ModuleDependency -> Core.Term
moduleDependency x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.ModuleDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "module"),
          Core.fieldTerm = (moduleName (Packaging.moduleDependencyModule x))},
        Core.Field {
          Core.fieldName = (Core.Name "package"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map packageName opt)) (Packaging.moduleDependencyPackage x))}]})
-- | Encoder for hydra.packaging.ModuleName
moduleName :: Packaging.ModuleName -> Core.Term
moduleName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.ModuleName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unModuleName x))})
-- | Encoder for hydra.packaging.Package
package :: Packaging.Package -> Core.Term
package x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Package"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (packageName (Packaging.packageName x))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map entityMetadata opt)) (Packaging.packageMetadata x))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map packageDependency xs)) (Packaging.packageDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map module_ xs)) (Packaging.packageModules x))}]})
-- | Encoder for hydra.packaging.PackageDependency
packageDependency :: Packaging.PackageDependency -> Core.Term
packageDependency x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PackageDependency"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (packageName (Packaging.packageDependencyName x))},
        Core.Field {
          Core.fieldName = (Core.Name "version"),
          Core.fieldTerm = (versionSpecifier (Packaging.packageDependencyVersion x))}]})
-- | Encoder for hydra.packaging.PackageName
packageName :: Packaging.PackageName -> Core.Term
packageName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unPackageName x))})
-- | Encoder for hydra.packaging.PrimitiveDefinition
primitiveDefinition :: Packaging.PrimitiveDefinition -> Core.Term
primitiveDefinition x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.PrimitiveDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Packaging.primitiveDefinitionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map entityMetadata opt)) (Packaging.primitiveDefinitionMetadata x))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = (Typing.termSignature (Packaging.primitiveDefinitionSignature x))},
        Core.Field {
          Core.fieldName = (Core.Name "isPure"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Packaging.primitiveDefinitionIsPure x))},
        Core.Field {
          Core.fieldName = (Core.Name "isTotal"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralBoolean x2)) (Packaging.primitiveDefinitionIsTotal x))},
        Core.Field {
          Core.fieldName = (Core.Name "defaultImplementation"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map EncodeCore.term opt)) (Packaging.primitiveDefinitionDefaultImplementation x))}]})
-- | Encoder for hydra.packaging.TermDefinition
termDefinition :: Packaging.TermDefinition -> Core.Term
termDefinition x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TermDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Packaging.termDefinitionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map entityMetadata opt)) (Packaging.termDefinitionMetadata x))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map Typing.termSignature opt)) (Packaging.termDefinitionSignature x))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (EncodeCore.term (Packaging.termDefinitionBody x))}]})
-- | Encoder for hydra.packaging.TypeDefinition
typeDefinition :: Packaging.TypeDefinition -> Core.Term
typeDefinition x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.TypeDefinition"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (EncodeCore.name (Packaging.typeDefinitionName x))},
        Core.Field {
          Core.fieldName = (Core.Name "metadata"),
          Core.fieldTerm = ((\opt -> Core.TermOptional (Optionals.map entityMetadata opt)) (Packaging.typeDefinitionMetadata x))},
        Core.Field {
          Core.fieldName = (Core.Name "body"),
          Core.fieldTerm = (EncodeCore.typeScheme (Packaging.typeDefinitionBody x))}]})
-- | Encoder for hydra.packaging.Version
version :: Packaging.Version -> Core.Term
version x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Version"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unVersion x))})
-- | Encoder for hydra.packaging.VersionSpecifier
versionSpecifier :: Packaging.VersionSpecifier -> Core.Term
versionSpecifier x =
    case x of
      Packaging.VersionSpecifierAny -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.VersionSpecifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "any"),
          Core.fieldTerm = Core.TermUnit}})
