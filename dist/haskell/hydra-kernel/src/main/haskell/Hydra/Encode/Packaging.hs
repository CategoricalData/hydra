-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.packaging

module Hydra.Encode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Encode.Typing as Typing
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maybes as Maybes
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
-- | Encoder for hydra.packaging.FileExtension
fileExtension :: Packaging.FileExtension -> Core.Term
fileExtension x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unFileExtension x))})
-- | Encoder for hydra.packaging.Module
module_ :: Packaging.Module -> Core.Term
module_ x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Packaging.moduleDescription x))},
        Core.Field {
          Core.fieldName = (Core.Name "name"),
          Core.fieldTerm = (moduleName (Packaging.moduleName x))},
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
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map packageName opt)) (Packaging.moduleDependencyPackage x))}]})
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
          Core.fieldName = (Core.Name "modules"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map module_ xs)) (Packaging.packageModules x))},
        Core.Field {
          Core.fieldName = (Core.Name "dependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map packageDependency xs)) (Packaging.packageDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Packaging.packageDescription x))}]})
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
          Core.fieldTerm = (packageVersionSpecifier (Packaging.packageDependencyVersion x))}]})
-- | Encoder for hydra.packaging.PackageName
packageName :: Packaging.PackageName -> Core.Term
packageName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unPackageName x))})
-- | Encoder for hydra.packaging.PackageVersionSpecifier
packageVersionSpecifier :: Packaging.PackageVersionSpecifier -> Core.Term
packageVersionSpecifier x =
    case x of
      Packaging.PackageVersionSpecifierAny -> Core.TermInject (Core.Injection {
        Core.injectionTypeName = (Core.Name "hydra.packaging.PackageVersionSpecifier"),
        Core.injectionField = Core.Field {
          Core.fieldName = (Core.Name "any"),
          Core.fieldTerm = Core.TermUnit}})
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
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.primitiveDefinitionDescription x))},
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
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map EncodeCore.term opt)) (Packaging.primitiveDefinitionDefaultImplementation x))}]})
-- | Encoder for hydra.packaging.QualifiedName
qualifiedName :: Packaging.QualifiedName -> Core.Term
qualifiedName x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "moduleName"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map moduleName opt)) (Packaging.qualifiedNameModuleName x))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.qualifiedNameLocal x))}]})
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
          Core.fieldName = (Core.Name "term"),
          Core.fieldTerm = (EncodeCore.term (Packaging.termDefinitionTerm x))},
        Core.Field {
          Core.fieldName = (Core.Name "signature"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map Typing.termSignature opt)) (Packaging.termDefinitionSignature x))}]})
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
          Core.fieldName = (Core.Name "typeScheme"),
          Core.fieldTerm = (EncodeCore.typeScheme (Packaging.typeDefinitionTypeScheme x))}]})
