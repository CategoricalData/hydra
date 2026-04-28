-- Note: this is an automatically generated file. Do not edit.
-- | Term encoders for hydra.packaging

module Hydra.Encode.Packaging where
import qualified Hydra.Core as Core
import qualified Hydra.Encode.Core as EncodeCore
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Packaging as Packaging
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
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
fileExtension :: Packaging.FileExtension -> Core.Term
fileExtension x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.FileExtension"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unFileExtension x))})
module_ :: Packaging.Module -> Core.Term
module_ x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Module"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Packaging.moduleDescription x))},
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = (namespace (Packaging.moduleNamespace x))},
        Core.Field {
          Core.fieldName = (Core.Name "termDependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map namespace xs)) (Packaging.moduleTermDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "typeDependencies"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map namespace xs)) (Packaging.moduleTypeDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "definitions"),
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map definition xs)) (Packaging.moduleDefinitions x))}]})
namespace :: Packaging.Namespace -> Core.Term
namespace x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.Namespace"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unNamespace x))})
namespaces :: (t0 -> Core.Term) -> Packaging.Namespaces t0 -> Core.Term
namespaces n x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.Namespaces"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "focus"),
          Core.fieldTerm = ((\p -> Core.TermPair (Pairs.bimap namespace n p)) (Packaging.namespacesFocus x))},
        Core.Field {
          Core.fieldName = (Core.Name "mapping"),
          Core.fieldTerm = ((\m -> Core.TermMap (Maps.bimap namespace n m)) (Packaging.namespacesMapping x))}]})
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
          Core.fieldTerm = ((\xs -> Core.TermList (Lists.map packageName xs)) (Packaging.packageDependencies x))},
        Core.Field {
          Core.fieldName = (Core.Name "description"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map (\x2 -> Core.TermLiteral (Core.LiteralString x2)) opt)) (Packaging.packageDescription x))}]})
packageName :: Packaging.PackageName -> Core.Term
packageName x =
    Core.TermWrap (Core.WrappedTerm {
      Core.wrappedTermTypeName = (Core.Name "hydra.packaging.PackageName"),
      Core.wrappedTermBody = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.unPackageName x))})
qualifiedName :: Packaging.QualifiedName -> Core.Term
qualifiedName x =
    Core.TermRecord (Core.Record {
      Core.recordTypeName = (Core.Name "hydra.packaging.QualifiedName"),
      Core.recordFields = [
        Core.Field {
          Core.fieldName = (Core.Name "namespace"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map namespace opt)) (Packaging.qualifiedNameNamespace x))},
        Core.Field {
          Core.fieldName = (Core.Name "local"),
          Core.fieldTerm = ((\x2 -> Core.TermLiteral (Core.LiteralString x2)) (Packaging.qualifiedNameLocal x))}]})
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
          Core.fieldName = (Core.Name "type"),
          Core.fieldTerm = ((\opt -> Core.TermMaybe (Maybes.map EncodeCore.typeScheme opt)) (Packaging.termDefinitionTypeScheme x))}]})
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
