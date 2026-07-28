module Hydra.Sources.Build.Format where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel                    hiding (
  definitionNameConventionProfile, definitionValidationProfile, digestEntry, digestKind,
  documentationProfile, generation, generationMode, hostOverride, inputDigest, languageName,
  moduleValidationProfile, outputDigest, packageDescriptor, packageManifest,
  packageValidationConfiguration, packageValidationProfile, repositoryDescriptor, severity,
  sha256Hash, termValidationProfile, typeValidationProfile)
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Packaging as Packaging
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.build.format"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Packaging.ns],
            moduleMetadata = descriptionMetadata (Just ("Type-level specifications of the JSON formats used by Hydra's build system:"
              ++ " the repository descriptor (hydra.json), per-package descriptors (package.json),"
              ++ " per-source-set input and output digests (digest.json), generated-source manifests (manifest.json),"
              ++ " and per-package validation configuration."
              ++ " Each on-disk file is the canonical JSON encoding of the corresponding type."
              ++ " Overlay build configuration (overlay/<lang>/<pkg>/build.json) is specified separately, by"
              ++ " hydra.gradle and hydra.python.pyproject. See https://github.com/CategoricalData/hydra/issues/512"))}
  where
    definitions = [
      definitionNameConventionProfile,
      definitionValidationProfile,
      digestEntry,
      digestKind,
      documentationProfile,
      generation,
      generationMode,
      hostOverride,
      inputDigest,
      languageName,
      moduleValidationProfile,
      outputDigest,
      packageDescriptor,
      packageManifest,
      packageValidationConfiguration,
      packageValidationProfile,
      repositoryDescriptor,
      severity,
      sha256Hash,
      termValidationProfile,
      typeValidationProfile]

definitionNameConventionProfile :: TypeDefinition
definitionNameConventionProfile = define "DefinitionNameConventionProfile" $
  doc "Configuration for the definition-name-convention check" $
  T.record [
    "severity">:
      doc "How a finding of this check is treated"
      severity,
    "allowUnderscores">:
      doc "Whether underscores are permitted in definition names"
      T.boolean]

definitionValidationProfile :: TypeDefinition
definitionValidationProfile = define "DefinitionValidationProfile" $
  doc "Validation rules applying to individual definitions within a module. Absent fields mean the corresponding check is not configured." $
  T.record [
    "hasDescription">:
      doc "Configuration for the definition-documentation check" $
      T.optional documentationProfile,
    "ordering">:
      doc "Whether definitions must appear in alphabetical order" $
      T.optional severity,
    "nameConvention">:
      doc "Configuration for the definition-name-convention check" $
      T.optional definitionNameConventionProfile,
    "notInModuleName">:
      doc "Whether a definition name may duplicate part of its module name" $
      T.optional severity,
    "duplicateName">:
      doc "Whether duplicate definition names within a module are reported" $
      T.optional severity]

digestEntry :: TypeDefinition
digestEntry = define "DigestEntry" $
  doc "One file's classification and content hash within a digest" $
  T.record [
    "kind">:
      doc "The kind of artifact recorded"
      digestKind,
    "hash">:
      doc "The SHA-256 hash of the file content"
      sha256Hash]

digestKind :: TypeDefinition
digestKind = define "DigestKind" $
  doc ("The kind of artifact recorded in a digest entry. Lets a single digest mix typed entries"
    ++ " without losing the discriminator.") $
  T.union [
    "dslSource">:
      doc "A DSL source file under a package's src/main source tree"
      T.unit,
    "jsonFile">:
      doc "A JSON file under dist/json/"
      T.unit,
    "targetFile">:
      doc "A generated source file under dist/<lang>/"
      T.unit,
    "runtimeFile">:
      doc "A hand-written runtime support file copied into the distribution"
      T.unit,
    "other">:
      doc "Anything else (escape hatch; unknown kinds round-trip as this variant)"
      T.unit]

documentationProfile :: TypeDefinition
documentationProfile = define "DocumentationProfile" $
  doc "Configuration for the definition-documentation check" $
  T.record [
    "severity">:
      doc "How a finding of this check is treated"
      severity,
    "exemptDeprecated">:
      doc "Whether deprecated definitions are exempt from the documentation requirement"
      T.boolean]

generation :: TypeDefinition
generation = define "Generation" $
  doc ("A structured generation-provenance record for generated artifacts, separating gating identity"
    ++ " from informational provenance. Only generatorId gates cache freshness; it is host-independent"
    ++ " by design, since self-hosting requires every host to produce byte-identical output."
    ++ " All other fields are informational and must not gate."
    ++ " Invariant: a shim-mode generation has a revision (a shim has no release version, so the"
    ++ " working-tree revision is its only precise identity).") $
  T.record [
    "generatorId">:
      doc "The gating cache key identifying the generator that produced the outputs"
      T.string,
    "mode">:
      doc "Whether the artifact was produced by a published host or a locally-built migration shim"
      generationMode,
    "host">:
      doc "The producing host language (informational)"
      languageName,
    "hydraVersion">:
      doc "The release version of the producing host (informational; omitted for shim builds)" $
      T.optional Packaging.version,
    "revision">:
      doc "The producing worktree's revision, as <short-sha> with \"-dirty\" appended when the tree has uncommitted changes (informational; required for shim builds)" $
      T.optional T.string,
    "timestamp">:
      doc "The ISO-8601 UTC time of generation (informational; non-deterministic across byte-identical rebuilds)" $
      T.optional T.string]

generationMode :: TypeDefinition
generationMode = define "GenerationMode" $
  doc "The provenance class of a generated artifact" $
  T.union [
    "published">:
      doc "Produced by a published, versioned host"
      T.unit,
    "shim">:
      doc "Produced by a locally-built migration shim, ahead of any published host"
      T.unit]

hostOverride :: TypeDefinition
hostOverride = define "HostOverride" $
  doc ("An override of the default published-host consumption for one host language:"
    ++ " either force a local from-source build, or pin a specific published version.") $
  T.union [
    "local">:
      doc "Build and consume the host locally from source rather than from the package registry"
      T.unit,
    "version">:
      doc "Pin the host to a specific published version"
      Packaging.version]

inputDigest :: TypeDefinition
inputDigest = define "InputDigest" $
  doc ("A per-package, per-source-set input digest (dist/json/<pkg>/build/<set>/digest.json):"
    ++ " Merkle-style hashes of the package's DSL sources, recorded when the package's JSON is"
    ++ " written and consumed by downstream freshness checks."
    ++ " The selfHash summarizes the package's own module hashes; dependencyHashes captures the"
    ++ " selfHash of each declared dependency package at write time, giving transitive invalidation.") $
  T.record [
    "digestFormatVersion">:
      doc "The version of this digest file's own schema"
      T.int32,
    "moduleFormatVersion">:
      doc "The version of the JSON encoding of the sibling module files"
      T.int32,
    "selfHash">:
      doc "A deterministic hash over all of the package's per-module hashes (absent in legacy digests)" $
      T.optional sha256Hash,
    "dependencyHashes">:
      doc "For each declared dependency package, that package's selfHash as recorded at write time" $
      T.map Packaging.packageName sha256Hash,
    "moduleHashes">:
      doc "The hash of each module's source, keyed by module name" $
      T.map Packaging.moduleNameDef sha256Hash]

languageName :: TypeDefinition
languageName = define "LanguageName" $
  doc "The name of a source or target language in Hydra's build system, e.g. \"haskell\", \"java\", or \"python\"" $
  T.wrap T.string

moduleValidationProfile :: TypeDefinition
moduleValidationProfile = define "ModuleValidationProfile" $
  doc "Validation rules applying to a module as a whole. Absent fields mean the corresponding check is not configured." $
  T.record [
    "definitions">:
      doc "Validation rules applying to the module's individual definitions" $
      T.optional definitionValidationProfile,
    "nameConvention">:
      doc "Whether module names must follow naming conventions" $
      T.optional severity,
    "conflictingVariantName">:
      doc "Whether union variant names conflicting across the module are reported" $
      T.optional severity]

outputDigest :: TypeDefinition
outputDigest = define "OutputDigest" $
  doc ("A per-package, per-source-set output digest for one target language"
    ++ " (dist/<lang>/<pkg>/build/<set>/digest.json), recording every input file whose content"
    ++ " determines the generated output, every output file the generation step is responsible for,"
    ++ " and the identity of the generator. A generation step is fresh exactly when all input hashes,"
    ++ " all output hashes, the generator identity, and the recorded package and dependency hashes match.") $
  T.record [
    "digestFormatVersion">:
      doc "The version of this digest file's own schema"
      T.int32,
    "moduleFormatVersion">:
      doc "The version of the JSON encoding of the module files this digest governs"
      T.int32,
    "generator">:
      doc "The flat gating generator identity (a compatibility duplicate of generation.generatorId)"
      T.string,
    "generation">:
      doc "The structured generation-provenance record"
      generation,
    "selfHash">:
      doc "The input package's selfHash as recorded at refresh time (absent in legacy digests)" $
      T.optional sha256Hash,
    "dependencyHashes">:
      doc "For each declared dependency package, that package's selfHash as recorded at refresh time" $
      T.map Packaging.packageName sha256Hash,
    "inputs">:
      doc "Every input file's kind and hash, keyed by worktree-relative path" $
      T.map T.string digestEntry,
    "outputs">:
      doc "Every output file's kind and hash, keyed by path relative to the output directory" $
      T.map T.string digestEntry]

packageDescriptor :: TypeDefinition
packageDescriptor = define "PackageDescriptor" $
  doc ("A package descriptor (packages/<pkg>/package.json): the source-of-truth declaration of a"
    ++ " Hydra package's name, source language, dependencies, and optional per-package configuration.") $
  T.record [
    "packageFormatVersion">:
      doc "The version of this descriptor file's schema"
      T.int32,
    "name">:
      doc "The unique name of the package, e.g. \"hydra-kernel\""
      Packaging.packageName,
    "description">:
      doc "A concise human-readable summary of the package"
      T.string,
    "sourceLanguage">:
      doc "The language in which the package's DSL sources are authored"
      languageName,
    "targetLanguages">:
      doc "The target languages for which distributions of this package are generated. When absent, the default target set applies." $
      T.optional (T.list languageName),
    "dependencies">:
      doc "The names of the packages this package depends upon"
      (T.list Packaging.packageName),
    "validation">:
      doc "Optional per-package validation configuration. When absent, the validation engine's defaults apply." $
      T.optional packageValidationConfiguration]

packageManifest :: TypeDefinition
packageManifest = define "PackageManifest" $
  doc ("A generated-source manifest (dist/<lang>/<pkg>/src/<set>/<lang>/manifest.json): the list of"
    ++ " modules generated into a distribution package, partitioned by role.") $
  T.record [
    "manifestFormatVersion">:
      doc "The version of this manifest file's schema"
      T.int32,
    "moduleFormatVersion">:
      doc "The version of the JSON encoding of the sibling module files"
      T.int32,
    "package">:
      doc "The name of the package this manifest describes"
      Packaging.packageName,
    "mainModules">:
      doc "All main-source-set modules in the package"
      (T.list Packaging.moduleNameDef),
    "testModules">:
      doc "All test-source-set modules in the package"
      (T.list Packaging.moduleNameDef),
    "mainDslModules">:
      doc "The subset of main modules which define type schemas and give rise to generated DSL wrapper modules"
      (T.list Packaging.moduleNameDef),
    "mainEncodingModules">:
      doc "The subset of main modules for which encoding and decoding modules are generated"
      (T.list Packaging.moduleNameDef)]

packageValidationConfiguration :: TypeDefinition
packageValidationConfiguration = define "PackageValidationConfiguration" $
  doc ("The full, opt-in validation configuration for a package, as declared in its package"
    ++ " descriptor. Every field cascades optional; an absent field means the corresponding rule"
    ++ " family is not configured and the validation engine's defaults apply.") $
  T.record [
    "package">:
      doc "Validation rules applying to the package as a whole" $
      T.optional packageValidationProfile,
    "module">:
      doc "Validation rules applying to each module" $
      T.optional moduleValidationProfile,
    "type">:
      doc "Validation rules applying to type-definition bodies" $
      T.optional typeValidationProfile,
    "term">:
      doc "Validation rules applying to term-definition bodies" $
      T.optional termValidationProfile]

packageValidationProfile :: TypeDefinition
packageValidationProfile = define "PackageValidationProfile" $
  doc "Validation rules applying to a package as a whole. Absent fields mean the corresponding check is not configured." $
  T.record [
    "conflictingModuleName">:
      doc "Whether module names conflicting across the package are reported" $
      T.optional severity,
    "duplicateModuleName">:
      doc "Whether duplicate module names within the package are reported" $
      T.optional severity,
    "nameConvention">:
      doc "Whether package names must follow naming conventions" $
      T.optional severity]

repositoryDescriptor :: TypeDefinition
repositoryDescriptor = define "RepositoryDescriptor" $
  doc ("The repository descriptor (hydra.json at the repository root): release and host versions,"
    ++ " host consumption overrides, publication metadata, and the list of source packages.") $
  T.record [
    "currentVersion">:
      doc "The version currently under development"
      Packaging.version,
    "hostVersion">:
      doc "The published host version consumed by default when generating code"
      Packaging.version,
    "hostOverrides">:
      doc "Per-host-language overrides of the default published-host consumption"
      (T.map languageName hostOverride),
    "group">:
      doc "The group identifier under which artifacts are published, e.g. a Maven group id"
      T.string,
    "author">:
      doc "The author attribution for published artifacts"
      T.string,
    "url">:
      doc "The URL of the project"
      T.string,
    "license">:
      doc "The license identifier for published artifacts, e.g. \"Apache-2.0\""
      T.string,
    "packages">:
      doc "The names of the repository's source packages"
      (T.list Packaging.packageName)]

severity :: TypeDefinition
severity = define "Severity" $
  doc "How a validation finding is treated" $
  T.enum ["error", "warning"]

sha256Hash :: TypeDefinition
sha256Hash = define "Sha256Hash" $
  doc "A SHA-256 hash in lowercase hexadecimal notation" $
  T.wrap T.string

termValidationProfile :: TypeDefinition
termValidationProfile = define "TermValidationProfile" $
  doc "Validation rules applying to term-definition bodies. Absent fields mean the corresponding check is not configured." $
  T.record [
    "duplicateBinding">:
      doc "Whether duplicate let bindings are reported" $
      T.optional severity,
    "emptyTermAnnotation">:
      doc "Whether empty term annotations are reported" $
      T.optional severity,
    "undefinedVariable">:
      doc "Whether references to undefined variables are reported" $
      T.optional severity]

typeValidationProfile :: TypeDefinition
typeValidationProfile = define "TypeValidationProfile" $
  doc "Validation rules applying to type-definition bodies. Absent fields mean the corresponding check is not configured." $
  T.record [
    "emptyAnnotation">:
      doc "Whether empty type annotations are reported" $
      T.optional severity,
    "duplicateField">:
      doc "Whether duplicate field names within a record or union are reported" $
      T.optional severity,
    "singleVariantUnion">:
      doc "Whether single-variant unions are reported" $
      T.optional severity]
