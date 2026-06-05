module Hydra.Sources.Kernel.Types.Coders where

-- Standard type-level kernel imports
import           Hydra.Kernel hiding (language, languageName, languageConstraints)
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Typing as Typing
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Hydra.Sources.Kernel.Types.Errors as Error
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Util as Util
import qualified Hydra.Sources.Kernel.Types.Variants as Variants


ns :: ModuleName
ns = ModuleName "hydra.coders"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Error.ns, Graph.ns, Variants.ns, Core.ns, Typing.ns, Util.ns],
            moduleMetadata = descriptionMetadata (Just "Abstractions for paired transformations between languages")}
  where
    definitions = [
      adapter,
      adapterContext,
      bicoder,
      caseConventions,
      coder,
      coderDirection,
      language,
      languageConstraints,
      languageFeature,
      languageName,
      symmetricAdapter,
      traversalOrder,
      typeAdapter]

adapter :: TypeDefinition
adapter = define "Adapter" $
  doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
  T.forAlls ["t1", "t2", "v1", "v2"] $ T.record [
    "isLossy">:
      doc "Whether information may be lost in the course of this adaptation"
      T.boolean,
    "source">:
      doc "The source type"
      "t1",
    "target">:
      doc "The target type"
      "t2",
    "coder">:
      doc "The coder for transforming instances of the source type to instances of the target type" $
      coder @@ "v1" @@ "v2"]

adapterContext :: TypeDefinition
adapterContext = define "AdapterContext" $
  doc "An evaluation context together with a source language and a target language" $
  T.record [
    "graph">:
      doc "The underlying graph of elements and primitives"
      Graph.graph,
    "language">:
      doc "The language being encoded or decoded"
      language,
    "adapters">:
      doc "A map of type names to adapters for those types" $
      T.map Core.name (adapter
        @@ Core.type_ @@ Core.type_
        @@ Core.term @@ Core.term)]

bicoder :: TypeDefinition
bicoder = define "Bicoder" $
  doc "A two-level encoder and decoder, operating both at a type level and an instance (data) level" $
  T.forAlls ["t1", "t2", "v1", "v2"] $ T.record [
    "encode">:
      doc "A function from source types to adapters" $
      "t1" ~> adapter @@ "t1" @@ "t2" @@ "v1" @@ "v2",
    "decode">:
      doc "A function from target types to adapters" $
      "t2" ~> adapter @@ "t2" @@ "t1" @@ "v2" @@ "v1"]

caseConventions :: TypeDefinition
caseConventions = define "CaseConventions" $
  doc "Per-target case conventions for name forms that vary across emission targets" $
  T.record [
    "constant">:
      doc "Convention for compile-time constant names"
      Util.caseConvention,
    "directory">:
      doc "Convention for each directory level in the emitted source tree"
      Util.caseConvention,
    "enumValue">:
      doc "Convention for enum-variant value names"
      Util.caseConvention,
    "field">:
      doc "Convention for record field names"
      Util.caseConvention,
    "file">:
      doc "Convention for the source-file basename"
      Util.caseConvention,
    "module">:
      doc "Convention for a single segment of a module name"
      Util.caseConvention,
    "term">:
      doc "Convention for top-level term definitions (functions, module-level values)"
      Util.caseConvention,
    "termVariable">:
      doc "Convention for locally-bound term names (lambda parameters, let-bindings)"
      Util.caseConvention,
    "type">:
      doc "Convention for type names"
      Util.caseConvention,
    "typeVariable">:
      doc "Convention for type-level variable names"
      Util.caseConvention]

coder :: TypeDefinition
coder = define "Coder" $
  doc "An encoder and decoder; a bidirectional transformation between two types" $
  T.forAlls ["v1", "v2"] $ T.record [
    "encode">:
      doc "A function which encodes source values as target values, given an InferenceContext for fresh-variable state and subterm-path tracing" $
      Typing.inferenceContext ~> "v1" ~> T.either_ Error.error_ "v2",
    "decode">:
      doc "A function which decodes target values as source values, given an InferenceContext for fresh-variable state and subterm-path tracing" $
      Typing.inferenceContext ~> "v2" ~> T.either_ Error.error_ "v1"]

coderDirection :: TypeDefinition
coderDirection = define "CoderDirection" $
  doc "Indicates either the 'out' or the 'in' direction of a coder" $
  T.enum [
    "encode",
    "decode"]

language :: TypeDefinition
language = define "Language" $
  doc "A named language together with its grammar constraints, capability profile, naming conventions, and conventional file extension" $
  T.record [
    "name">:
      doc "The unique name of the language"
      languageName,
    "constraints">:
      doc "Constraints which characterize the language's type and term grammars"
      languageConstraints,
    "supportedFeatures">:
      doc "Target-language or target-runtime capabilities the emitter may assume are available"
      (T.set languageFeature),
    "caseConventions">:
      doc "Per-target case conventions for the various kinds of identifiers emitted by the coder"
      caseConventions,
    "defaultFileExtension">:
      doc "Conventional file extension for emitted source files, without the leading dot (e.g. \"scala\", \"py\")"
      Util.fileExtension]

languageConstraints :: TypeDefinition
languageConstraints = define "LanguageConstraints" $
  doc "A set of constraints on valid type and term expressions, characterizing a language" $
  T.record [
    "literalVariants">:
      doc "All supported literal variants" $
      T.set Variants.literalVariant,
    "floatTypes">:
      doc "All supported float types" $
      T.set Core.floatType,
    "integerTypes">:
      doc "All supported integer types" $
      T.set Core.integerType,
    "termVariants">:
      doc "All supported term variants" $
      T.set Variants.termVariant,
    "typeVariants">:
      doc "All supported type variants" $
      T.set Variants.typeVariant,
    "types">:
      doc "A logical set of types, as a predicate which tests a type for inclusion" $
      Core.type_ ~> T.boolean]

languageFeature :: TypeDefinition
languageFeature = define "LanguageFeature" $
  doc "A target-language or target-runtime capability the coder may rely on. Absence from a Language's supportedFeatures set means the emitter must work around the missing capability." $
  T.union [
    "partialApplication">:
      doc "The target runtime can invoke an n-ary function with fewer than n arguments without error. When absent, the emitter eta-expands partially-applied terms."
      T.unit,
    "nestedCaseStatements">:
      doc "The target runtime can handle deeply nested case statements without stack issues. When absent, the emitter hoists cases out into top-level helpers."
      T.unit,
    "nestedPolymorphicLetBindings">:
      doc "The target language permits polymorphic let-bindings in expression position. When absent, the emitter hoists polymorphic lets to top level."
      T.unit]

languageName :: TypeDefinition
languageName = define "LanguageName" $
  doc "The unique name of a language" $
  T.wrap T.string

symmetricAdapter :: TypeDefinition
symmetricAdapter = define "SymmetricAdapter" $
  doc "A bidirectional encoder which maps between the same type and term languages on either side" $
  T.forAlls ["t", "v"] $ adapter @@ "t" @@ "t" @@ "v" @@ "v"

traversalOrder :: TypeDefinition
traversalOrder = define "TraversalOrder" $
  doc "Specifies either a pre-order or post-order traversal" $
  T.union [
    "pre">: doc "Pre-order traversal" T.unit,
    "post">: doc "Post-order traversal" T.unit]

typeAdapter :: TypeDefinition
typeAdapter = define "TypeAdapter" $
  doc "A function which maps a Hydra type to a symmetric adapter between types and terms" $
  adapterContext ~> Core.type_ ~> T.either_ T.string
    (symmetricAdapter @@ Core.type_ @@ Core.term)
