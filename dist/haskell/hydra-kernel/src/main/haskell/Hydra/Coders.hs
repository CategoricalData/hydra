-- Note: this is an automatically generated file. Do not edit.
-- | Abstractions for paired transformations between languages

module Hydra.Coders where
import qualified Hydra.Core as Core
import qualified Hydra.Errors as Errors
import qualified Hydra.Graph as Graph
import qualified Hydra.Typing as Typing
import qualified Hydra.Util as Util
import qualified Hydra.Variants as Variants
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | A two-level bidirectional encoder which adapts types to types and terms to terms
data Adapter t1 t2 v1 v2 =
  Adapter {
    -- | Whether information may be lost in the course of this adaptation
    adapterIsLossy :: Bool,
    -- | The source type
    adapterSource :: t1,
    -- | The target type
    adapterTarget :: t2,
    -- | The coder for transforming instances of the source type to instances of the target type
    adapterCoder :: (Coder v1 v2)}
_Adapter = Core.Name "hydra.coders.Adapter"
_Adapter_isLossy = Core.Name "isLossy"
_Adapter_source = Core.Name "source"
_Adapter_target = Core.Name "target"
_Adapter_coder = Core.Name "coder"
-- | An evaluation context together with a source language and a target language
data AdapterContext =
  AdapterContext {
    -- | The underlying graph of elements and primitives
    adapterContextGraph :: Graph.Graph,
    -- | The language being encoded or decoded
    adapterContextLanguage :: Language,
    -- | A map of type names to adapters for those types
    adapterContextAdapters :: (M.Map Core.Name (Adapter Core.Type Core.Type Core.Term Core.Term))}
_AdapterContext = Core.Name "hydra.coders.AdapterContext"
_AdapterContext_graph = Core.Name "graph"
_AdapterContext_language = Core.Name "language"
_AdapterContext_adapters = Core.Name "adapters"
-- | A two-level encoder and decoder, operating both at a type level and an instance (data) level
data Bicoder t1 t2 v1 v2 =
  Bicoder {
    -- | A function from source types to adapters
    bicoderEncode :: (t1 -> Adapter t1 t2 v1 v2),
    -- | A function from target types to adapters
    bicoderDecode :: (t2 -> Adapter t2 t1 v2 v1)}
_Bicoder = Core.Name "hydra.coders.Bicoder"
_Bicoder_encode = Core.Name "encode"
_Bicoder_decode = Core.Name "decode"
-- | Per-target case conventions for name forms that vary across emission targets
data CaseConventions =
  CaseConventions {
    -- | Convention for compile-time constant names
    caseConventionsConstant :: Util.CaseConvention,
    -- | Convention for each directory level in the emitted source tree
    caseConventionsDirectory :: Util.CaseConvention,
    -- | Convention for enum-variant value names
    caseConventionsEnumValue :: Util.CaseConvention,
    -- | Convention for record field names
    caseConventionsField :: Util.CaseConvention,
    -- | Convention for the source-file basename
    caseConventionsFile :: Util.CaseConvention,
    -- | Convention for a single segment of a module name
    caseConventionsModule :: Util.CaseConvention,
    -- | Convention for top-level term definitions (functions, module-level values)
    caseConventionsTerm :: Util.CaseConvention,
    -- | Convention for locally-bound term names (lambda parameters, let-bindings)
    caseConventionsTermVariable :: Util.CaseConvention,
    -- | Convention for type names
    caseConventionsType :: Util.CaseConvention,
    -- | Convention for type-level variable names
    caseConventionsTypeVariable :: Util.CaseConvention}
  deriving (Eq, Ord, Read, Show)
_CaseConventions = Core.Name "hydra.coders.CaseConventions"
_CaseConventions_constant = Core.Name "constant"
_CaseConventions_directory = Core.Name "directory"
_CaseConventions_enumValue = Core.Name "enumValue"
_CaseConventions_field = Core.Name "field"
_CaseConventions_file = Core.Name "file"
_CaseConventions_module = Core.Name "module"
_CaseConventions_term = Core.Name "term"
_CaseConventions_termVariable = Core.Name "termVariable"
_CaseConventions_type = Core.Name "type"
_CaseConventions_typeVariable = Core.Name "typeVariable"
-- | An encoder and decoder; a bidirectional transformation between two types
data Coder v1 v2 =
  Coder {
    -- | A function which encodes source values as target values, given an InferenceContext for fresh-variable state and subterm-path tracing
    coderEncode :: (Typing.InferenceContext -> v1 -> Either Errors.Error v2),
    -- | A function which decodes target values as source values, given an InferenceContext for fresh-variable state and subterm-path tracing
    coderDecode :: (Typing.InferenceContext -> v2 -> Either Errors.Error v1)}
_Coder = Core.Name "hydra.coders.Coder"
_Coder_encode = Core.Name "encode"
_Coder_decode = Core.Name "decode"
-- | Indicates either the 'out' or the 'in' direction of a coder
data CoderDirection =
  CoderDirectionEncode |
  CoderDirectionDecode
  deriving (Eq, Ord, Read, Show)
_CoderDirection = Core.Name "hydra.coders.CoderDirection"
_CoderDirection_encode = Core.Name "encode"
_CoderDirection_decode = Core.Name "decode"
-- | A named language together with its grammar constraints, capability profile, naming conventions, and conventional file extension
data Language =
  Language {
    -- | The unique name of the language
    languageName :: LanguageName,
    -- | Constraints which characterize the language's type and term grammars
    languageConstraints :: LanguageConstraints,
    -- | Target-language or target-runtime capabilities the emitter may assume are available
    languageSupportedFeatures :: (S.Set LanguageFeature),
    -- | Per-target case conventions for the various kinds of identifiers emitted by the coder
    languageCaseConventions :: CaseConventions,
    -- | Conventional file extension for emitted source files, without the leading dot (e.g. "scala", "py")
    languageDefaultFileExtension :: Util.FileExtension}
_Language = Core.Name "hydra.coders.Language"
_Language_name = Core.Name "name"
_Language_constraints = Core.Name "constraints"
_Language_supportedFeatures = Core.Name "supportedFeatures"
_Language_caseConventions = Core.Name "caseConventions"
_Language_defaultFileExtension = Core.Name "defaultFileExtension"
-- | A set of constraints on valid type and term expressions, characterizing a language
data LanguageConstraints =
  LanguageConstraints {
    -- | All supported literal variants
    languageConstraintsLiteralVariants :: (S.Set Variants.LiteralVariant),
    -- | All supported float types
    languageConstraintsFloatTypes :: (S.Set Core.FloatType),
    -- | All supported integer types
    languageConstraintsIntegerTypes :: (S.Set Core.IntegerType),
    -- | All supported term variants
    languageConstraintsTermVariants :: (S.Set Variants.TermVariant),
    -- | All supported type variants
    languageConstraintsTypeVariants :: (S.Set Variants.TypeVariant),
    -- | A logical set of types, as a predicate which tests a type for inclusion
    languageConstraintsTypes :: (Core.Type -> Bool)}
_LanguageConstraints = Core.Name "hydra.coders.LanguageConstraints"
_LanguageConstraints_literalVariants = Core.Name "literalVariants"
_LanguageConstraints_floatTypes = Core.Name "floatTypes"
_LanguageConstraints_integerTypes = Core.Name "integerTypes"
_LanguageConstraints_termVariants = Core.Name "termVariants"
_LanguageConstraints_typeVariants = Core.Name "typeVariants"
_LanguageConstraints_types = Core.Name "types"
-- | A target-language or target-runtime capability the coder may rely on. Absence from a Language's supportedFeatures set means the emitter must work around the missing capability.
data LanguageFeature =
  -- | The target runtime can invoke an n-ary function with fewer than n arguments without error. When absent, the emitter eta-expands partially-applied terms.
  LanguageFeaturePartialApplication |
  -- | The target runtime can handle deeply nested case statements without stack issues. When absent, the emitter hoists cases out into top-level helpers.
  LanguageFeatureNestedCaseStatements |
  -- | The target language permits polymorphic let-bindings in expression position. When absent, the emitter hoists polymorphic lets to top level.
  LanguageFeatureNestedPolymorphicLetBindings
  deriving (Eq, Ord, Read, Show)
_LanguageFeature = Core.Name "hydra.coders.LanguageFeature"
_LanguageFeature_partialApplication = Core.Name "partialApplication"
_LanguageFeature_nestedCaseStatements = Core.Name "nestedCaseStatements"
_LanguageFeature_nestedPolymorphicLetBindings = Core.Name "nestedPolymorphicLetBindings"
-- | The unique name of a language
newtype LanguageName =
  LanguageName {
    unLanguageName :: String}
  deriving (Eq, Ord, Read, Show)
_LanguageName = Core.Name "hydra.coders.LanguageName"
-- | A bidirectional encoder which maps between the same type and term languages on either side
type SymmetricAdapter t v = (Adapter t t v v)
_SymmetricAdapter = Core.Name "hydra.coders.SymmetricAdapter"
-- | Specifies either a pre-order or post-order traversal
data TraversalOrder =
  -- | Pre-order traversal
  TraversalOrderPre |
  -- | Post-order traversal
  TraversalOrderPost
  deriving (Eq, Ord, Read, Show)
_TraversalOrder = Core.Name "hydra.coders.TraversalOrder"
_TraversalOrder_pre = Core.Name "pre"
_TraversalOrder_post = Core.Name "post"
-- | A function which maps a Hydra type to a symmetric adapter between types and terms
type TypeAdapter = (AdapterContext -> Core.Type -> Either String (SymmetricAdapter Core.Type Core.Term))
_TypeAdapter = Core.Name "hydra.coders.TypeAdapter"
