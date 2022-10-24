-- | Abstractions for evaluation and transformations

module Hydra.Compute where

import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import Data.List
import Data.Map
import Data.Set

data Adapter s1 s2 t1 t2 v1 v2 = 
  Adapter {
    adapterIsLossy :: Bool,
    adapterSource :: t1,
    adapterTarget :: t2,
    adapterCoder :: (Coder s1 s2 v1 v2)}

_Adapter = (Core.Name "hydra/compute.Adapter")

_Adapter_isLossy = (Core.FieldName "isLossy")

_Adapter_source = (Core.FieldName "source")

_Adapter_target = (Core.FieldName "target")

_Adapter_coder = (Core.FieldName "coder")

data AdapterContext m = 
  AdapterContext {
    adapterContextEvaluation :: (Context m),
    adapterContextSource :: (Language m),
    adapterContextTarget :: (Language m)}

_AdapterContext = (Core.Name "hydra/compute.AdapterContext")

_AdapterContext_evaluation = (Core.FieldName "evaluation")

_AdapterContext_source = (Core.FieldName "source")

_AdapterContext_target = (Core.FieldName "target")

-- | A typeclass-like construct providing common functions for working with annotations
data AnnotationClass m = 
  AnnotationClass {
    annotationClassDefault :: m,
    annotationClassEqual :: (m -> m -> Bool),
    annotationClassCompare :: (m -> m -> Mantle.Comparison),
    annotationClassShow :: (m -> String),
    annotationClassRead :: (String -> Maybe m),
    annotationClassTermMeta :: (Core.Term m -> m),
    annotationClassTypeMeta :: (Core.Type m -> m),
    annotationClassTermDescription :: (Core.Term m -> Flow (Context m) (Maybe String)),
    annotationClassTypeDescription :: (Core.Type m -> Flow (Context m) (Maybe String)),
    annotationClassTermType :: (Core.Term m -> Flow (Context m) (Maybe (Core.Type m))),
    annotationClassSetTermDescription :: (Context m -> Maybe String -> Core.Term m -> Core.Term m),
    annotationClassSetTermType :: (Context m -> Maybe (Core.Type m) -> Core.Term m -> Core.Term m),
    annotationClassTypeOf :: (m -> Flow (Context m) (Maybe (Core.Type m))),
    annotationClassSetTypeOf :: (Maybe (Core.Type m) -> m -> m)}

_AnnotationClass = (Core.Name "hydra/compute.AnnotationClass")

_AnnotationClass_default = (Core.FieldName "default")

_AnnotationClass_equal = (Core.FieldName "equal")

_AnnotationClass_compare = (Core.FieldName "compare")

_AnnotationClass_show = (Core.FieldName "show")

_AnnotationClass_read = (Core.FieldName "read")

_AnnotationClass_termMeta = (Core.FieldName "termMeta")

_AnnotationClass_typeMeta = (Core.FieldName "typeMeta")

_AnnotationClass_termDescription = (Core.FieldName "termDescription")

_AnnotationClass_typeDescription = (Core.FieldName "typeDescription")

_AnnotationClass_termType = (Core.FieldName "termType")

_AnnotationClass_setTermDescription = (Core.FieldName "setTermDescription")

_AnnotationClass_setTermType = (Core.FieldName "setTermType")

_AnnotationClass_typeOf = (Core.FieldName "typeOf")

_AnnotationClass_setTypeOf = (Core.FieldName "setTypeOf")

-- | An encoder and decoder; a bidirectional flow between two types
data Coder s1 s2 v1 v2 = 
  Coder {
    coderEncode :: (v1 -> Flow s1 v2),
    coderDecode :: (v2 -> Flow s2 v1)}

_Coder = (Core.Name "hydra/compute.Coder")

_Coder_encode = (Core.FieldName "encode")

_Coder_decode = (Core.FieldName "decode")

-- | Indicates either the 'out' or the 'in' direction of a coder
data CoderDirection = 
  CoderDirectionEncode  |
  CoderDirectionDecode 
  deriving (Eq, Ord, Read, Show)

_CoderDirection = (Core.Name "hydra/compute.CoderDirection")

_CoderDirection_encode = (Core.FieldName "encode")

_CoderDirection_decode = (Core.FieldName "decode")

-- | An environment containing a graph together with primitive functions and other necessary components for evaluation
data Context m = 
  Context {
    contextGraph :: (Mantle.Graph m),
    contextFunctions :: (Map Core.Name (PrimitiveFunction m)),
    contextStrategy :: EvaluationStrategy,
    contextAnnotations :: (AnnotationClass m)}

_Context = (Core.Name "hydra/compute.Context")

_Context_graph = (Core.FieldName "graph")

_Context_functions = (Core.FieldName "functions")

_Context_strategy = (Core.FieldName "strategy")

_Context_annotations = (Core.FieldName "annotations")

-- | Settings which determine how terms are evaluated
data EvaluationStrategy = 
  EvaluationStrategy {
    evaluationStrategyOpaqueTermVariants :: (Set Mantle.TermVariant)}
  deriving (Eq, Ord, Read, Show)

_EvaluationStrategy = (Core.Name "hydra/compute.EvaluationStrategy")

_EvaluationStrategy_opaqueTermVariants = (Core.FieldName "opaqueTermVariants")

-- | A variant of the State monad with built-in logging and error handling
newtype Flow s a = 
  Flow {
    unFlow :: (s -> Trace -> FlowWrapper s a)}

_Flow = (Core.Name "hydra/compute.Flow")

data FlowWrapper s a = 
  FlowWrapper {
    flowWrapperValue :: (Maybe a),
    flowWrapperState :: s,
    flowWrapperTrace :: Trace}
  deriving (Eq, Ord, Read, Show)

_FlowWrapper = (Core.Name "hydra/compute.FlowWrapper")

_FlowWrapper_value = (Core.FieldName "value")

_FlowWrapper_state = (Core.FieldName "state")

_FlowWrapper_trace = (Core.FieldName "trace")

data Language m = 
  Language {
    languageName :: LanguageName,
    languageConstraints :: (LanguageConstraints m)}

_Language = (Core.Name "hydra/compute.Language")

_Language_name = (Core.FieldName "name")

_Language_constraints = (Core.FieldName "constraints")

data LanguageConstraints m = 
  LanguageConstraints {
    languageConstraintsEliminationVariants :: (Set Mantle.EliminationVariant),
    languageConstraintsLiteralVariants :: (Set Mantle.LiteralVariant),
    languageConstraintsFloatTypes :: (Set Core.FloatType),
    languageConstraintsFunctionVariants :: (Set Mantle.FunctionVariant),
    languageConstraintsIntegerTypes :: (Set Core.IntegerType),
    languageConstraintsTermVariants :: (Set Mantle.TermVariant),
    languageConstraintsTypeVariants :: (Set Mantle.TypeVariant),
    languageConstraintsTypes :: (Core.Type m -> Bool)}

_LanguageConstraints = (Core.Name "hydra/compute.LanguageConstraints")

_LanguageConstraints_eliminationVariants = (Core.FieldName "eliminationVariants")

_LanguageConstraints_literalVariants = (Core.FieldName "literalVariants")

_LanguageConstraints_floatTypes = (Core.FieldName "floatTypes")

_LanguageConstraints_functionVariants = (Core.FieldName "functionVariants")

_LanguageConstraints_integerTypes = (Core.FieldName "integerTypes")

_LanguageConstraints_termVariants = (Core.FieldName "termVariants")

_LanguageConstraints_typeVariants = (Core.FieldName "typeVariants")

_LanguageConstraints_types = (Core.FieldName "types")

newtype LanguageName = 
  LanguageName {
    unLanguageName :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageName = (Core.Name "hydra/compute.LanguageName")

-- | A built-in metadata container for terms
data Meta = 
  Meta {
    -- | A map of annotation names to annotation values
    metaAnnotations :: (Map String (Core.Term Meta))}
  deriving (Eq, Ord, Read, Show)

_Meta = (Core.Name "hydra/compute.Meta")

_Meta_annotations = (Core.FieldName "annotations")

-- | A built-in function
data PrimitiveFunction m = 
  PrimitiveFunction {
    primitiveFunctionName :: Core.Name,
    primitiveFunctionType :: (Core.FunctionType m),
    primitiveFunctionImplementation :: ([Core.Term m] -> Flow (Context m) (Core.Term m))}

_PrimitiveFunction = (Core.Name "hydra/compute.PrimitiveFunction")

_PrimitiveFunction_name = (Core.FieldName "name")

_PrimitiveFunction_type = (Core.FieldName "type")

_PrimitiveFunction_implementation = (Core.FieldName "implementation")

-- | A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms
data TermCoder m a = 
  TermCoder {
    termCoderType :: (Core.Type m),
    termCoderCoder :: (Coder (Context m) (Context m) (Core.Term m) a)}

_TermCoder = (Core.Name "hydra/compute.TermCoder")

_TermCoder_type = (Core.FieldName "type")

_TermCoder_coder = (Core.FieldName "coder")

-- | A container for logging and error information
data Trace = 
  Trace {
    traceStack :: [String],
    traceMessages :: [String],
    -- | A map of string keys to arbitrary terms as values, for application-specific use
    traceOther :: (Map String (Core.Term Meta))}
  deriving (Eq, Ord, Read, Show)

_Trace = (Core.Name "hydra/compute.Trace")

_Trace_stack = (Core.FieldName "stack")

_Trace_messages = (Core.FieldName "messages")

_Trace_other = (Core.FieldName "other")

data TraversalOrder = 
  -- | Pre-order traversal
  TraversalOrderPre  |
  -- | Post-order traversal
  TraversalOrderPost 
  deriving (Eq, Ord, Read, Show)

_TraversalOrder = (Core.Name "hydra/compute.TraversalOrder")

_TraversalOrder_pre = (Core.FieldName "pre")

_TraversalOrder_post = (Core.FieldName "post")