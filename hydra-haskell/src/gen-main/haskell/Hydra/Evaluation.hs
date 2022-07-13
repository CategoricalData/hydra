module Hydra.Evaluation where

import qualified Hydra.Core as Core
import qualified Hydra.Graph as Graph
import Data.Map
import Data.Set

-- A pointed set of graph modules; a graph in the logical sense
data Context m 
  = Context {
    contextGraphs :: (Graph.GraphSet m),
    contextElements :: (Map Core.Name (Graph.Element m)),
    contextFunctions :: (Map Core.Name (PrimitiveFunction m)),
    contextStrategy :: EvaluationStrategy,
    contextAnnotations :: (AnnotationClass m)}

_Context = (Core.Name "hydra/evaluation.Context")

_Context_graphs = (Core.FieldName "graphs")

_Context_elements = (Core.FieldName "elements")

_Context_functions = (Core.FieldName "functions")

_Context_strategy = (Core.FieldName "strategy")

_Context_annotations = (Core.FieldName "annotations")

-- A typeclass-like construct providing common functions for working with annotations
data AnnotationClass m 
  = AnnotationClass {
    annotationClassDefault :: m,
    annotationClassEqual :: (m -> m -> Bool),
    annotationClassCompare :: (m -> m -> Core.Comparison),
    annotationClassShow :: (m -> String),
    annotationClassRead :: (String -> Maybe m),
    annotationClassTermDescription :: Context m -> (Core.Term m -> Result (Maybe String)),
    annotationClassTypeDescription :: Context m -> (Core.Type m -> Result (Maybe String)),
    annotationClassTermType :: Context m -> (Core.Term m -> Result (Maybe (Core.Type m))),
    annotationClassSetTermDescription :: Context m -> (Maybe String -> Core.Term m -> Core.Term m),
    annotationClassSetTermType :: Context m -> (Maybe (Core.Type m) -> Core.Term m -> Core.Term m),
    annotationClassTypeOf :: Context m -> (m -> Result (Maybe (Core.Type m))),
    annotationClassSetTypeOf :: Context m -> (Maybe (Core.Type m) -> m -> m)}

_AnnotationClass = (Core.Name "hydra/evaluation.AnnotationClass")

_AnnotationClass_default = (Core.FieldName "default")

_AnnotationClass_equal = (Core.FieldName "equal")

_AnnotationClass_compare = (Core.FieldName "compare")

_AnnotationClass_show = (Core.FieldName "show")

_AnnotationClass_read = (Core.FieldName "read")

_AnnotationClass_termDescription = (Core.FieldName "termDescription")

_AnnotationClass_typeDescription = (Core.FieldName "typeDescription")

_AnnotationClass_termType = (Core.FieldName "termType")

_AnnotationClass_setTermDescription = (Core.FieldName "setTermDescription")

_AnnotationClass_setTermType = (Core.FieldName "setTermType")

_AnnotationClass_typeOf = (Core.FieldName "typeOf")

_AnnotationClass_setTypeOf = (Core.FieldName "setTypeOf")

-- Settings which determine how terms are evaluated
data EvaluationStrategy 
  = EvaluationStrategy {
    evaluationStrategyOpaqueTermVariants :: (Set Core.TermVariant)}
  deriving (Eq, Ord, Read, Show)

_EvaluationStrategy = (Core.Name "hydra/evaluation.EvaluationStrategy")

_EvaluationStrategy_opaqueTermVariants = (Core.FieldName "opaqueTermVariants")

-- A helper object for specifying and unmarshalling an argument to a primitive function
data InputSpec a m 
  = InputSpec {
    inputSpecType :: (Core.Type m),
    inputSpecUnmarshal :: (Core.Term m -> Result a)}

_InputSpec = (Core.Name "hydra/evaluation.InputSpec")

_InputSpec_type = (Core.FieldName "type")

_InputSpec_unmarshal = (Core.FieldName "unmarshal")

-- A helper object for specifying and marshalling the output of a primitive function
data OutputSpec a m 
  = OutputSpec {
    outputSpecType :: (Core.Type m),
    outputSpecMarshal :: (a -> Core.Term m)}

_OutputSpec = (Core.Name "hydra/evaluation.OutputSpec")

_OutputSpec_type = (Core.FieldName "type")

_OutputSpec_marshal = (Core.FieldName "marshal")

-- A built-in function
data PrimitiveFunction m 
  = PrimitiveFunction {
    primitiveFunctionName :: Core.Name,
    primitiveFunctionType :: (Core.FunctionType m),
    primitiveFunctionImplementation :: ([Core.Term m] -> Result (Core.Term m))}

_PrimitiveFunction = (Core.Name "hydra/evaluation.PrimitiveFunction")

_PrimitiveFunction_name = (Core.FieldName "name")

_PrimitiveFunction_type = (Core.FieldName "type")

_PrimitiveFunction_implementation = (Core.FieldName "implementation")

-- A qualified result; success with a value or failure with an error message
data Result m 
  = ResultSuccess m
  | ResultFailure String
  deriving (Eq, Ord, Read, Show)

_Result = (Core.Name "hydra/evaluation.Result")

_Result_success = (Core.FieldName "success")

_Result_failure = (Core.FieldName "failure")

-- A qualified bidirectional transformation
data Step a b 
  = Step {
    stepOut :: (a -> Result b),
    stepIn :: (b -> Result a)}

_Step = (Core.Name "hydra/evaluation.Step")

_Step_out = (Core.FieldName "out")

_Step_in = (Core.FieldName "in")

-- Indicates either the 'out' or the 'in' direction of a step
data StepDirection 
  = StepDirectionOut 
  | StepDirectionIn 
  deriving (Eq, Ord, Read, Show)

_StepDirection = (Core.Name "hydra/evaluation.StepDirection")

_StepDirection_out = (Core.FieldName "out")

_StepDirection_in = (Core.FieldName "in")