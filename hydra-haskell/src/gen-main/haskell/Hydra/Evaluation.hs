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
    contextDescriptionOf :: (m -> Result (Maybe String)),
    contextTypeOf :: (m -> Result (Maybe (Core.Type m))),
    contextSetDescriptionOf :: (Maybe String -> m -> m),
    contextSetTypeOf :: (Maybe (Core.Type m) -> m -> m)}

_Context = (Core.Name "hydra/evaluation.Context")

_Context_graphs = (Core.FieldName "graphs")

_Context_elements = (Core.FieldName "elements")

_Context_functions = (Core.FieldName "functions")

_Context_strategy = (Core.FieldName "strategy")

_Context_descriptionOf = (Core.FieldName "descriptionOf")

_Context_typeOf = (Core.FieldName "typeOf")

_Context_setDescriptionOf = (Core.FieldName "setDescriptionOf")

_Context_setTypeOf = (Core.FieldName "setTypeOf")

-- Settings which determine how terms are evaluated
data EvaluationStrategy 
  = EvaluationStrategy {
    evaluationStrategyOpaqueDataVariants :: (Set Core.DataVariant)}
  deriving (Eq, Ord, Read, Show)

_EvaluationStrategy = (Core.Name "hydra/evaluation.EvaluationStrategy")

_EvaluationStrategy_opaqueDataVariants = (Core.FieldName "opaqueDataVariants")

-- A helper object for specifying and unmarshalling an argument to a primitive function
data InputSpec a m 
  = InputSpec {
    inputSpecType :: (Core.Type m),
    inputSpecUnmarshal :: (Core.Data m -> Result a)}

_InputSpec = (Core.Name "hydra/evaluation.InputSpec")

_InputSpec_type = (Core.FieldName "type")

_InputSpec_unmarshal = (Core.FieldName "unmarshal")

-- A helper object for specifying and marshalling the output of a primitive function
data OutputSpec a m 
  = OutputSpec {
    outputSpecType :: (Core.Type m),
    outputSpecMarshal :: (a -> Core.Data m)}

_OutputSpec = (Core.Name "hydra/evaluation.OutputSpec")

_OutputSpec_type = (Core.FieldName "type")

_OutputSpec_marshal = (Core.FieldName "marshal")

-- A built-in function
data PrimitiveFunction m 
  = PrimitiveFunction {
    primitiveFunctionName :: Core.Name,
    primitiveFunctionType :: (Core.FunctionType m),
    primitiveFunctionImplementation :: ([Core.Data m] -> Result (Core.Data m))}

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