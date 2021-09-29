package hydra.evaluation

import hydra.core.Name
import hydra.graph.Element

case class Context(
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values: hydra/graph.Element
     */
    elements: Map[Name, Element],
    
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values: hydra/evaluation.PrimitiveFunction
     */
    functions: Map[Name, PrimitiveFunction],
    
    /**
     * @type hydra/evaluation.EvaluationStrategy
     */
    strategy: EvaluationStrategy
)

val _Context: String = "hydra/evaluation.Context"
val _Context_elements: String = "elements"
val _Context_functions: String = "functions"
val _Context_strategy: String = "strategy"
