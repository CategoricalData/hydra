package hydra.evaluation

import hydra.core.Name
import hydra.graph.Element
import hydra.graph.GraphSet

case class Context(
    /**
     * @type hydra/graph.GraphSet
     */
    graphs: GraphSet,
    
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
val _Context_graphs: String = "graphs"
val _Context_strategy: String = "strategy"
