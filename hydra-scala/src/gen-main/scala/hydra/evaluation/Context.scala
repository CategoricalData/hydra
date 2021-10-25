package hydra.evaluation

case class Context (
    /**
     * @type hydra/graph.GraphSet
     */
    graphs: hydra.graph.GraphSet,
    
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values: hydra/graph.Element
     */
    elements: Map[hydra.core.Name, hydra.graph.Element],
    
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values: hydra/evaluation.PrimitiveFunction
     */
    functions: Map[hydra.core.Name, hydra.evaluation.PrimitiveFunction],
    
    /**
     * @type hydra/evaluation.EvaluationStrategy
     */
    strategy: hydra.evaluation.EvaluationStrategy
)

val _Context: String = "hydra/evaluation.Context"
val _Context_elements: String = "elements"
val _Context_functions: String = "functions"
val _Context_graphs: String = "graphs"
val _Context_strategy: String = "strategy"
