package hydra.evaluation

case class Context[a] (
    /**
     * @type parameterized:
     *         genericType: hydra/graph.GraphSet
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    graphs: hydra.graph.GraphSet[a],
    
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values:
     *           parameterized:
     *             genericType: hydra/graph.Element
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     */
    elements: Map[hydra.core.Name, hydra.graph.Element[a]],
    
    /**
     * @type map:
     *         keys: hydra/core.Name
     *         values:
     *           parameterized:
     *             genericType: hydra/evaluation.PrimitiveFunction
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     */
    functions: Map[hydra.core.Name, hydra.evaluation.PrimitiveFunction[a]],
    
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
