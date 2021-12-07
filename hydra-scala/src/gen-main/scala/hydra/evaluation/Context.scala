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
    strategy: hydra.evaluation.EvaluationStrategy,
    
    /**
     * @type function:
     *         from:
     *         - variable: a
     *         to:
     *           optional: string
     */
    descriptionOf: a => Option[String],
    
    /**
     * @type function:
     *         from:
     *         - variable: a
     *         to:
     *           optional: hydra/core.Type
     */
    typeOf: a => Option[hydra.core.Type],
    
    /**
     * @type function:
     *         from:
     *         - optional: hydra/core.Type
     *         - variable: a
     *         to:
     *           variable: a
     */
    setTypeOf: Option[hydra.core.Type] => a => a
)

val _Context: String = "hydra/evaluation.Context"
val _Context_descriptionOf: String = "descriptionOf"
val _Context_elements: String = "elements"
val _Context_functions: String = "functions"
val _Context_graphs: String = "graphs"
val _Context_setTypeOf: String = "setTypeOf"
val _Context_strategy: String = "strategy"
val _Context_typeOf: String = "typeOf"
