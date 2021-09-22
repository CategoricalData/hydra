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
     * @type hydra/evaluation.ReductionStyle
     */
    reductionStyle: ReductionStyle
)
