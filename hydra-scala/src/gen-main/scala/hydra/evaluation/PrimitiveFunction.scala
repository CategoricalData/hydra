package hydra.evaluation

import hydra.core.FunctionType
import hydra.core.Name
import hydra.core.Term

case class PrimitiveFunction(
    /**
     * @type hydra/core.Name
     */
    name: Name,
    
    /**
     * @type hydra/core.FunctionType
     */
    `type`: FunctionType,
    
    /**
     * @type function:
     *         from:
     *         - list: hydra/core.Term
     *         to:
     *           parameterized:
     *             genericType: hydra/evaluation.Result
     *             parameters:
     *             - type: hydra/core.Term
     *               variable: a
     */
    implementation: Seq[Term] => Result[Term]
)

val _PrimitiveFunction: String = "hydra/evaluation.PrimitiveFunction"
val _PrimitiveFunction_implementation: String = "implementation"
val _PrimitiveFunction_name: String = "name"
val _PrimitiveFunction_type: String = "type"
