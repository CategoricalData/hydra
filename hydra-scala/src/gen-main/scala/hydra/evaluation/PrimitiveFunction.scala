package hydra.evaluation

case class PrimitiveFunction (
    /**
     * @type hydra/core.Name
     */
    name: hydra.core.Name,
    
    /**
     * @type hydra/core.FunctionType
     */
    `type`: hydra.core.FunctionType,
    
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
    implementation: Seq[hydra.core.Term] => hydra.evaluation.Result[hydra.core.Term]
)

val _PrimitiveFunction: String = "hydra/evaluation.PrimitiveFunction"
val _PrimitiveFunction_implementation: String = "implementation"
val _PrimitiveFunction_name: String = "name"
val _PrimitiveFunction_type: String = "type"
