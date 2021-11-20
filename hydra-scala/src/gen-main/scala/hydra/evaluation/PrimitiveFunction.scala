package hydra.evaluation

case class PrimitiveFunction[a] (
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
     *         - list:
     *             parameterized:
     *               genericType: hydra/core.Term
     *               parameters:
     *               - type:
     *                   variable: a
     *                 variable: a
     *         to:
     *           parameterized:
     *             genericType: hydra/evaluation.Result
     *             parameters:
     *             - type:
     *                 parameterized:
     *                   genericType: hydra/core.Term
     *                   parameters:
     *                   - type:
     *                       variable: a
     *                     variable: a
     *               variable: a
     */
    implementation: Seq[hydra.core.Term[a]] => hydra.evaluation.Result[hydra.core.Term[a]]
)

val _PrimitiveFunction: String = "hydra/evaluation.PrimitiveFunction"
val _PrimitiveFunction_implementation: String = "implementation"
val _PrimitiveFunction_name: String = "name"
val _PrimitiveFunction_type: String = "type"
