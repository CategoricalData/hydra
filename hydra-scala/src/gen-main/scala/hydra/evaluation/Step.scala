package hydra.evaluation

case class Step[a, b] (
    /**
     * @type function:
     *         from:
     *         - variable: a
     *         to:
     *           parameterized:
     *             genericType: hydra/evaluation.Result
     *             parameters:
     *             - type:
     *                 variable: b
     *               variable: a
     */
    out: a => hydra.evaluation.Result[b],
    
    /**
     * @type function:
     *         from:
     *         - variable: b
     *         to:
     *           parameterized:
     *             genericType: hydra/evaluation.Result
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     */
    in: b => hydra.evaluation.Result[a]
)

val _Step: String = "hydra/evaluation.Step"
val _Step_in: String = "in"
val _Step_out: String = "out"
