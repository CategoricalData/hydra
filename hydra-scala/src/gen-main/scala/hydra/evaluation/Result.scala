package hydra.evaluation

enum Result[a]:
    /**
     * @type variable: a
     */
    case success(value: a) extends Result[a]
    /**
     * @type string
     */
    case failure(value: String) extends Result[a]

val _Result: String = "hydra/evaluation.Result"
val _Result_failure: String = "failure"
val _Result_success: String = "success"
