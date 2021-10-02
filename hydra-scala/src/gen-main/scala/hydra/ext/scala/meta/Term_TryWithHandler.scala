package hydra.ext.scala.meta

case class Term_TryWithHandler(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    catchp: Term,
    
    /**
     * @type optional: hydra/ext/scala/meta.Term
     */
    finallyp: Option[Term]
)

val _Term_TryWithHandler: String = "hydra/ext/scala/meta.Term_TryWithHandler"
val _Term_TryWithHandler_catchp: String = "catchp"
val _Term_TryWithHandler_expr: String = "expr"
val _Term_TryWithHandler_finallyp: String = "finallyp"
