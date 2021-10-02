package hydra.ext.scala.meta

case class Term_Try(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Case
     */
    catchp: Seq[Case],
    
    /**
     * @type optional: hydra/ext/scala/meta.Term
     */
    finallyp: Option[Term]
)

val _Term_Try: String = "hydra/ext/scala/meta.Term_Try"
val _Term_Try_catchp: String = "catchp"
val _Term_Try_expr: String = "expr"
val _Term_Try_finallyp: String = "finallyp"
