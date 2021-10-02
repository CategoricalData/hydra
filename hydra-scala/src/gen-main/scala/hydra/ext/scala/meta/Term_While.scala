package hydra.ext.scala.meta

case class Term_While(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Term_While: String = "hydra/ext/scala/meta.Term_While"
val _Term_While_body: String = "body"
val _Term_While_expr: String = "expr"
