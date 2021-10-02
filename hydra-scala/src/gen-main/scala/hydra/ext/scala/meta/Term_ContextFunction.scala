package hydra.ext.scala.meta

case class Term_ContextFunction(
    /**
     * @type list: hydra/ext/scala/meta.Term.Param
     */
    params: Seq[Term_Param],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Term_ContextFunction: String = "hydra/ext/scala/meta.Term_ContextFunction"
val _Term_ContextFunction_body: String = "body"
val _Term_ContextFunction_params: String = "params"
