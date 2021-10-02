package hydra.ext.scala.meta

case class Term_Function(
    /**
     * @type list: hydra/ext/scala/meta.Term.Param
     */
    params: Seq[Term_Param],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Term_Function: String = "hydra/ext/scala/meta.Term_Function"
val _Term_Function_body: String = "body"
val _Term_Function_params: String = "params"
