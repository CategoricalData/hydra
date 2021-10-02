package hydra.ext.scala.meta

case class Term_ApplyUnary(
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    op: Term_Name,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    arg: Term
)

val _Term_ApplyUnary: String = "hydra/ext/scala/meta.Term_ApplyUnary"
val _Term_ApplyUnary_arg: String = "arg"
val _Term_ApplyUnary_op: String = "op"
