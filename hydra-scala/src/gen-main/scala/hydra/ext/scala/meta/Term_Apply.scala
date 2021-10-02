package hydra.ext.scala.meta

case class Term_Apply(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    fun: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Term
     */
    args: Seq[Term]
)

val _Term_Apply: String = "hydra/ext/scala/meta.Term_Apply"
val _Term_Apply_args: String = "args"
val _Term_Apply_fun: String = "fun"
