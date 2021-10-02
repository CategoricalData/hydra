package hydra.ext.scala.meta

case class Pat_Extract(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    fun: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Pat
     */
    args: Seq[Pat]
)

val _Pat_Extract: String = "hydra/ext/scala/meta.Pat_Extract"
val _Pat_Extract_args: String = "args"
val _Pat_Extract_fun: String = "fun"
