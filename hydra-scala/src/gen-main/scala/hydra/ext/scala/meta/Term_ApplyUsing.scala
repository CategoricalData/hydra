package hydra.ext.scala.meta

case class Term_ApplyUsing(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    fun: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    targs: Seq[Type]
)

val _Term_ApplyUsing: String = "hydra/ext/scala/meta.Term_ApplyUsing"
val _Term_ApplyUsing_fun: String = "fun"
val _Term_ApplyUsing_targs: String = "targs"
