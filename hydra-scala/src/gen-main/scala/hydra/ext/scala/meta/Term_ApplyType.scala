package hydra.ext.scala.meta

case class Term_ApplyType(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    lhs: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    op: Term_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    targs: Seq[Type],
    
    /**
     * @type list: hydra/ext/scala/meta.Term
     */
    args: Seq[Term]
)

val _Term_ApplyType: String = "hydra/ext/scala/meta.Term_ApplyType"
val _Term_ApplyType_args: String = "args"
val _Term_ApplyType_lhs: String = "lhs"
val _Term_ApplyType_op: String = "op"
val _Term_ApplyType_targs: String = "targs"
