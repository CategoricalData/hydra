package hydra.ext.scala.meta

case class Term_ApplyInfix(
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

val _Term_ApplyInfix: String = "hydra/ext/scala/meta.Term_ApplyInfix"
val _Term_ApplyInfix_args: String = "args"
val _Term_ApplyInfix_lhs: String = "lhs"
val _Term_ApplyInfix_op: String = "op"
val _Term_ApplyInfix_targs: String = "targs"
