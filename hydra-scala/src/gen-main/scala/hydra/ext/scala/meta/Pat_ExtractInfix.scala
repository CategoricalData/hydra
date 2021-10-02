package hydra.ext.scala.meta

case class Pat_ExtractInfix(
    /**
     * @type hydra/ext/scala/meta.Pat
     */
    lhs: Pat,
    
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    op: Term_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Pat
     */
    rhs: Seq[Pat]
)

val _Pat_ExtractInfix: String = "hydra/ext/scala/meta.Pat_ExtractInfix"
val _Pat_ExtractInfix_lhs: String = "lhs"
val _Pat_ExtractInfix_op: String = "op"
val _Pat_ExtractInfix_rhs: String = "rhs"
