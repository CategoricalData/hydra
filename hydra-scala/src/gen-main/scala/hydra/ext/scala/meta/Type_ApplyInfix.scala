package hydra.ext.scala.meta

case class Type_ApplyInfix(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    lhs: Type,
    
    /**
     * @type hydra/ext/scala/meta.Type.Name
     */
    op: Type_Name,
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    rhs: Type
)

val _Type_ApplyInfix: String = "hydra/ext/scala/meta.Type_ApplyInfix"
val _Type_ApplyInfix_lhs: String = "lhs"
val _Type_ApplyInfix_op: String = "op"
val _Type_ApplyInfix_rhs: String = "rhs"
