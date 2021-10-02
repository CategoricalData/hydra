package hydra.ext.scala.meta

case class Term_If(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    cond: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    thenp: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    elsep: Term
)

val _Term_If: String = "hydra/ext/scala/meta.Term_If"
val _Term_If_cond: String = "cond"
val _Term_If_elsep: String = "elsep"
val _Term_If_thenp: String = "thenp"
