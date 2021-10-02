package hydra.ext.scala.meta

case class Term_Ascribe(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type
)

val _Term_Ascribe: String = "hydra/ext/scala/meta.Term_Ascribe"
val _Term_Ascribe_expr: String = "expr"
val _Term_Ascribe_tpe: String = "tpe"
