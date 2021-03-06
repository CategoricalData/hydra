package hydra.ext.scala.meta

case class Case (
    /**
     * @type hydra/ext/scala/meta.Pat
     */
    pat: hydra.ext.scala.meta.Pat,
    
    /**
     * @type optional: hydra/ext/scala/meta.Term
     */
    cond: Option[hydra.ext.scala.meta.Term],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: hydra.ext.scala.meta.Term
)

val _Case: String = "hydra/ext/scala/meta.Case"
val _Case_body: String = "body"
val _Case_cond: String = "cond"
val _Case_pat: String = "pat"
