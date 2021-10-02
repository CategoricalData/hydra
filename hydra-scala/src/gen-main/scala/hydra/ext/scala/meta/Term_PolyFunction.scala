package hydra.ext.scala.meta

case class Term_PolyFunction(
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Term_PolyFunction: String = "hydra/ext/scala/meta.Term_PolyFunction"
val _Term_PolyFunction_body: String = "body"
val _Term_PolyFunction_tparams: String = "tparams"
