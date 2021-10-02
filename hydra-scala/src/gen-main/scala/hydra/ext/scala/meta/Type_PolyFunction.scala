package hydra.ext.scala.meta

case class Type_PolyFunction(
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type
)

val _Type_PolyFunction: String = "hydra/ext/scala/meta.Type_PolyFunction"
val _Type_PolyFunction_tparams: String = "tparams"
val _Type_PolyFunction_tpe: String = "tpe"
