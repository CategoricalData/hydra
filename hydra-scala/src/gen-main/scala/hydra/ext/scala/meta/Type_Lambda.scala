package hydra.ext.scala.meta

case class Type_Lambda(
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type
)

val _Type_Lambda: String = "hydra/ext/scala/meta.Type_Lambda"
val _Type_Lambda_tparams: String = "tparams"
val _Type_Lambda_tpe: String = "tpe"
