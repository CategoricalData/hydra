package hydra.ext.scala.meta

case class Type_Method(
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term.Param
     */
    paramss: Seq[Seq[Term_Param]],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type
)

val _Type_Method: String = "hydra/ext/scala/meta.Type_Method"
val _Type_Method_paramss: String = "paramss"
val _Type_Method_tpe: String = "tpe"
