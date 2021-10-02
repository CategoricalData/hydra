package hydra.ext.scala.meta

case class Type_ImplicitFunction(
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    params: Seq[Type],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    res: Type
)

val _Type_ImplicitFunction: String = "hydra/ext/scala/meta.Type_ImplicitFunction"
val _Type_ImplicitFunction_params: String = "params"
val _Type_ImplicitFunction_res: String = "res"
