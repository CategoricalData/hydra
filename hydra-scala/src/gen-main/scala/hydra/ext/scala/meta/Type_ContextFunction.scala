package hydra.ext.scala.meta

case class Type_ContextFunction(
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    params: Seq[Type],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    res: Type
)

val _Type_ContextFunction: String = "hydra/ext/scala/meta.Type_ContextFunction"
val _Type_ContextFunction_params: String = "params"
val _Type_ContextFunction_res: String = "res"
