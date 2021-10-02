package hydra.ext.scala.meta

case class Type_Function(
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    params: Seq[Type],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    res: Type
)

val _Type_Function: String = "hydra/ext/scala/meta.Type_Function"
val _Type_Function_params: String = "params"
val _Type_Function_res: String = "res"
