package hydra.ext.scala.meta

case class Type_Apply(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type,
    
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    args: Seq[Type]
)

val _Type_Apply: String = "hydra/ext/scala/meta.Type_Apply"
val _Type_Apply_args: String = "args"
val _Type_Apply_tpe: String = "tpe"
