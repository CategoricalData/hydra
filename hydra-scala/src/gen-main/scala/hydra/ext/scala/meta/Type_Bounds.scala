package hydra.ext.scala.meta

case class Type_Bounds(
    /**
     * @type optional: hydra/ext/scala/meta.Type
     */
    lo: Option[Type],
    
    /**
     * @type optional: hydra/ext/scala/meta.Type
     */
    hi: Option[Type]
)

val _Type_Bounds: String = "hydra/ext/scala/meta.Type_Bounds"
val _Type_Bounds_hi: String = "hi"
val _Type_Bounds_lo: String = "lo"
