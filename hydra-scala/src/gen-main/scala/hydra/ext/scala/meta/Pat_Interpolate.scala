package hydra.ext.scala.meta

case class Pat_Interpolate(
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    prefix: Term_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Lit
     */
    parts: Seq[Lit]
)

val _Pat_Interpolate: String = "hydra/ext/scala/meta.Pat_Interpolate"
val _Pat_Interpolate_parts: String = "parts"
val _Pat_Interpolate_prefix: String = "prefix"
