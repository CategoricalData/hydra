package hydra.ext.scala.meta

case class Term_Interpolate(
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    prefix: Term_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Lit
     */
    parts: Seq[Lit],
    
    /**
     * @type list: hydra/ext/scala/meta.Term
     */
    args: Seq[Term]
)

val _Term_Interpolate: String = "hydra/ext/scala/meta.Term_Interpolate"
val _Term_Interpolate_args: String = "args"
val _Term_Interpolate_parts: String = "parts"
val _Term_Interpolate_prefix: String = "prefix"
