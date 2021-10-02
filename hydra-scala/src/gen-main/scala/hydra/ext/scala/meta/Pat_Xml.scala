package hydra.ext.scala.meta

case class Pat_Xml(
    /**
     * @type list: hydra/ext/scala/meta.Lit
     */
    parts: Seq[Lit],
    
    /**
     * @type list: hydra/ext/scala/meta.Pat
     */
    args: Seq[Pat]
)

val _Pat_Xml: String = "hydra/ext/scala/meta.Pat_Xml"
val _Pat_Xml_args: String = "args"
val _Pat_Xml_parts: String = "parts"
