package hydra.ext.scala.meta

case class Term_Xml(
    /**
     * @type list: hydra/ext/scala/meta.Lit
     */
    parts: Seq[Lit],
    
    /**
     * @type list: hydra/ext/scala/meta.Term
     */
    args: Seq[Term]
)

val _Term_Xml: String = "hydra/ext/scala/meta.Term_Xml"
val _Term_Xml_args: String = "args"
val _Term_Xml_parts: String = "parts"
