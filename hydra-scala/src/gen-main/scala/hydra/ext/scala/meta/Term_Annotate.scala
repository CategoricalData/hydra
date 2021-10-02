package hydra.ext.scala.meta

case class Term_Annotate(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Mod.Annot
     */
    annots: Seq[Mod_Annot]
)

val _Term_Annotate: String = "hydra/ext/scala/meta.Term_Annotate"
val _Term_Annotate_annots: String = "annots"
val _Term_Annotate_expr: String = "expr"
