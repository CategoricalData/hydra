package hydra.ext.scala.meta

case class Type_Annotate(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type,
    
    /**
     * @type list: hydra/ext/scala/meta.Mod.Annot
     */
    annots: Seq[Mod_Annot]
)

val _Type_Annotate: String = "hydra/ext/scala/meta.Type_Annotate"
val _Type_Annotate_annots: String = "annots"
val _Type_Annotate_tpe: String = "tpe"
