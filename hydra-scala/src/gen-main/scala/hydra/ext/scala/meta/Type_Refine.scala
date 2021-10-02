package hydra.ext.scala.meta

case class Type_Refine(
    /**
     * @type optional: hydra/ext/scala/meta.Type
     */
    tpe: Option[Type],
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[Stat]
)

val _Type_Refine: String = "hydra/ext/scala/meta.Type_Refine"
val _Type_Refine_stats: String = "stats"
val _Type_Refine_tpe: String = "tpe"
