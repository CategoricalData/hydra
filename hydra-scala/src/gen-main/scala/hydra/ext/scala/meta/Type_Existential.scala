package hydra.ext.scala.meta

case class Type_Existential(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type,
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[Stat]
)

val _Type_Existential: String = "hydra/ext/scala/meta.Type_Existential"
val _Type_Existential_stats: String = "stats"
val _Type_Existential_tpe: String = "tpe"
