package hydra.ext.scala.meta

case class Type_Match(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type,
    
    /**
     * @type list: hydra/ext/scala/meta.TypeCase
     */
    cases: Seq[TypeCase]
)

val _Type_Match: String = "hydra/ext/scala/meta.Type_Match"
val _Type_Match_cases: String = "cases"
val _Type_Match_tpe: String = "tpe"
