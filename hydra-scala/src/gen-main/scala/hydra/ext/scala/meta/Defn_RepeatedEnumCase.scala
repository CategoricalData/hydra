package hydra.ext.scala.meta

case class Defn_RepeatedEnumCase(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type list: hydra/ext/scala/meta.Term.Name
     */
    cases: Seq[Term_Name]
)

val _Defn_RepeatedEnumCase: String = "hydra/ext/scala/meta.Defn_RepeatedEnumCase"
val _Defn_RepeatedEnumCase_cases: String = "cases"
val _Defn_RepeatedEnumCase_mods: String = "mods"
