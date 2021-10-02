package hydra.ext.scala.meta

case class Term_Param(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Name
     */
    name: Name
)

val _Term_Param: String = "hydra/ext/scala/meta.Term_Param"
val _Term_Param_mods: String = "mods"
val _Term_Param_name: String = "name"
