package hydra.ext.scala.meta

case class Ctor_Primary(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Name
     */
    name: Name,
    
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term.Param
     */
    paramss: Seq[Seq[Term_Param]]
)

val _Ctor_Primary: String = "hydra/ext/scala/meta.Ctor_Primary"
val _Ctor_Primary_mods: String = "mods"
val _Ctor_Primary_name: String = "name"
val _Ctor_Primary_paramss: String = "paramss"
