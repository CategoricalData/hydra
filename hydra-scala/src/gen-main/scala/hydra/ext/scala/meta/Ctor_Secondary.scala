package hydra.ext.scala.meta

case class Ctor_Secondary(
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
    paramss: Seq[Seq[Term_Param]],
    
    /**
     * @type hydra/ext/scala/meta.Init
     */
    init: Init,
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[Stat]
)

val _Ctor_Secondary: String = "hydra/ext/scala/meta.Ctor_Secondary"
val _Ctor_Secondary_init: String = "init"
val _Ctor_Secondary_mods: String = "mods"
val _Ctor_Secondary_name: String = "name"
val _Ctor_Secondary_paramss: String = "paramss"
val _Ctor_Secondary_stats: String = "stats"
