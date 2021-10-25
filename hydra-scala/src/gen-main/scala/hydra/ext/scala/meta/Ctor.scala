package hydra.ext.scala.meta

enum Ctor:
    /**
     * @type hydra/ext/scala/meta.Ctor.Primary
     */
    case primary(value: hydra.ext.scala.meta.Ctor.Primary) extends Ctor
    /**
     * @type hydra/ext/scala/meta.Ctor.Secondary
     */
    case secondary(value: hydra.ext.scala.meta.Ctor.Secondary) extends Ctor
object Ctor {
    case class Primary (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name,
        
        /**
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         */
        paramss: Seq[Seq[hydra.ext.scala.meta.Term.Param]]
    )
    
    case class Secondary (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name,
        
        /**
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         */
        paramss: Seq[Seq[hydra.ext.scala.meta.Term.Param]],
        
        /**
         * @type hydra/ext/scala/meta.Init
         * 
         * @type hydra/ext/scala/meta.Init
         */
        init: hydra.ext.scala.meta.Init,
        
        /**
         * @type list: hydra/ext/scala/meta.Stat
         * 
         * @type list: hydra/ext/scala/meta.Stat
         */
        stats: Seq[hydra.ext.scala.meta.Stat]
    )
}

val _Ctor: String = "hydra/ext/scala/meta.Ctor"
val _Ctor_Primary: String = "hydra/ext/scala/meta.Ctor.Primary"
val _Ctor_Primary_mods: String = "mods"
val _Ctor_Primary_name: String = "name"
val _Ctor_Primary_paramss: String = "paramss"
val _Ctor_Secondary: String = "hydra/ext/scala/meta.Ctor.Secondary"
val _Ctor_Secondary_init: String = "init"
val _Ctor_Secondary_mods: String = "mods"
val _Ctor_Secondary_name: String = "name"
val _Ctor_Secondary_paramss: String = "paramss"
val _Ctor_Secondary_stats: String = "stats"
val _Ctor_primary: String = "primary"
val _Ctor_secondary: String = "secondary"
