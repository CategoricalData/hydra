package hydra.ext.scala.meta

case class Pkg (
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    name: hydra.ext.scala.meta.Term.Name,
    
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    ref: hydra.ext.scala.meta.Term.Ref,
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[hydra.ext.scala.meta.Stat]
)
object Pkg {
    case class Object (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        template: hydra.ext.scala.meta.Template
    )
}

val _Pkg: String = "hydra/ext/scala/meta.Pkg"
val _Pkg_Object: String = "hydra/ext/scala/meta.Pkg.Object"
val _Pkg_Object_mods: String = "mods"
val _Pkg_Object_name: String = "name"
val _Pkg_Object_template: String = "template"
val _Pkg_name: String = "name"
val _Pkg_ref: String = "ref"
val _Pkg_stats: String = "stats"
