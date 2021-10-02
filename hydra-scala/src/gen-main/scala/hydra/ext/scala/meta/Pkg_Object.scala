package hydra.ext.scala.meta

case class Pkg_Object(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    name: Term_Name,
    
    /**
     * @type hydra/ext/scala/meta.Template
     */
    template: Template
)

val _Pkg_Object: String = "hydra/ext/scala/meta.Pkg_Object"
val _Pkg_Object_mods: String = "mods"
val _Pkg_Object_name: String = "name"
val _Pkg_Object_template: String = "template"
