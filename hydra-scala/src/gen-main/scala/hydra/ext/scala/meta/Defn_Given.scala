package hydra.ext.scala.meta

case class Defn_Given(
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
     *         list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Seq[Type_Param]],
    
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term.Param
     */
    sparams: Seq[Seq[Term_Param]],
    
    /**
     * @type hydra/ext/scala/meta.Template
     */
    templ: Template
)

val _Defn_Given: String = "hydra/ext/scala/meta.Defn_Given"
val _Defn_Given_mods: String = "mods"
val _Defn_Given_name: String = "name"
val _Defn_Given_sparams: String = "sparams"
val _Defn_Given_templ: String = "templ"
val _Defn_Given_tparams: String = "tparams"
