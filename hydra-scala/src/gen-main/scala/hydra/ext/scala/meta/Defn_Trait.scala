package hydra.ext.scala.meta

case class Defn_Trait(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Type.Name
     */
    name: Type_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type hydra/ext/scala/meta.Ctor.Primary
     */
    ctor: Ctor_Primary,
    
    /**
     * @type hydra/ext/scala/meta.Template
     */
    template: Template
)

val _Defn_Trait: String = "hydra/ext/scala/meta.Defn_Trait"
val _Defn_Trait_ctor: String = "ctor"
val _Defn_Trait_mods: String = "mods"
val _Defn_Trait_name: String = "name"
val _Defn_Trait_template: String = "template"
val _Defn_Trait_tparams: String = "tparams"
