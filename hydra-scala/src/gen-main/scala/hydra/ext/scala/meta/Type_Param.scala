package hydra.ext.scala.meta

case class Type_Param(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Name
     */
    name: Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type list: hydra/ext/scala/meta.Type.Bounds
     */
    tbounds: Seq[Type_Bounds],
    
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    vbounds: Seq[Type],
    
    /**
     * @type list: hydra/ext/scala/meta.Type
     */
    cbounds: Seq[Type]
)

val _Type_Param: String = "hydra/ext/scala/meta.Type_Param"
val _Type_Param_cbounds: String = "cbounds"
val _Type_Param_mods: String = "mods"
val _Type_Param_name: String = "name"
val _Type_Param_tbounds: String = "tbounds"
val _Type_Param_tparams: String = "tparams"
val _Type_Param_vbounds: String = "vbounds"
