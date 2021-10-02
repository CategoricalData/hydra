package hydra.ext.scala.meta

case class Decl_Type(
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
     * @type hydra/ext/scala/meta.Type.Bounds
     */
    bounds: Type_Bounds
)

val _Decl_Type: String = "hydra/ext/scala/meta.Decl_Type"
val _Decl_Type_bounds: String = "bounds"
val _Decl_Type_mods: String = "mods"
val _Decl_Type_name: String = "name"
val _Decl_Type_tparams: String = "tparams"
