package hydra.ext.scala.meta

case class Defn_EnumCase(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    name: Term_Name,
    
    /**
     * @type list: hydra/ext/scala/meta.Type.Param
     */
    tparams: Seq[Type_Param],
    
    /**
     * @type hydra/ext/scala/meta.Ctor.Primary
     */
    ctor: Ctor_Primary,
    
    /**
     * @type list: hydra/ext/scala/meta.Init
     */
    inits: Seq[Init]
)

val _Defn_EnumCase: String = "hydra/ext/scala/meta.Defn_EnumCase"
val _Defn_EnumCase_ctor: String = "ctor"
val _Defn_EnumCase_inits: String = "inits"
val _Defn_EnumCase_mods: String = "mods"
val _Defn_EnumCase_name: String = "name"
val _Defn_EnumCase_tparams: String = "tparams"
