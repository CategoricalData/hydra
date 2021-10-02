package hydra.ext.scala.meta

case class Defn_Type(
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
     * @type hydra/ext/scala/meta.Type
     */
    body: Type
)

val _Defn_Type: String = "hydra/ext/scala/meta.Defn_Type"
val _Defn_Type_body: String = "body"
val _Defn_Type_mods: String = "mods"
val _Defn_Type_name: String = "name"
val _Defn_Type_tparams: String = "tparams"
