package hydra.ext.scala.meta

case class Defn_Macro(
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
     * @type list:
     *         list: hydra/ext/scala/meta.Term.Param
     */
    paramss: Seq[Seq[Term_Param]],
    
    /**
     * @type optional: hydra/ext/scala/meta.Type
     */
    decltpe: Option[Type],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Defn_Macro: String = "hydra/ext/scala/meta.Defn_Macro"
val _Defn_Macro_body: String = "body"
val _Defn_Macro_decltpe: String = "decltpe"
val _Defn_Macro_mods: String = "mods"
val _Defn_Macro_name: String = "name"
val _Defn_Macro_paramss: String = "paramss"
val _Defn_Macro_tparams: String = "tparams"
