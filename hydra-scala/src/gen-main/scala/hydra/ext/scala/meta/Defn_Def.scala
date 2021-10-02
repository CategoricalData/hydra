package hydra.ext.scala.meta

case class Defn_Def(
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
    
    decltpe: Void,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Defn_Def: String = "hydra/ext/scala/meta.Defn_Def"
val _Defn_Def_body: String = "body"
val _Defn_Def_decltpe: String = "decltpe"
val _Defn_Def_mods: String = "mods"
val _Defn_Def_name: String = "name"
val _Defn_Def_paramss: String = "paramss"
val _Defn_Def_tparams: String = "tparams"
