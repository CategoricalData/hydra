package hydra.ext.scala.meta

case class Decl_Def(
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
     * @type hydra/ext/scala/meta.Type
     */
    decltpe: Type
)

val _Decl_Def: String = "hydra/ext/scala/meta.Decl_Def"
val _Decl_Def_decltpe: String = "decltpe"
val _Decl_Def_mods: String = "mods"
val _Decl_Def_name: String = "name"
val _Decl_Def_paramss: String = "paramss"
val _Decl_Def_tparams: String = "tparams"
