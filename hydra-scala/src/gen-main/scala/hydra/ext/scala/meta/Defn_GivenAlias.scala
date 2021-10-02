package hydra.ext.scala.meta

case class Defn_GivenAlias(
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
     * @type hydra/ext/scala/meta.Type
     */
    decltpe: Type,
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    body: Term
)

val _Defn_GivenAlias: String = "hydra/ext/scala/meta.Defn_GivenAlias"
val _Defn_GivenAlias_body: String = "body"
val _Defn_GivenAlias_decltpe: String = "decltpe"
val _Defn_GivenAlias_mods: String = "mods"
val _Defn_GivenAlias_name: String = "name"
val _Defn_GivenAlias_sparams: String = "sparams"
val _Defn_GivenAlias_tparams: String = "tparams"
