package hydra.ext.scala.meta

case class Decl_Given(
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
    sparams: Seq[Seq[Term_Param]],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    decltpe: Type
)

val _Decl_Given: String = "hydra/ext/scala/meta.Decl_Given"
val _Decl_Given_decltpe: String = "decltpe"
val _Decl_Given_mods: String = "mods"
val _Decl_Given_name: String = "name"
val _Decl_Given_sparams: String = "sparams"
val _Decl_Given_tparams: String = "tparams"
