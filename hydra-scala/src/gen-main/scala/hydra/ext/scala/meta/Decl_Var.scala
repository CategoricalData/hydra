package hydra.ext.scala.meta

case class Decl_Var(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type list: hydra/ext/scala/meta.Pat
     */
    pats: Seq[Pat],
    
    /**
     * @type hydra/ext/scala/meta.Type
     */
    decltpe: Type
)

val _Decl_Var: String = "hydra/ext/scala/meta.Decl_Var"
val _Decl_Var_decltpe: String = "decltpe"
val _Decl_Var_mods: String = "mods"
val _Decl_Var_pats: String = "pats"
