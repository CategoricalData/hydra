package hydra.ext.scala.meta

case class Decl_Val(
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

val _Decl_Val: String = "hydra/ext/scala/meta.Decl_Val"
val _Decl_Val_decltpe: String = "decltpe"
val _Decl_Val_mods: String = "mods"
val _Decl_Val_pats: String = "pats"
