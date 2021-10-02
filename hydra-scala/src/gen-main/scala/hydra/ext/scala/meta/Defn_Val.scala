package hydra.ext.scala.meta

case class Defn_Val(
    /**
     * @type list: hydra/ext/scala/meta.Mod
     */
    mods: Seq[Mod],
    
    /**
     * @type list: hydra/ext/scala/meta.Pat
     */
    pats: Seq[Pat],
    
    /**
     * @type optional: hydra/ext/scala/meta.Type
     */
    decltpe: Option[Type],
    
    /**
     * @type hydra/ext/scala/meta.Term
     */
    rhs: Term
)

val _Defn_Val: String = "hydra/ext/scala/meta.Defn_Val"
val _Defn_Val_decltpe: String = "decltpe"
val _Defn_Val_mods: String = "mods"
val _Defn_Val_pats: String = "pats"
val _Defn_Val_rhs: String = "rhs"
