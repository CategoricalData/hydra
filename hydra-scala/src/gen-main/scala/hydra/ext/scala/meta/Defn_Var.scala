package hydra.ext.scala.meta

case class Defn_Var(
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
    decltpe: Type,
    
    /**
     * @type optional: hydra/ext/scala/meta.Term
     */
    rhs: Option[Term]
)

val _Defn_Var: String = "hydra/ext/scala/meta.Defn_Var"
val _Defn_Var_decltpe: String = "decltpe"
val _Defn_Var_mods: String = "mods"
val _Defn_Var_pats: String = "pats"
val _Defn_Var_rhs: String = "rhs"
