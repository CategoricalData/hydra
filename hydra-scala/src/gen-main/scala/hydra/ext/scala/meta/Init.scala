package hydra.ext.scala.meta

case class Init(
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: Type,
    
    /**
     * @type hydra/ext/scala/meta.Name
     */
    name: Name,
    
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term
     */
    argss: Seq[Seq[Term]]
)

val _Init: String = "hydra/ext/scala/meta.Init"
val _Init_argss: String = "argss"
val _Init_name: String = "name"
val _Init_tpe: String = "tpe"
