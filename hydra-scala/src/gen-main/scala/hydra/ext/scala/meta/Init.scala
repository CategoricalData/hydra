package hydra.ext.scala.meta

case class Init (
    /**
     * @type hydra/ext/scala/meta.Type
     */
    tpe: hydra.ext.scala.meta.Type,
    
    /**
     * @type hydra/ext/scala/meta.Name
     */
    name: hydra.ext.scala.meta.Name,
    
    /**
     * @type list:
     *         list: hydra/ext/scala/meta.Term
     */
    argss: Seq[Seq[hydra.ext.scala.meta.Term]]
)

val _Init: String = "hydra/ext/scala/meta.Init"
val _Init_argss: String = "argss"
val _Init_name: String = "name"
val _Init_tpe: String = "tpe"
