package hydra.ext.scala.meta

case class Template (
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    early: Seq[hydra.ext.scala.meta.Stat],
    
    /**
     * @type list: hydra/ext/scala/meta.Init
     */
    inits: Seq[hydra.ext.scala.meta.Init],
    
    /**
     * @type hydra/ext/scala/meta.Self
     */
    self: hydra.ext.scala.meta.Self,
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[hydra.ext.scala.meta.Stat]
)

val _Template: String = "hydra/ext/scala/meta.Template"
val _Template_early: String = "early"
val _Template_inits: String = "inits"
val _Template_self: String = "self"
val _Template_stats: String = "stats"
