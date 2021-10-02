package hydra.ext.scala.meta

case class Pkg(
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    name: Term_Name,
    
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    ref: Term_Ref,
    
    /**
     * @type list: hydra/ext/scala/meta.Stat
     */
    stats: Seq[Stat]
)

val _Pkg: String = "hydra/ext/scala/meta.Pkg"
val _Pkg_name: String = "name"
val _Pkg_ref: String = "ref"
val _Pkg_stats: String = "stats"
