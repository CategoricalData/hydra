package hydra.ext.scala.meta

enum Ref:
    /**
     * @type hydra/ext/scala/meta.Name
     */
    case name(value: hydra.ext.scala.meta.Name) extends Ref
    /**
     * @type hydra/ext/scala/meta.Init
     */
    case init(value: hydra.ext.scala.meta.Init) extends Ref

val _Ref: String = "hydra/ext/scala/meta.Ref"
val _Ref_init: String = "init"
val _Ref_name: String = "name"
