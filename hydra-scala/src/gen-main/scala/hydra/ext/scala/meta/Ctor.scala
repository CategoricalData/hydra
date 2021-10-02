package hydra.ext.scala.meta

enum Ctor:
    /**
     * @type hydra/ext/scala/meta.Ctor.Primary
     */
    case primary(value: Ctor_Primary) extends Ctor
    /**
     * @type hydra/ext/scala/meta.Ctor.Secondary
     */
    case secondary(value: Ctor_Secondary) extends Ctor

val _Ctor: String = "hydra/ext/scala/meta.Ctor"
val _Ctor_primary: String = "primary"
val _Ctor_secondary: String = "secondary"
