package hydra.ext.scala.meta

enum Name:
    /**
     * @type string
     */
    case value(value: String) extends Name
    case anonymous() extends Name
    /**
     * @type hydra/ext/scala/meta.PredefString
     */
    case indeterminate(value: hydra.ext.scala.meta.PredefString) extends Name

val _Name: String = "hydra/ext/scala/meta.Name"
val _Name_anonymous: String = "anonymous"
val _Name_indeterminate: String = "indeterminate"
val _Name_value: String = "value"
