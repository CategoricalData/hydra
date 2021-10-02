package hydra.ext.scala.meta

case class Name(
    /**
     * @type string
     */
    value: String,
    
    anonymous: Void,
    
    /**
     * @type hydra/ext/scala/meta.PredefString
     */
    indeterminate: PredefString
)

val _Name: String = "hydra/ext/scala/meta.Name"
val _Name_anonymous: String = "anonymous"
val _Name_indeterminate: String = "indeterminate"
val _Name_value: String = "value"
