package hydra.errors

case class Qualified[a](
    /**
     * @type optional:
     *         variable: a
     */
    value: Option[a],
    
    /**
     * @type list: string
     */
    warnings: Seq[String]
)

val _Qualified: String = "hydra/errors.Qualified"
val _Qualified_value: String = "value"
val _Qualified_warnings: String = "warnings"
