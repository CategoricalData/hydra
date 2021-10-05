package hydra.translation

case class Language(
    /**
     * @type hydra/translation.Language.Name
     */
    name: Language_Name,
    
    /**
     * @type hydra/translation.Language.Constraints
     */
    constraints: Language_Constraints
)

val _Language: String = "hydra/translation.Language"
val _Language_constraints: String = "constraints"
val _Language_name: String = "name"
