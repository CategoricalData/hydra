package hydra.adapter

case class Language(
    /**
     * @type hydra/adapter.Language.Name
     */
    name: Language_Name,
    
    /**
     * @type hydra/adapter.Language.Constraints
     */
    constraints: Language_Constraints
)

val _Language: String = "hydra/adapter.Language"
val _Language_constraints: String = "constraints"
val _Language_name: String = "name"
