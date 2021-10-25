package hydra.adapter

case class Language (
    /**
     * @type hydra/adapter.Language.Name
     */
    name: hydra.adapter.Language.Name,
    
    /**
     * @type hydra/adapter.Language.Constraints
     */
    constraints: hydra.adapter.Language.Constraints
)
object Language {
    case class Constraints (
        /**
         * @type set: hydra/core.AtomicVariant
         * 
         * @type set: hydra/core.AtomicVariant
         */
        atomicVariants: Set[hydra.core.AtomicVariant],
        
        /**
         * @type set: hydra/core.FloatVariant
         * 
         * @type set: hydra/core.FloatVariant
         */
        floatVariants: Set[hydra.core.FloatVariant],
        
        /**
         * @type set: hydra/core.IntegerVariant
         * 
         * @type set: hydra/core.IntegerVariant
         */
        integerVariants: Set[hydra.core.IntegerVariant],
        
        /**
         * @type set: hydra/core.TermVariant
         * 
         * @type set: hydra/core.TermVariant
         */
        termVariants: Set[hydra.core.TermVariant],
        
        /**
         * @type set: hydra/core.TypeVariant
         * 
         * @type set: hydra/core.TypeVariant
         */
        typeVariants: Set[hydra.core.TypeVariant],
        
        /**
         * @type function:
         *         from:
         *         - hydra/core.Type
         *         to: boolean
         * 
         * @type function:
         *         from:
         *         - hydra/core.Type
         *         to: boolean
         */
        types: hydra.core.Type => Boolean
    )
    
    /**
     * @type string
     */
    type Name = String
}

val _Language: String = "hydra/adapter.Language"
val _Language_Constraints: String = "hydra/adapter.Language.Constraints"
val _Language_Constraints_atomicVariants: String = "atomicVariants"
val _Language_Constraints_floatVariants: String = "floatVariants"
val _Language_Constraints_integerVariants: String = "integerVariants"
val _Language_Constraints_termVariants: String = "termVariants"
val _Language_Constraints_typeVariants: String = "typeVariants"
val _Language_Constraints_types: String = "types"
val _Language_Name: String = "hydra/adapter.Language.Name"
val _Language_constraints: String = "constraints"
val _Language_name: String = "name"
