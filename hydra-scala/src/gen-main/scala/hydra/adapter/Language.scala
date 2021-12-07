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
         * @type set: hydra/core.LiteralVariant
         * 
         * @type set: hydra/core.LiteralVariant
         */
        literalVariants: Set[hydra.core.LiteralVariant],
        
        /**
         * @type set: hydra/core.FloatType
         * 
         * @type set: hydra/core.FloatType
         */
        floatTypes: Set[hydra.core.FloatType],
        
        /**
         * @type set: hydra/core.FunctionVariant
         * 
         * @type set: hydra/core.FunctionVariant
         */
        functionVariants: Set[hydra.core.FunctionVariant],
        
        /**
         * @type set: hydra/core.IntegerType
         * 
         * @type set: hydra/core.IntegerType
         */
        integerTypes: Set[hydra.core.IntegerType],
        
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
val _Language_Constraints_floatTypes: String = "floatTypes"
val _Language_Constraints_functionVariants: String = "functionVariants"
val _Language_Constraints_integerTypes: String = "integerTypes"
val _Language_Constraints_literalVariants: String = "literalVariants"
val _Language_Constraints_termVariants: String = "termVariants"
val _Language_Constraints_typeVariants: String = "typeVariants"
val _Language_Constraints_types: String = "types"
val _Language_Name: String = "hydra/adapter.Language.Name"
val _Language_constraints: String = "constraints"
val _Language_name: String = "name"
