package hydra.adapter

import hydra.core.AtomicVariant
import hydra.core.FloatVariant
import hydra.core.IntegerVariant
import hydra.core.TermVariant
import hydra.core.TypeVariant

case class Language_Constraints(
    /**
     * @type set: hydra/core.AtomicVariant
     */
    atomicVariants: Set[AtomicVariant],
    
    /**
     * @type set: hydra/core.FloatVariant
     */
    floatVariants: Set[FloatVariant],
    
    /**
     * @type set: hydra/core.IntegerVariant
     */
    integerVariants: Set[IntegerVariant],
    
    /**
     * @type set: hydra/core.TermVariant
     */
    termVariants: Set[TermVariant],
    
    /**
     * @type set: hydra/core.TypeVariant
     */
    typeVariants: Set[TypeVariant]
)

val _Language_Constraints: String = "hydra/adapter.Language_Constraints"
val _Language_Constraints_atomicVariants: String = "atomicVariants"
val _Language_Constraints_floatVariants: String = "floatVariants"
val _Language_Constraints_integerVariants: String = "integerVariants"
val _Language_Constraints_termVariants: String = "termVariants"
val _Language_Constraints_typeVariants: String = "typeVariants"
