package hydra.translation

import hydra.core.AtomicVariant
import hydra.core.TermVariant
import hydra.core.TypeVariant

case class Language_Constraints(
    /**
     * @type set: hydra/core.AtomicVariant
     */
    atomicVariants: Set[AtomicVariant],
    
    /**
     * @type set: hydra/core.TermVariant
     */
    termVariants: Set[TermVariant],
    
    /**
     * @type set: hydra/core.TypeVariant
     */
    typeVariants: Set[TypeVariant]
)

val _Language_Constraints: String = "hydra/translation.Language_Constraints"
val _Language_Constraints_atomicVariants: String = "atomicVariants"
val _Language_Constraints_termVariants: String = "termVariants"
val _Language_Constraints_typeVariants: String = "typeVariants"
