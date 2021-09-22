package hydra.core

case class CaseStatement(
    /**
     * A handler for each alternative in a union type. The term of each case must be function-typed.
     * 
     * @type list: hydra/core.Field
     */
    cases: Seq[Field],
    
    /**
     * A convenience which allows certain "don't care" cases to be omitted. The result is a term which does not otherwise
     * depend on the variant value.
     * 
     * @type hydra/core.Term
     */
    default: Term
)
