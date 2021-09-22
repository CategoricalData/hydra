package hydra.evaluation

case class ReductionStyle(
    /**
     * Whether all lambda terms are considered to be fully reduced
     * 
     * @type boolean
     */
    lambdas: Boolean,
    
    /**
     * Whether all record terms are considered to be fully reduced
     * 
     * @type boolean
     */
    records: Boolean,
    
    /**
     * Whether all union terms are considered to be fully reduced
     * 
     * @type boolean
     */
    unions: Boolean,
    
    /**
     * Whether all element reference terms are considered to be fully reduced
     * 
     * @type boolean
     */
    elements: Boolean,
    
    /**
     * Whether all case statement terms are considered to be fully reduced
     * 
     * @type boolean
     */
    caseStatements: Boolean
)
