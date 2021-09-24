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

val _ReductionStyle: String = "hydra/evaluation.ReductionStyle"
val _ReductionStyle_caseStatements: String = "caseStatements"
val _ReductionStyle_elements: String = "elements"
val _ReductionStyle_lambdas: String = "lambdas"
val _ReductionStyle_records: String = "records"
val _ReductionStyle_unions: String = "unions"
