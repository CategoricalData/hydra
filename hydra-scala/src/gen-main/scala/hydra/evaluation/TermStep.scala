package hydra.evaluation

import hydra.core.FieldName

enum TermStep:
    case applicationFunction() extends TermStep
    case applicationArgument() extends TermStep
    /**
     * @type hydra/core.FieldName
     */
    case `case`(value: FieldName) extends TermStep
    case compareTo() extends TermStep
    case lambdaBody() extends TermStep
    /**
     * @type integer
     */
    case list(value: Int) extends TermStep
    /**
     * @type integer
     */
    case mapKey(value: Int) extends TermStep
    /**
     * @type integer
     */
    case mapValue(value: Int) extends TermStep
    /**
     * @type hydra/core.FieldName
     */
    case record(value: FieldName) extends TermStep
    /**
     * @type integer
     */
    case set(value: Int) extends TermStep
    case union() extends TermStep

val _TermStep: String = "hydra/evaluation.TermStep"
val _TermStep_applicationArgument: String = "applicationArgument"
val _TermStep_applicationFunction: String = "applicationFunction"
val _TermStep_case: String = "case"
val _TermStep_compareTo: String = "compareTo"
val _TermStep_lambdaBody: String = "lambdaBody"
val _TermStep_list: String = "list"
val _TermStep_mapKey: String = "mapKey"
val _TermStep_mapValue: String = "mapValue"
val _TermStep_record: String = "record"
val _TermStep_set: String = "set"
val _TermStep_union: String = "union"
