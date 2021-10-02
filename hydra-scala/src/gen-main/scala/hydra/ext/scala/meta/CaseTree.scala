package hydra.ext.scala.meta

enum CaseTree:
    /**
     * @type hydra/ext/scala/meta.Case
     */
    case `case`(value: Case) extends CaseTree
    /**
     * @type hydra/ext/scala/meta.TypeCase
     */
    case typeCase(value: TypeCase) extends CaseTree

val _CaseTree: String = "hydra/ext/scala/meta.CaseTree"
val _CaseTree_case: String = "case"
val _CaseTree_typeCase: String = "typeCase"
