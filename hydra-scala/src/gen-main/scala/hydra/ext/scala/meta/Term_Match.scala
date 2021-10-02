package hydra.ext.scala.meta

case class Term_Match(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    expr: Term,
    
    /**
     * @type list: hydra/ext/scala/meta.Case
     */
    cases: Seq[Case]
)

val _Term_Match: String = "hydra/ext/scala/meta.Term_Match"
val _Term_Match_cases: String = "cases"
val _Term_Match_expr: String = "expr"
