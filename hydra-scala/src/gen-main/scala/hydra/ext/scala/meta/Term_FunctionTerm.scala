package hydra.ext.scala.meta

enum Term_FunctionTerm:
    /**
     * @type hydra/ext/scala/meta.Term.ContextFunction
     */
    case contextFunction(value: Term_ContextFunction) extends Term_FunctionTerm
    /**
     * @type hydra/ext/scala/meta.Term.Function
     */
    case Function(value: Term_Function) extends Term_FunctionTerm

val _Term_FunctionTerm: String = "hydra/ext/scala/meta.Term_FunctionTerm"
val _Term_FunctionTerm_Function: String = "Function"
val _Term_FunctionTerm_contextFunction: String = "contextFunction"
