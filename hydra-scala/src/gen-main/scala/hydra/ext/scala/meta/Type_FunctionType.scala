package hydra.ext.scala.meta

enum Type_FunctionType:
    /**
     * @type hydra/ext/scala/meta.Type.Function
     */
    case function(value: Type_Function) extends Type_FunctionType
    /**
     * @type hydra/ext/scala/meta.Type.ContextFunction
     */
    case contextFunction(value: Type_ContextFunction) extends Type_FunctionType

val _Type_FunctionType: String = "hydra/ext/scala/meta.Type_FunctionType"
val _Type_FunctionType_contextFunction: String = "contextFunction"
val _Type_FunctionType_function: String = "function"
