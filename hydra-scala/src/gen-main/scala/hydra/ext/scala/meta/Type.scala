package hydra.ext.scala.meta

enum Type:
    /**
     * @type hydra/ext/scala/meta.Type.Ref
     */
    case ref(value: Type_Ref) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.AnonymousName
     */
    case anonymousName(value: Type_AnonymousName) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Apply
     */
    case apply(value: Type_Apply) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ApplyInfix
     */
    case applyInfix(value: Type_ApplyInfix) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.FunctionType
     */
    case functionType(value: Type_FunctionType) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.PolyFunction
     */
    case polyFunction(value: Type_PolyFunction) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ImplicitFunction
     */
    case implicitFunction(value: Type_ImplicitFunction) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Tuple
     */
    case tuple(value: Type_Tuple) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.With
     */
    case `with`(value: Type_With) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.And
     */
    case and(value: Type_And) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Or
     */
    case or(value: Type_Or) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Refine
     */
    case refine(value: Type_Refine) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Existential
     */
    case existential(value: Type_Existential) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Annotate
     */
    case annotate(value: Type_Annotate) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Lambda
     */
    case lambda(value: Type_Lambda) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Macro
     */
    case `macro`(value: Type_Macro) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Method
     */
    case method(value: Type_Method) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Placeholder
     */
    case placeholder(value: Type_Placeholder) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ByName
     */
    case byName(value: Type_ByName) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Repeated
     */
    case repeated(value: Type_Repeated) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Var
     */
    case `var`(value: Type_Var) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.TypedParam
     */
    case typedParam(value: Type_TypedParam) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Match
     */
    case `match`(value: Type_Match) extends Type

val _Type: String = "hydra/ext/scala/meta.Type"
val _Type_and: String = "and"
val _Type_annotate: String = "annotate"
val _Type_anonymousName: String = "anonymousName"
val _Type_apply: String = "apply"
val _Type_applyInfix: String = "applyInfix"
val _Type_byName: String = "byName"
val _Type_existential: String = "existential"
val _Type_functionType: String = "functionType"
val _Type_implicitFunction: String = "implicitFunction"
val _Type_lambda: String = "lambda"
val _Type_macro: String = "macro"
val _Type_match: String = "match"
val _Type_method: String = "method"
val _Type_or: String = "or"
val _Type_placeholder: String = "placeholder"
val _Type_polyFunction: String = "polyFunction"
val _Type_ref: String = "ref"
val _Type_refine: String = "refine"
val _Type_repeated: String = "repeated"
val _Type_tuple: String = "tuple"
val _Type_typedParam: String = "typedParam"
val _Type_var: String = "var"
val _Type_with: String = "with"
