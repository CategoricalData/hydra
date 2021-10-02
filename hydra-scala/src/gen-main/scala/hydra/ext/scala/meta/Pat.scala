package hydra.ext.scala.meta

enum Pat:
    /**
     * @type hydra/ext/scala/meta.Pat.Var
     */
    case `var`(value: Pat_Var) extends Pat
    case wildcard() extends Pat
    case seqWildcard() extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Bind
     */
    case bind(value: Pat_Bind) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Alternative
     */
    case alternative(value: Pat_Alternative) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Tuple
     */
    case tuple(value: Pat_Tuple) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Repeated
     */
    case repeated(value: Pat_Repeated) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Extract
     */
    case extract(value: Pat_Extract) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.ExtractInfix
     */
    case extractInfix(value: Pat_ExtractInfix) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Interpolate
     */
    case interpolate(value: Pat_Interpolate) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Xml
     */
    case xml(value: Pat_Xml) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Typed
     */
    case typed(value: Pat_Typed) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Macro
     */
    case `macro`(value: Pat_Macro) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Given
     */
    case `given`(value: Pat_Given) extends Pat

val _Pat: String = "hydra/ext/scala/meta.Pat"
val _Pat_alternative: String = "alternative"
val _Pat_bind: String = "bind"
val _Pat_extract: String = "extract"
val _Pat_extractInfix: String = "extractInfix"
val _Pat_given: String = "given"
val _Pat_interpolate: String = "interpolate"
val _Pat_macro: String = "macro"
val _Pat_repeated: String = "repeated"
val _Pat_seqWildcard: String = "seqWildcard"
val _Pat_tuple: String = "tuple"
val _Pat_typed: String = "typed"
val _Pat_var: String = "var"
val _Pat_wildcard: String = "wildcard"
val _Pat_xml: String = "xml"
