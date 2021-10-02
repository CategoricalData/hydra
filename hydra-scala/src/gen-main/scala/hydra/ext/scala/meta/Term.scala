package hydra.ext.scala.meta

enum Term:
    /**
     * @type hydra/ext/scala/meta.Lit
     */
    case lit(value: Lit) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    case ref(value: Term_Ref) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Interpolate
     */
    case interpolate(value: Term_Interpolate) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Xml
     */
    case xml(value: Term_Xml) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Apply
     */
    case apply(value: Term_Apply) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyUsing
     */
    case applyUsing(value: Term_ApplyUsing) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyType
     */
    case applyType(value: Term_ApplyType) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyInfix
     */
    case applyInfix(value: Term_ApplyInfix) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Assign
     */
    case assign(value: Term_Assign) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Return
     */
    case `return`(value: Term_Return) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Throw
     */
    case `throw`(value: Term_Throw) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Ascribe
     */
    case ascribe(value: Term_Ascribe) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Annotate
     */
    case annotate(value: Term_Annotate) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Tuple
     */
    case tuple(value: Term_Tuple) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Block
     */
    case block(value: Term_Block) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.EndMarker
     */
    case endMarker(value: Term_EndMarker) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.If
     */
    case `if`(value: Term_If) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.QuotedMacroExpr
     */
    case quotedMacroExpr(value: Term_QuotedMacroExpr) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.QuotedMacroType
     */
    case quotedMacroType(value: Term_QuotedMacroType) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.SplicedMacroExpr
     */
    case splicedMacroExpr(value: Term_SplicedMacroExpr) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Match
     */
    case `match`(value: Term_Match) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Try
     */
    case `try`(value: Term_Try) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.TryWithHandler
     */
    case tryWithHandler(value: Term_TryWithHandler) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.FunctionTerm
     */
    case functionTerm(value: Term_FunctionTerm) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.PolyFunction
     */
    case polyFunction(value: Term_PolyFunction) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.PartialFunction
     */
    case partialFunction(value: Term_PartialFunction) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.While
     */
    case `while`(value: Term_While) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Do
     */
    case `do`(value: Term_Do) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.For
     */
    case `for`(value: Term_For) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ForYield
     */
    case forYield(value: Term_ForYield) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.New
     */
    case `new`(value: Term_New) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.NewAnonymous
     */
    case newAnonymous(value: Term_NewAnonymous) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Placeholder
     */
    case placeholder(value: Term_Placeholder) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Eta
     */
    case eta(value: Term_Eta) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Repeated
     */
    case repeated(value: Term_Repeated) extends Term

val _Term: String = "hydra/ext/scala/meta.Term"
val _Term_annotate: String = "annotate"
val _Term_apply: String = "apply"
val _Term_applyInfix: String = "applyInfix"
val _Term_applyType: String = "applyType"
val _Term_applyUsing: String = "applyUsing"
val _Term_ascribe: String = "ascribe"
val _Term_assign: String = "assign"
val _Term_block: String = "block"
val _Term_do: String = "do"
val _Term_endMarker: String = "endMarker"
val _Term_eta: String = "eta"
val _Term_for: String = "for"
val _Term_forYield: String = "forYield"
val _Term_functionTerm: String = "functionTerm"
val _Term_if: String = "if"
val _Term_interpolate: String = "interpolate"
val _Term_lit: String = "lit"
val _Term_match: String = "match"
val _Term_new: String = "new"
val _Term_newAnonymous: String = "newAnonymous"
val _Term_partialFunction: String = "partialFunction"
val _Term_placeholder: String = "placeholder"
val _Term_polyFunction: String = "polyFunction"
val _Term_quotedMacroExpr: String = "quotedMacroExpr"
val _Term_quotedMacroType: String = "quotedMacroType"
val _Term_ref: String = "ref"
val _Term_repeated: String = "repeated"
val _Term_return: String = "return"
val _Term_splicedMacroExpr: String = "splicedMacroExpr"
val _Term_throw: String = "throw"
val _Term_try: String = "try"
val _Term_tryWithHandler: String = "tryWithHandler"
val _Term_tuple: String = "tuple"
val _Term_while: String = "while"
val _Term_xml: String = "xml"
