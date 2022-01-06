package hydra.ext.scala.meta

enum Term:
    /**
     * @type hydra/ext/scala/meta.Lit
     */
    case lit(value: hydra.ext.scala.meta.Lit) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    case ref(value: hydra.ext.scala.meta.Term.Ref) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Interpolate
     */
    case interpolate(value: hydra.ext.scala.meta.Term.Interpolate) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Xml
     */
    case xml(value: hydra.ext.scala.meta.Term.Xml) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Apply
     */
    case apply(value: hydra.ext.scala.meta.Term.Apply) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyUsing
     */
    case applyUsing(value: hydra.ext.scala.meta.Term.ApplyUsing) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyType
     */
    case applyType(value: hydra.ext.scala.meta.Term.ApplyType) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ApplyInfix
     */
    case applyInfix(value: hydra.ext.scala.meta.Term.ApplyInfix) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Assign
     */
    case assign(value: hydra.ext.scala.meta.Term.Assign) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Return
     */
    case `return`(value: hydra.ext.scala.meta.Term.Return) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Throw
     */
    case `throw`(value: hydra.ext.scala.meta.Term.Throw) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Ascribe
     */
    case ascribe(value: hydra.ext.scala.meta.Term.Ascribe) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Annotate
     */
    case annotate(value: hydra.ext.scala.meta.Term.Annotate) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Tuple
     */
    case tuple(value: hydra.ext.scala.meta.Term.Tuple) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Block
     */
    case block(value: hydra.ext.scala.meta.Term.Block) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.EndMarker
     */
    case endMarker(value: hydra.ext.scala.meta.Term.EndMarker) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.If
     */
    case `if`(value: hydra.ext.scala.meta.Term.If) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.QuotedMacroExpr
     */
    case quotedMacroExpr(value: hydra.ext.scala.meta.Term.QuotedMacroExpr) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.QuotedMacroType
     */
    case quotedMacroType(value: hydra.ext.scala.meta.Term.QuotedMacroType) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.SplicedMacroExpr
     */
    case splicedMacroExpr(value: hydra.ext.scala.meta.Term.SplicedMacroExpr) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Match
     */
    case `match`(value: hydra.ext.scala.meta.Term.Match) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Try
     */
    case `try`(value: hydra.ext.scala.meta.Term.Try) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.TryWithHandler
     */
    case tryWithHandler(value: hydra.ext.scala.meta.Term.TryWithHandler) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.FunctionTerm
     */
    case functionTerm(value: hydra.ext.scala.meta.Term.FunctionTerm) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.PolyFunction
     */
    case polyFunction(value: hydra.ext.scala.meta.Term.PolyFunction) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.PartialFunction
     */
    case partialFunction(value: hydra.ext.scala.meta.Term.PartialFunction) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.While
     */
    case `while`(value: hydra.ext.scala.meta.Term.While) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Do
     */
    case `do`(value: hydra.ext.scala.meta.Term.Do) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.For
     */
    case `for`(value: hydra.ext.scala.meta.Term.For) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.ForYield
     */
    case forYield(value: hydra.ext.scala.meta.Term.ForYield) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.New
     */
    case `new`(value: hydra.ext.scala.meta.Term.New) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.NewAnonymous
     */
    case newAnonymous(value: hydra.ext.scala.meta.Term.NewAnonymous) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Placeholder
     */
    case placeholder(value: hydra.ext.scala.meta.Term.Placeholder) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Eta
     */
    case eta(value: hydra.ext.scala.meta.Term.Eta) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Repeated
     */
    case repeated(value: hydra.ext.scala.meta.Term.Repeated) extends Term
    /**
     * @type hydra/ext/scala/meta.Term.Param
     */
    case param(value: hydra.ext.scala.meta.Term.Param) extends Term
object Term {
    enum Ref:
        /**
         * @type hydra/ext/scala/meta.Term.This
         * 
         * @type hydra/ext/scala/meta.Term.This
         */
        case `this`(value: hydra.ext.scala.meta.Term.This) extends Ref
        /**
         * @type hydra/ext/scala/meta.Term.Super
         * 
         * @type hydra/ext/scala/meta.Term.Super
         */
        case `super`(value: hydra.ext.scala.meta.Term.Super) extends Ref
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        case name(value: hydra.ext.scala.meta.Term.Name) extends Ref
        /**
         * @type hydra/ext/scala/meta.Term.Anonymous
         * 
         * @type hydra/ext/scala/meta.Term.Anonymous
         */
        case anonymous(value: hydra.ext.scala.meta.Term.Anonymous) extends Ref
        /**
         * @type hydra/ext/scala/meta.Term.Select
         * 
         * @type hydra/ext/scala/meta.Term.Select
         */
        case select(value: hydra.ext.scala.meta.Term.Select) extends Ref
        /**
         * @type hydra/ext/scala/meta.Term.ApplyUnary
         * 
         * @type hydra/ext/scala/meta.Term.ApplyUnary
         */
        case applyUnary(value: hydra.ext.scala.meta.Term.ApplyUnary) extends Ref
    
    case class This ()
    
    case class Super (
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        thisp: hydra.ext.scala.meta.Name,
        
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        superp: hydra.ext.scala.meta.Name
    )
    
    case class Name (
        /**
         * @type hydra/ext/scala/meta.PredefString
         * 
         * @type hydra/ext/scala/meta.PredefString
         */
        value: hydra.ext.scala.meta.PredefString
    )
    
    case class Anonymous ()
    
    case class Select (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        qual: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name
    )
    
    case class Interpolate (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        prefix: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Lit
         * 
         * @type list: hydra/ext/scala/meta.Lit
         */
        parts: Seq[hydra.ext.scala.meta.Lit],
        
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class Xml (
        /**
         * @type list: hydra/ext/scala/meta.Lit
         * 
         * @type list: hydra/ext/scala/meta.Lit
         */
        parts: Seq[hydra.ext.scala.meta.Lit],
        
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class Apply (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        fun: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class ApplyUsing (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        fun: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        targs: Seq[hydra.ext.scala.meta.Type]
    )
    
    case class ApplyType (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        lhs: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        op: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        targs: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class ApplyInfix (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        lhs: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        op: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        targs: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class ApplyUnary (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        op: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        arg: hydra.ext.scala.meta.Term
    )
    
    case class Assign (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        lhs: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        rhs: hydra.ext.scala.meta.Term
    )
    
    case class Return (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term
    )
    
    case class Throw (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term
    )
    
    case class Ascribe (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class Annotate (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Mod.Annot
         * 
         * @type list: hydra/ext/scala/meta.Mod.Annot
         */
        annots: Seq[hydra.ext.scala.meta.Mod.Annot]
    )
    
    case class Tuple (
        /**
         * @type list: hydra/ext/scala/meta.Term
         * 
         * @type list: hydra/ext/scala/meta.Term
         */
        args: Seq[hydra.ext.scala.meta.Term]
    )
    
    case class Block (
        /**
         * @type list: hydra/ext/scala/meta.Stat
         * 
         * @type list: hydra/ext/scala/meta.Stat
         */
        stats: Seq[hydra.ext.scala.meta.Stat]
    )
    
    case class EndMarker (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name
    )
    
    case class If (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        cond: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        thenp: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        elsep: hydra.ext.scala.meta.Term
    )
    
    case class QuotedMacroExpr (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class QuotedMacroType (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class SplicedMacroExpr (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Match (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Case
         * 
         * @type list: hydra/ext/scala/meta.Case
         */
        cases: Seq[hydra.ext.scala.meta.Case]
    )
    
    case class Try (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Case
         * 
         * @type list: hydra/ext/scala/meta.Case
         */
        catchp: Seq[hydra.ext.scala.meta.Case],
        
        /**
         * @type optional: hydra/ext/scala/meta.Term
         * 
         * @type optional: hydra/ext/scala/meta.Term
         */
        finallyp: Option[hydra.ext.scala.meta.Term]
    )
    
    case class TryWithHandler (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        catchp: hydra.ext.scala.meta.Term,
        
        /**
         * @type optional: hydra/ext/scala/meta.Term
         * 
         * @type optional: hydra/ext/scala/meta.Term
         */
        finallyp: Option[hydra.ext.scala.meta.Term]
    )
    
    enum FunctionTerm:
        /**
         * @type hydra/ext/scala/meta.Term.ContextFunction
         * 
         * @type hydra/ext/scala/meta.Term.ContextFunction
         */
        case contextFunction(value: hydra.ext.scala.meta.Term.ContextFunction) extends FunctionTerm
        /**
         * @type hydra/ext/scala/meta.Term.Function
         * 
         * @type hydra/ext/scala/meta.Term.Function
         */
        case Function(value: hydra.ext.scala.meta.Term.Function) extends FunctionTerm
    
    case class ContextFunction (
        /**
         * @type list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list: hydra/ext/scala/meta.Term.Param
         */
        params: Seq[hydra.ext.scala.meta.Term.Param],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Function (
        /**
         * @type list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list: hydra/ext/scala/meta.Term.Param
         */
        params: Seq[hydra.ext.scala.meta.Term.Param],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class PolyFunction (
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class PartialFunction (
        /**
         * @type list: hydra/ext/scala/meta.Case
         * 
         * @type list: hydra/ext/scala/meta.Case
         */
        cases: Seq[hydra.ext.scala.meta.Case]
    )
    
    case class While (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Do (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term
    )
    
    case class For (
        /**
         * @type list: hydra/ext/scala/meta.Enumerator
         * 
         * @type list: hydra/ext/scala/meta.Enumerator
         */
        enums: Seq[hydra.ext.scala.meta.Enumerator]
    )
    
    case class ForYield (
        /**
         * @type list: hydra/ext/scala/meta.Enumerator
         * 
         * @type list: hydra/ext/scala/meta.Enumerator
         */
        enums: Seq[hydra.ext.scala.meta.Enumerator]
    )
    
    case class New (
        /**
         * @type hydra/ext/scala/meta.Init
         * 
         * @type hydra/ext/scala/meta.Init
         */
        init: hydra.ext.scala.meta.Init
    )
    
    case class NewAnonymous (
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        templ: hydra.ext.scala.meta.Template
    )
    
    case class Placeholder ()
    
    case class Eta (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term
    )
    
    case class Repeated (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        expr: hydra.ext.scala.meta.Term
    )
    
    case class Param (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name,
        
        /**
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        decltpe: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type optional: hydra/ext/scala/meta.Term
         * 
         * @type optional: hydra/ext/scala/meta.Term
         */
        default: Option[hydra.ext.scala.meta.Term]
    )
}

val _Term: String = "hydra/ext/scala/meta.Term"
val _Term_Annotate: String = "hydra/ext/scala/meta.Term.Annotate"
val _Term_Annotate_annots: String = "annots"
val _Term_Annotate_expr: String = "expr"
val _Term_Anonymous: String = "hydra/ext/scala/meta.Term.Anonymous"
val _Term_Apply: String = "hydra/ext/scala/meta.Term.Apply"
val _Term_ApplyInfix: String = "hydra/ext/scala/meta.Term.ApplyInfix"
val _Term_ApplyInfix_args: String = "args"
val _Term_ApplyInfix_lhs: String = "lhs"
val _Term_ApplyInfix_op: String = "op"
val _Term_ApplyInfix_targs: String = "targs"
val _Term_ApplyType: String = "hydra/ext/scala/meta.Term.ApplyType"
val _Term_ApplyType_args: String = "args"
val _Term_ApplyType_lhs: String = "lhs"
val _Term_ApplyType_op: String = "op"
val _Term_ApplyType_targs: String = "targs"
val _Term_ApplyUnary: String = "hydra/ext/scala/meta.Term.ApplyUnary"
val _Term_ApplyUnary_arg: String = "arg"
val _Term_ApplyUnary_op: String = "op"
val _Term_ApplyUsing: String = "hydra/ext/scala/meta.Term.ApplyUsing"
val _Term_ApplyUsing_fun: String = "fun"
val _Term_ApplyUsing_targs: String = "targs"
val _Term_Apply_args: String = "args"
val _Term_Apply_fun: String = "fun"
val _Term_Ascribe: String = "hydra/ext/scala/meta.Term.Ascribe"
val _Term_Ascribe_expr: String = "expr"
val _Term_Ascribe_tpe: String = "tpe"
val _Term_Assign: String = "hydra/ext/scala/meta.Term.Assign"
val _Term_Assign_lhs: String = "lhs"
val _Term_Assign_rhs: String = "rhs"
val _Term_Block: String = "hydra/ext/scala/meta.Term.Block"
val _Term_Block_stats: String = "stats"
val _Term_ContextFunction: String = "hydra/ext/scala/meta.Term.ContextFunction"
val _Term_ContextFunction_body: String = "body"
val _Term_ContextFunction_params: String = "params"
val _Term_Do: String = "hydra/ext/scala/meta.Term.Do"
val _Term_Do_body: String = "body"
val _Term_Do_expr: String = "expr"
val _Term_EndMarker: String = "hydra/ext/scala/meta.Term.EndMarker"
val _Term_EndMarker_name: String = "name"
val _Term_Eta: String = "hydra/ext/scala/meta.Term.Eta"
val _Term_Eta_expr: String = "expr"
val _Term_For: String = "hydra/ext/scala/meta.Term.For"
val _Term_ForYield: String = "hydra/ext/scala/meta.Term.ForYield"
val _Term_ForYield_enums: String = "enums"
val _Term_For_enums: String = "enums"
val _Term_Function: String = "hydra/ext/scala/meta.Term.Function"
val _Term_FunctionTerm: String = "hydra/ext/scala/meta.Term.FunctionTerm"
val _Term_FunctionTerm_Function: String = "Function"
val _Term_FunctionTerm_contextFunction: String = "contextFunction"
val _Term_Function_body: String = "body"
val _Term_Function_params: String = "params"
val _Term_If: String = "hydra/ext/scala/meta.Term.If"
val _Term_If_cond: String = "cond"
val _Term_If_elsep: String = "elsep"
val _Term_If_thenp: String = "thenp"
val _Term_Interpolate: String = "hydra/ext/scala/meta.Term.Interpolate"
val _Term_Interpolate_args: String = "args"
val _Term_Interpolate_parts: String = "parts"
val _Term_Interpolate_prefix: String = "prefix"
val _Term_Match: String = "hydra/ext/scala/meta.Term.Match"
val _Term_Match_cases: String = "cases"
val _Term_Match_expr: String = "expr"
val _Term_Name: String = "hydra/ext/scala/meta.Term.Name"
val _Term_Name_value: String = "value"
val _Term_New: String = "hydra/ext/scala/meta.Term.New"
val _Term_NewAnonymous: String = "hydra/ext/scala/meta.Term.NewAnonymous"
val _Term_NewAnonymous_templ: String = "templ"
val _Term_New_init: String = "init"
val _Term_Param: String = "hydra/ext/scala/meta.Term.Param"
val _Term_Param_decltpe: String = "decltpe"
val _Term_Param_default: String = "default"
val _Term_Param_mods: String = "mods"
val _Term_Param_name: String = "name"
val _Term_PartialFunction: String = "hydra/ext/scala/meta.Term.PartialFunction"
val _Term_PartialFunction_cases: String = "cases"
val _Term_Placeholder: String = "hydra/ext/scala/meta.Term.Placeholder"
val _Term_PolyFunction: String = "hydra/ext/scala/meta.Term.PolyFunction"
val _Term_PolyFunction_body: String = "body"
val _Term_PolyFunction_tparams: String = "tparams"
val _Term_QuotedMacroExpr: String = "hydra/ext/scala/meta.Term.QuotedMacroExpr"
val _Term_QuotedMacroExpr_body: String = "body"
val _Term_QuotedMacroType: String = "hydra/ext/scala/meta.Term.QuotedMacroType"
val _Term_QuotedMacroType_tpe: String = "tpe"
val _Term_Ref: String = "hydra/ext/scala/meta.Term.Ref"
val _Term_Ref_anonymous: String = "anonymous"
val _Term_Ref_applyUnary: String = "applyUnary"
val _Term_Ref_name: String = "name"
val _Term_Ref_select: String = "select"
val _Term_Ref_super: String = "super"
val _Term_Ref_this: String = "this"
val _Term_Repeated: String = "hydra/ext/scala/meta.Term.Repeated"
val _Term_Repeated_expr: String = "expr"
val _Term_Return: String = "hydra/ext/scala/meta.Term.Return"
val _Term_Return_expr: String = "expr"
val _Term_Select: String = "hydra/ext/scala/meta.Term.Select"
val _Term_Select_name: String = "name"
val _Term_Select_qual: String = "qual"
val _Term_SplicedMacroExpr: String = "hydra/ext/scala/meta.Term.SplicedMacroExpr"
val _Term_SplicedMacroExpr_body: String = "body"
val _Term_Super: String = "hydra/ext/scala/meta.Term.Super"
val _Term_Super_superp: String = "superp"
val _Term_Super_thisp: String = "thisp"
val _Term_This: String = "hydra/ext/scala/meta.Term.This"
val _Term_Throw: String = "hydra/ext/scala/meta.Term.Throw"
val _Term_Throw_expr: String = "expr"
val _Term_Try: String = "hydra/ext/scala/meta.Term.Try"
val _Term_TryWithHandler: String = "hydra/ext/scala/meta.Term.TryWithHandler"
val _Term_TryWithHandler_catchp: String = "catchp"
val _Term_TryWithHandler_expr: String = "expr"
val _Term_TryWithHandler_finallyp: String = "finallyp"
val _Term_Try_catchp: String = "catchp"
val _Term_Try_expr: String = "expr"
val _Term_Try_finallyp: String = "finallyp"
val _Term_Tuple: String = "hydra/ext/scala/meta.Term.Tuple"
val _Term_Tuple_args: String = "args"
val _Term_While: String = "hydra/ext/scala/meta.Term.While"
val _Term_While_body: String = "body"
val _Term_While_expr: String = "expr"
val _Term_Xml: String = "hydra/ext/scala/meta.Term.Xml"
val _Term_Xml_args: String = "args"
val _Term_Xml_parts: String = "parts"
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
val _Term_param: String = "param"
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
