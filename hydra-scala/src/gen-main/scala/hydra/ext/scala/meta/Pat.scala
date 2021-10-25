package hydra.ext.scala.meta

enum Pat:
    /**
     * @type hydra/ext/scala/meta.Pat.Var
     */
    case `var`(value: hydra.ext.scala.meta.Pat.Var) extends Pat
    case wildcard() extends Pat
    case seqWildcard() extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Bind
     */
    case bind(value: hydra.ext.scala.meta.Pat.Bind) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Alternative
     */
    case alternative(value: hydra.ext.scala.meta.Pat.Alternative) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Tuple
     */
    case tuple(value: hydra.ext.scala.meta.Pat.Tuple) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Repeated
     */
    case repeated(value: hydra.ext.scala.meta.Pat.Repeated) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Extract
     */
    case extract(value: hydra.ext.scala.meta.Pat.Extract) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.ExtractInfix
     */
    case extractInfix(value: hydra.ext.scala.meta.Pat.ExtractInfix) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Interpolate
     */
    case interpolate(value: hydra.ext.scala.meta.Pat.Interpolate) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Xml
     */
    case xml(value: hydra.ext.scala.meta.Pat.Xml) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Typed
     */
    case typed(value: hydra.ext.scala.meta.Pat.Typed) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Macro
     */
    case `macro`(value: hydra.ext.scala.meta.Pat.Macro) extends Pat
    /**
     * @type hydra/ext/scala/meta.Pat.Given
     */
    case `given`(value: hydra.ext.scala.meta.Pat.Given) extends Pat
object Pat {
    case class Var (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name
    )
    
    case class Bind (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        lhs: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        rhs: hydra.ext.scala.meta.Pat
    )
    
    case class Alternative (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        lhs: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        rhs: hydra.ext.scala.meta.Pat
    )
    
    case class Tuple (
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        args: Seq[hydra.ext.scala.meta.Pat]
    )
    
    case class Repeated (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name
    )
    
    case class Extract (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        fun: hydra.ext.scala.meta.Term,
        
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        args: Seq[hydra.ext.scala.meta.Pat]
    )
    
    case class ExtractInfix (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        lhs: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        op: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        rhs: Seq[hydra.ext.scala.meta.Pat]
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
        parts: Seq[hydra.ext.scala.meta.Lit]
    )
    
    case class Xml (
        /**
         * @type list: hydra/ext/scala/meta.Lit
         * 
         * @type list: hydra/ext/scala/meta.Lit
         */
        parts: Seq[hydra.ext.scala.meta.Lit],
        
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        args: Seq[hydra.ext.scala.meta.Pat]
    )
    
    case class Typed (
        /**
         * @type hydra/ext/scala/meta.Pat
         * 
         * @type hydra/ext/scala/meta.Pat
         */
        lhs: hydra.ext.scala.meta.Pat,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        rhs: hydra.ext.scala.meta.Type
    )
    
    case class Macro (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Given (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
}

val _Pat: String = "hydra/ext/scala/meta.Pat"
val _Pat_Alternative: String = "hydra/ext/scala/meta.Pat.Alternative"
val _Pat_Alternative_lhs: String = "lhs"
val _Pat_Alternative_rhs: String = "rhs"
val _Pat_Bind: String = "hydra/ext/scala/meta.Pat.Bind"
val _Pat_Bind_lhs: String = "lhs"
val _Pat_Bind_rhs: String = "rhs"
val _Pat_Extract: String = "hydra/ext/scala/meta.Pat.Extract"
val _Pat_ExtractInfix: String = "hydra/ext/scala/meta.Pat.ExtractInfix"
val _Pat_ExtractInfix_lhs: String = "lhs"
val _Pat_ExtractInfix_op: String = "op"
val _Pat_ExtractInfix_rhs: String = "rhs"
val _Pat_Extract_args: String = "args"
val _Pat_Extract_fun: String = "fun"
val _Pat_Given: String = "hydra/ext/scala/meta.Pat.Given"
val _Pat_Given_tpe: String = "tpe"
val _Pat_Interpolate: String = "hydra/ext/scala/meta.Pat.Interpolate"
val _Pat_Interpolate_parts: String = "parts"
val _Pat_Interpolate_prefix: String = "prefix"
val _Pat_Macro: String = "hydra/ext/scala/meta.Pat.Macro"
val _Pat_Macro_body: String = "body"
val _Pat_Repeated: String = "hydra/ext/scala/meta.Pat.Repeated"
val _Pat_Repeated_name: String = "name"
val _Pat_Tuple: String = "hydra/ext/scala/meta.Pat.Tuple"
val _Pat_Tuple_args: String = "args"
val _Pat_Typed: String = "hydra/ext/scala/meta.Pat.Typed"
val _Pat_Typed_lhs: String = "lhs"
val _Pat_Typed_rhs: String = "rhs"
val _Pat_Var: String = "hydra/ext/scala/meta.Pat.Var"
val _Pat_Var_name: String = "name"
val _Pat_Xml: String = "hydra/ext/scala/meta.Pat.Xml"
val _Pat_Xml_args: String = "args"
val _Pat_Xml_parts: String = "parts"
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
