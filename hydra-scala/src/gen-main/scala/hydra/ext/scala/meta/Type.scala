package hydra.ext.scala.meta

enum Type:
    /**
     * @type hydra/ext/scala/meta.Type.Ref
     */
    case ref(value: hydra.ext.scala.meta.Type.Ref) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.AnonymousName
     */
    case anonymousName(value: hydra.ext.scala.meta.Type.AnonymousName) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Apply
     */
    case apply(value: hydra.ext.scala.meta.Type.Apply) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ApplyInfix
     */
    case applyInfix(value: hydra.ext.scala.meta.Type.ApplyInfix) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.FunctionType
     */
    case functionType(value: hydra.ext.scala.meta.Type.FunctionType) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.PolyFunction
     */
    case polyFunction(value: hydra.ext.scala.meta.Type.PolyFunction) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ImplicitFunction
     */
    case implicitFunction(value: hydra.ext.scala.meta.Type.ImplicitFunction) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Tuple
     */
    case tuple(value: hydra.ext.scala.meta.Type.Tuple) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.With
     */
    case `with`(value: hydra.ext.scala.meta.Type.With) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.And
     */
    case and(value: hydra.ext.scala.meta.Type.And) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Or
     */
    case or(value: hydra.ext.scala.meta.Type.Or) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Refine
     */
    case refine(value: hydra.ext.scala.meta.Type.Refine) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Existential
     */
    case existential(value: hydra.ext.scala.meta.Type.Existential) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Annotate
     */
    case annotate(value: hydra.ext.scala.meta.Type.Annotate) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Lambda
     */
    case lambda(value: hydra.ext.scala.meta.Type.Lambda) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Macro
     */
    case `macro`(value: hydra.ext.scala.meta.Type.Macro) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Method
     */
    case method(value: hydra.ext.scala.meta.Type.Method) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Placeholder
     */
    case placeholder(value: hydra.ext.scala.meta.Type.Placeholder) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.ByName
     */
    case byName(value: hydra.ext.scala.meta.Type.ByName) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Repeated
     */
    case repeated(value: hydra.ext.scala.meta.Type.Repeated) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Var
     */
    case `var`(value: hydra.ext.scala.meta.Type.Var) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.TypedParam
     */
    case typedParam(value: hydra.ext.scala.meta.Type.TypedParam) extends Type
    /**
     * @type hydra/ext/scala/meta.Type.Match
     */
    case `match`(value: hydra.ext.scala.meta.Type.Match) extends Type
object Type {
    enum Ref:
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        case name(value: hydra.ext.scala.meta.Type.Name) extends Ref
        /**
         * @type hydra/ext/scala/meta.Type.Select
         * 
         * @type hydra/ext/scala/meta.Type.Select
         */
        case select(value: hydra.ext.scala.meta.Type.Select) extends Ref
        /**
         * @type hydra/ext/scala/meta.Type.Project
         * 
         * @type hydra/ext/scala/meta.Type.Project
         */
        case project(value: hydra.ext.scala.meta.Type.Project) extends Ref
        /**
         * @type hydra/ext/scala/meta.Type.Singleton
         * 
         * @type hydra/ext/scala/meta.Type.Singleton
         */
        case singleton(value: hydra.ext.scala.meta.Type.Singleton) extends Ref
    
    case class Name (
        /**
         * @type string
         * 
         * @type string
         */
        value: String
    )
    
    case class AnonymousName ()
    
    case class Select (
        /**
         * @type hydra/ext/scala/meta.Term.Ref
         * 
         * @type hydra/ext/scala/meta.Term.Ref
         */
        qual: hydra.ext.scala.meta.Term.Ref,
        
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        name: hydra.ext.scala.meta.Type.Name
    )
    
    case class Project (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        qual: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        name: hydra.ext.scala.meta.Type.Name
    )
    
    case class Singleton (
        /**
         * @type hydra/ext/scala/meta.Term.Ref
         * 
         * @type hydra/ext/scala/meta.Term.Ref
         */
        ref: hydra.ext.scala.meta.Term.Ref
    )
    
    case class Apply (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        args: Seq[hydra.ext.scala.meta.Type]
    )
    
    case class ApplyInfix (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        lhs: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        op: hydra.ext.scala.meta.Type.Name,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        rhs: hydra.ext.scala.meta.Type
    )
    
    enum FunctionType:
        /**
         * @type hydra/ext/scala/meta.Type.Function
         * 
         * @type hydra/ext/scala/meta.Type.Function
         */
        case function(value: hydra.ext.scala.meta.Type.Function) extends FunctionType
        /**
         * @type hydra/ext/scala/meta.Type.ContextFunction
         * 
         * @type hydra/ext/scala/meta.Type.ContextFunction
         */
        case contextFunction(value: hydra.ext.scala.meta.Type.ContextFunction) extends FunctionType
    
    case class Function (
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        params: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        res: hydra.ext.scala.meta.Type
    )
    
    case class PolyFunction (
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class ContextFunction (
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        params: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        res: hydra.ext.scala.meta.Type
    )
    
    case class ImplicitFunction (
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        params: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        res: hydra.ext.scala.meta.Type
    )
    
    case class Tuple (
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        args: Seq[hydra.ext.scala.meta.Type]
    )
    
    case class With (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        lhs: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        rhs: hydra.ext.scala.meta.Type
    )
    
    case class And (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        lhs: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        rhs: hydra.ext.scala.meta.Type
    )
    
    case class Or (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        lhs: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        rhs: hydra.ext.scala.meta.Type
    )
    
    case class Refine (
        /**
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        tpe: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type list: hydra/ext/scala/meta.Stat
         * 
         * @type list: hydra/ext/scala/meta.Stat
         */
        stats: Seq[hydra.ext.scala.meta.Stat]
    )
    
    case class Existential (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type list: hydra/ext/scala/meta.Stat
         * 
         * @type list: hydra/ext/scala/meta.Stat
         */
        stats: Seq[hydra.ext.scala.meta.Stat]
    )
    
    case class Annotate (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type list: hydra/ext/scala/meta.Mod.Annot
         * 
         * @type list: hydra/ext/scala/meta.Mod.Annot
         */
        annots: Seq[hydra.ext.scala.meta.Mod.Annot]
    )
    
    case class Lambda (
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class Macro (
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Method (
        /**
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         */
        paramss: Seq[Seq[hydra.ext.scala.meta.Term.Param]],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class Placeholder (
        /**
         * @type hydra/ext/scala/meta.Type.Bounds
         * 
         * @type hydra/ext/scala/meta.Type.Bounds
         */
        bounds: hydra.ext.scala.meta.Type.Bounds
    )
    
    case class Bounds (
        /**
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        lo: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        hi: Option[hydra.ext.scala.meta.Type]
    )
    
    case class ByName ()
    
    case class Repeated (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class Var (
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        name: hydra.ext.scala.meta.Type.Name
    )
    
    case class TypedParam (
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name,
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        typ: hydra.ext.scala.meta.Type
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
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type list: hydra/ext/scala/meta.Type.Bounds
         * 
         * @type list: hydra/ext/scala/meta.Type.Bounds
         */
        tbounds: Seq[hydra.ext.scala.meta.Type.Bounds],
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        vbounds: Seq[hydra.ext.scala.meta.Type],
        
        /**
         * @type list: hydra/ext/scala/meta.Type
         * 
         * @type list: hydra/ext/scala/meta.Type
         */
        cbounds: Seq[hydra.ext.scala.meta.Type]
    )
    
    case class Match (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type list: hydra/ext/scala/meta.TypeCase
         * 
         * @type list: hydra/ext/scala/meta.TypeCase
         */
        cases: Seq[hydra.ext.scala.meta.TypeCase]
    )
}

val _Type: String = "hydra/ext/scala/meta.Type"
val _Type_And: String = "hydra/ext/scala/meta.Type.And"
val _Type_And_lhs: String = "lhs"
val _Type_And_rhs: String = "rhs"
val _Type_Annotate: String = "hydra/ext/scala/meta.Type.Annotate"
val _Type_Annotate_annots: String = "annots"
val _Type_Annotate_tpe: String = "tpe"
val _Type_AnonymousName: String = "hydra/ext/scala/meta.Type.AnonymousName"
val _Type_Apply: String = "hydra/ext/scala/meta.Type.Apply"
val _Type_ApplyInfix: String = "hydra/ext/scala/meta.Type.ApplyInfix"
val _Type_ApplyInfix_lhs: String = "lhs"
val _Type_ApplyInfix_op: String = "op"
val _Type_ApplyInfix_rhs: String = "rhs"
val _Type_Apply_args: String = "args"
val _Type_Apply_tpe: String = "tpe"
val _Type_Bounds: String = "hydra/ext/scala/meta.Type.Bounds"
val _Type_Bounds_hi: String = "hi"
val _Type_Bounds_lo: String = "lo"
val _Type_ByName: String = "hydra/ext/scala/meta.Type.ByName"
val _Type_ContextFunction: String = "hydra/ext/scala/meta.Type.ContextFunction"
val _Type_ContextFunction_params: String = "params"
val _Type_ContextFunction_res: String = "res"
val _Type_Existential: String = "hydra/ext/scala/meta.Type.Existential"
val _Type_Existential_stats: String = "stats"
val _Type_Existential_tpe: String = "tpe"
val _Type_Function: String = "hydra/ext/scala/meta.Type.Function"
val _Type_FunctionType: String = "hydra/ext/scala/meta.Type.FunctionType"
val _Type_FunctionType_contextFunction: String = "contextFunction"
val _Type_FunctionType_function: String = "function"
val _Type_Function_params: String = "params"
val _Type_Function_res: String = "res"
val _Type_ImplicitFunction: String = "hydra/ext/scala/meta.Type.ImplicitFunction"
val _Type_ImplicitFunction_params: String = "params"
val _Type_ImplicitFunction_res: String = "res"
val _Type_Lambda: String = "hydra/ext/scala/meta.Type.Lambda"
val _Type_Lambda_tparams: String = "tparams"
val _Type_Lambda_tpe: String = "tpe"
val _Type_Macro: String = "hydra/ext/scala/meta.Type.Macro"
val _Type_Macro_body: String = "body"
val _Type_Match: String = "hydra/ext/scala/meta.Type.Match"
val _Type_Match_cases: String = "cases"
val _Type_Match_tpe: String = "tpe"
val _Type_Method: String = "hydra/ext/scala/meta.Type.Method"
val _Type_Method_paramss: String = "paramss"
val _Type_Method_tpe: String = "tpe"
val _Type_Name: String = "hydra/ext/scala/meta.Type.Name"
val _Type_Name_value: String = "value"
val _Type_Or: String = "hydra/ext/scala/meta.Type.Or"
val _Type_Or_lhs: String = "lhs"
val _Type_Or_rhs: String = "rhs"
val _Type_Param: String = "hydra/ext/scala/meta.Type.Param"
val _Type_Param_cbounds: String = "cbounds"
val _Type_Param_mods: String = "mods"
val _Type_Param_name: String = "name"
val _Type_Param_tbounds: String = "tbounds"
val _Type_Param_tparams: String = "tparams"
val _Type_Param_vbounds: String = "vbounds"
val _Type_Placeholder: String = "hydra/ext/scala/meta.Type.Placeholder"
val _Type_Placeholder_bounds: String = "bounds"
val _Type_PolyFunction: String = "hydra/ext/scala/meta.Type.PolyFunction"
val _Type_PolyFunction_tparams: String = "tparams"
val _Type_PolyFunction_tpe: String = "tpe"
val _Type_Project: String = "hydra/ext/scala/meta.Type.Project"
val _Type_Project_name: String = "name"
val _Type_Project_qual: String = "qual"
val _Type_Ref: String = "hydra/ext/scala/meta.Type.Ref"
val _Type_Ref_name: String = "name"
val _Type_Ref_project: String = "project"
val _Type_Ref_select: String = "select"
val _Type_Ref_singleton: String = "singleton"
val _Type_Refine: String = "hydra/ext/scala/meta.Type.Refine"
val _Type_Refine_stats: String = "stats"
val _Type_Refine_tpe: String = "tpe"
val _Type_Repeated: String = "hydra/ext/scala/meta.Type.Repeated"
val _Type_Repeated_tpe: String = "tpe"
val _Type_Select: String = "hydra/ext/scala/meta.Type.Select"
val _Type_Select_name: String = "name"
val _Type_Select_qual: String = "qual"
val _Type_Singleton: String = "hydra/ext/scala/meta.Type.Singleton"
val _Type_Singleton_ref: String = "ref"
val _Type_Tuple: String = "hydra/ext/scala/meta.Type.Tuple"
val _Type_Tuple_args: String = "args"
val _Type_TypedParam: String = "hydra/ext/scala/meta.Type.TypedParam"
val _Type_TypedParam_name: String = "name"
val _Type_TypedParam_typ: String = "typ"
val _Type_Var: String = "hydra/ext/scala/meta.Type.Var"
val _Type_Var_name: String = "name"
val _Type_With: String = "hydra/ext/scala/meta.Type.With"
val _Type_With_lhs: String = "lhs"
val _Type_With_rhs: String = "rhs"
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
