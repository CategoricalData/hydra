package hydra.ext.scala.meta

enum Decl:
    /**
     * @type hydra/ext/scala/meta.Decl.Val
     */
    case `val`(value: hydra.ext.scala.meta.Decl.Val) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Var
     */
    case `var`(value: hydra.ext.scala.meta.Decl.Var) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Def
     */
    case `def`(value: hydra.ext.scala.meta.Decl.Def) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Type
     */
    case `type`(value: hydra.ext.scala.meta.Decl.Type) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Given
     */
    case `given`(value: hydra.ext.scala.meta.Decl.Given) extends Decl
object Decl {
    case class Val (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        pats: Seq[hydra.ext.scala.meta.Pat],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        decltpe: hydra.ext.scala.meta.Type
    )
    
    case class Var (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type list: hydra/ext/scala/meta.Pat
         * 
         * @type list: hydra/ext/scala/meta.Pat
         */
        pats: Seq[hydra.ext.scala.meta.Pat],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        decltpe: hydra.ext.scala.meta.Type
    )
    
    case class Def (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
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
        decltpe: hydra.ext.scala.meta.Type
    )
    
    case class Type (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        name: hydra.ext.scala.meta.Type.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type hydra/ext/scala/meta.Type.Bounds
         * 
         * @type hydra/ext/scala/meta.Type.Bounds
         */
        bounds: hydra.ext.scala.meta.Type.Bounds
    )
    
    case class Given (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name,
        
        /**
         * @type list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[hydra.ext.scala.meta.Type.Param],
        
        /**
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         */
        sparams: Seq[Seq[hydra.ext.scala.meta.Term.Param]],
        
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        decltpe: hydra.ext.scala.meta.Type
    )
}

val _Decl: String = "hydra/ext/scala/meta.Decl"
val _Decl_Def: String = "hydra/ext/scala/meta.Decl.Def"
val _Decl_Def_decltpe: String = "decltpe"
val _Decl_Def_mods: String = "mods"
val _Decl_Def_name: String = "name"
val _Decl_Def_paramss: String = "paramss"
val _Decl_Def_tparams: String = "tparams"
val _Decl_Given: String = "hydra/ext/scala/meta.Decl.Given"
val _Decl_Given_decltpe: String = "decltpe"
val _Decl_Given_mods: String = "mods"
val _Decl_Given_name: String = "name"
val _Decl_Given_sparams: String = "sparams"
val _Decl_Given_tparams: String = "tparams"
val _Decl_Type: String = "hydra/ext/scala/meta.Decl.Type"
val _Decl_Type_bounds: String = "bounds"
val _Decl_Type_mods: String = "mods"
val _Decl_Type_name: String = "name"
val _Decl_Type_tparams: String = "tparams"
val _Decl_Val: String = "hydra/ext/scala/meta.Decl.Val"
val _Decl_Val_decltpe: String = "decltpe"
val _Decl_Val_mods: String = "mods"
val _Decl_Val_pats: String = "pats"
val _Decl_Var: String = "hydra/ext/scala/meta.Decl.Var"
val _Decl_Var_decltpe: String = "decltpe"
val _Decl_Var_mods: String = "mods"
val _Decl_Var_pats: String = "pats"
val _Decl_def: String = "def"
val _Decl_given: String = "given"
val _Decl_type: String = "type"
val _Decl_val: String = "val"
val _Decl_var: String = "var"
