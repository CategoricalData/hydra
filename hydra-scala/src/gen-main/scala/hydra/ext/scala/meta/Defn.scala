package hydra.ext.scala.meta

enum Defn:
    /**
     * @type hydra/ext/scala/meta.Defn.Val
     */
    case `val`(value: hydra.ext.scala.meta.Defn.Val) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Var
     */
    case `var`(value: hydra.ext.scala.meta.Defn.Var) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Given
     */
    case `given`(value: hydra.ext.scala.meta.Defn.Given) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Enum
     */
    case `enum`(value: hydra.ext.scala.meta.Defn.Enum) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.EnumCase
     */
    case enumCase(value: hydra.ext.scala.meta.Defn.EnumCase) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.RepeatedEnumCase
     */
    case repeatedEnumCase(value: hydra.ext.scala.meta.Defn.RepeatedEnumCase) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.GivenAlias
     */
    case givenAlias(value: hydra.ext.scala.meta.Defn.GivenAlias) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.ExtensionGroup
     */
    case extensionGroup(value: hydra.ext.scala.meta.Defn.ExtensionGroup) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Def
     */
    case `def`(value: hydra.ext.scala.meta.Defn.Def) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Macro
     */
    case `macro`(value: hydra.ext.scala.meta.Defn.Macro) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Type
     */
    case `type`(value: hydra.ext.scala.meta.Defn.Type) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Class
     */
    case `class`(value: hydra.ext.scala.meta.Defn.Class) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Trait
     */
    case `trait`(value: hydra.ext.scala.meta.Defn.Trait) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Object
     */
    case `object`(value: hydra.ext.scala.meta.Defn.Object) extends Defn
object Defn {
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
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        decltpe: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        rhs: hydra.ext.scala.meta.Term
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
        decltpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type optional: hydra/ext/scala/meta.Term
         * 
         * @type optional: hydra/ext/scala/meta.Term
         */
        rhs: Option[hydra.ext.scala.meta.Term]
    )
    
    case class Given (
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
         * @type list:
         *         list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[Seq[hydra.ext.scala.meta.Type.Param]],
        
        /**
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Term.Param
         */
        sparams: Seq[Seq[hydra.ext.scala.meta.Term.Param]],
        
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        templ: hydra.ext.scala.meta.Template
    )
    
    case class Enum (
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
         * @type hydra/ext/scala/meta.Ctor.Primary
         * 
         * @type hydra/ext/scala/meta.Ctor.Primary
         */
        ctor: hydra.ext.scala.meta.Ctor.Primary,
        
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        template: hydra.ext.scala.meta.Template
    )
    
    case class EnumCase (
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
         * @type hydra/ext/scala/meta.Ctor.Primary
         * 
         * @type hydra/ext/scala/meta.Ctor.Primary
         */
        ctor: hydra.ext.scala.meta.Ctor.Primary,
        
        /**
         * @type list: hydra/ext/scala/meta.Init
         * 
         * @type list: hydra/ext/scala/meta.Init
         */
        inits: Seq[hydra.ext.scala.meta.Init]
    )
    
    case class RepeatedEnumCase (
        /**
         * @type list: hydra/ext/scala/meta.Mod
         * 
         * @type list: hydra/ext/scala/meta.Mod
         */
        mods: Seq[hydra.ext.scala.meta.Mod],
        
        /**
         * @type list: hydra/ext/scala/meta.Term.Name
         * 
         * @type list: hydra/ext/scala/meta.Term.Name
         */
        cases: Seq[hydra.ext.scala.meta.Term.Name]
    )
    
    case class GivenAlias (
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
         * @type list:
         *         list: hydra/ext/scala/meta.Type.Param
         * 
         * @type list:
         *         list: hydra/ext/scala/meta.Type.Param
         */
        tparams: Seq[Seq[hydra.ext.scala.meta.Type.Param]],
        
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
        decltpe: hydra.ext.scala.meta.Type,
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class ExtensionGroup (
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
         * @type hydra/ext/scala/meta.Stat
         * 
         * @type hydra/ext/scala/meta.Stat
         */
        body: hydra.ext.scala.meta.Stat
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
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        decltpe: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
    )
    
    case class Macro (
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
         * @type optional: hydra/ext/scala/meta.Type
         * 
         * @type optional: hydra/ext/scala/meta.Type
         */
        decltpe: Option[hydra.ext.scala.meta.Type],
        
        /**
         * @type hydra/ext/scala/meta.Term
         * 
         * @type hydra/ext/scala/meta.Term
         */
        body: hydra.ext.scala.meta.Term
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
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        body: hydra.ext.scala.meta.Type
    )
    
    case class Class (
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
         * @type hydra/ext/scala/meta.Ctor.Primary
         * 
         * @type hydra/ext/scala/meta.Ctor.Primary
         */
        ctor: hydra.ext.scala.meta.Ctor.Primary,
        
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        template: hydra.ext.scala.meta.Template
    )
    
    case class Trait (
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
         * @type hydra/ext/scala/meta.Ctor.Primary
         * 
         * @type hydra/ext/scala/meta.Ctor.Primary
         */
        ctor: hydra.ext.scala.meta.Ctor.Primary,
        
        /**
         * @type hydra/ext/scala/meta.Template
         * 
         * @type hydra/ext/scala/meta.Template
         */
        template: hydra.ext.scala.meta.Template
    )
    
    case class Object (
        /**
         * @type hydra/ext/scala/meta.Term.Name
         * 
         * @type hydra/ext/scala/meta.Term.Name
         */
        name: hydra.ext.scala.meta.Term.Name
    )
}

val _Defn: String = "hydra/ext/scala/meta.Defn"
val _Defn_Class: String = "hydra/ext/scala/meta.Defn.Class"
val _Defn_Class_ctor: String = "ctor"
val _Defn_Class_mods: String = "mods"
val _Defn_Class_name: String = "name"
val _Defn_Class_template: String = "template"
val _Defn_Class_tparams: String = "tparams"
val _Defn_Def: String = "hydra/ext/scala/meta.Defn.Def"
val _Defn_Def_body: String = "body"
val _Defn_Def_decltpe: String = "decltpe"
val _Defn_Def_mods: String = "mods"
val _Defn_Def_name: String = "name"
val _Defn_Def_paramss: String = "paramss"
val _Defn_Def_tparams: String = "tparams"
val _Defn_Enum: String = "hydra/ext/scala/meta.Defn.Enum"
val _Defn_EnumCase: String = "hydra/ext/scala/meta.Defn.EnumCase"
val _Defn_EnumCase_ctor: String = "ctor"
val _Defn_EnumCase_inits: String = "inits"
val _Defn_EnumCase_mods: String = "mods"
val _Defn_EnumCase_name: String = "name"
val _Defn_EnumCase_tparams: String = "tparams"
val _Defn_Enum_ctor: String = "ctor"
val _Defn_Enum_mods: String = "mods"
val _Defn_Enum_name: String = "name"
val _Defn_Enum_template: String = "template"
val _Defn_Enum_tparams: String = "tparams"
val _Defn_ExtensionGroup: String = "hydra/ext/scala/meta.Defn.ExtensionGroup"
val _Defn_ExtensionGroup_body: String = "body"
val _Defn_ExtensionGroup_paramss: String = "paramss"
val _Defn_ExtensionGroup_tparams: String = "tparams"
val _Defn_Given: String = "hydra/ext/scala/meta.Defn.Given"
val _Defn_GivenAlias: String = "hydra/ext/scala/meta.Defn.GivenAlias"
val _Defn_GivenAlias_body: String = "body"
val _Defn_GivenAlias_decltpe: String = "decltpe"
val _Defn_GivenAlias_mods: String = "mods"
val _Defn_GivenAlias_name: String = "name"
val _Defn_GivenAlias_sparams: String = "sparams"
val _Defn_GivenAlias_tparams: String = "tparams"
val _Defn_Given_mods: String = "mods"
val _Defn_Given_name: String = "name"
val _Defn_Given_sparams: String = "sparams"
val _Defn_Given_templ: String = "templ"
val _Defn_Given_tparams: String = "tparams"
val _Defn_Macro: String = "hydra/ext/scala/meta.Defn.Macro"
val _Defn_Macro_body: String = "body"
val _Defn_Macro_decltpe: String = "decltpe"
val _Defn_Macro_mods: String = "mods"
val _Defn_Macro_name: String = "name"
val _Defn_Macro_paramss: String = "paramss"
val _Defn_Macro_tparams: String = "tparams"
val _Defn_Object: String = "hydra/ext/scala/meta.Defn.Object"
val _Defn_Object_name: String = "name"
val _Defn_RepeatedEnumCase: String = "hydra/ext/scala/meta.Defn.RepeatedEnumCase"
val _Defn_RepeatedEnumCase_cases: String = "cases"
val _Defn_RepeatedEnumCase_mods: String = "mods"
val _Defn_Trait: String = "hydra/ext/scala/meta.Defn.Trait"
val _Defn_Trait_ctor: String = "ctor"
val _Defn_Trait_mods: String = "mods"
val _Defn_Trait_name: String = "name"
val _Defn_Trait_template: String = "template"
val _Defn_Trait_tparams: String = "tparams"
val _Defn_Type: String = "hydra/ext/scala/meta.Defn.Type"
val _Defn_Type_body: String = "body"
val _Defn_Type_mods: String = "mods"
val _Defn_Type_name: String = "name"
val _Defn_Type_tparams: String = "tparams"
val _Defn_Val: String = "hydra/ext/scala/meta.Defn.Val"
val _Defn_Val_decltpe: String = "decltpe"
val _Defn_Val_mods: String = "mods"
val _Defn_Val_pats: String = "pats"
val _Defn_Val_rhs: String = "rhs"
val _Defn_Var: String = "hydra/ext/scala/meta.Defn.Var"
val _Defn_Var_decltpe: String = "decltpe"
val _Defn_Var_mods: String = "mods"
val _Defn_Var_pats: String = "pats"
val _Defn_Var_rhs: String = "rhs"
val _Defn_class: String = "class"
val _Defn_def: String = "def"
val _Defn_enum: String = "enum"
val _Defn_enumCase: String = "enumCase"
val _Defn_extensionGroup: String = "extensionGroup"
val _Defn_given: String = "given"
val _Defn_givenAlias: String = "givenAlias"
val _Defn_macro: String = "macro"
val _Defn_object: String = "object"
val _Defn_repeatedEnumCase: String = "repeatedEnumCase"
val _Defn_trait: String = "trait"
val _Defn_type: String = "type"
val _Defn_val: String = "val"
val _Defn_var: String = "var"
