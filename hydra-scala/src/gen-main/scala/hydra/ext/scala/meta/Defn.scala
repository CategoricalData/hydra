package hydra.ext.scala.meta

enum Defn:
    /**
     * @type hydra/ext/scala/meta.Defn.Val
     */
    case `val`(value: Defn_Val) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Var
     */
    case `var`(value: Defn_Var) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Given
     */
    case `given`(value: Defn_Given) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Enum
     */
    case `enum`(value: Defn_Enum) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.EnumCase
     */
    case enumCase(value: Defn_EnumCase) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.RepeatedEnumCase
     */
    case repeatedEnumCase(value: Defn_RepeatedEnumCase) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.GivenAlias
     */
    case givenAlias(value: Defn_GivenAlias) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.ExtensionGroup
     */
    case extensionGroup(value: Defn_ExtensionGroup) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Def
     */
    case `def`(value: Defn_Def) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Macro
     */
    case `macro`(value: Defn_Macro) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Type
     */
    case `type`(value: Defn_Type) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Class
     */
    case `class`(value: Defn_Class) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Trait
     */
    case `trait`(value: Defn_Trait) extends Defn
    /**
     * @type hydra/ext/scala/meta.Defn.Object
     */
    case `object`(value: Defn_Object) extends Defn

val _Defn: String = "hydra/ext/scala/meta.Defn"
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
