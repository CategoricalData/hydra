package hydra.ext.scala.meta

enum Mod:
    /**
     * @type hydra/ext/scala/meta.Mod.Annot
     */
    case annot(value: Mod_Annot) extends Mod
    /**
     * @type hydra/ext/scala/meta.Mod.Private
     */
    case `private`(value: Mod_Private) extends Mod
    /**
     * @type hydra/ext/scala/meta.Mod.Protected
     */
    case `protected`(value: Mod_Protected) extends Mod
    case `implicit`() extends Mod
    case `final`() extends Mod
    case `sealed`() extends Mod
    case open() extends Mod
    case `super`() extends Mod
    case `override`() extends Mod
    case `case`() extends Mod
    case `abstract`() extends Mod
    case covariant() extends Mod
    case contravariant() extends Mod
    case `lazy`() extends Mod
    case valParam() extends Mod
    case varParam() extends Mod
    case infix() extends Mod
    case inline() extends Mod
    case using() extends Mod
    case opaque() extends Mod
    case transparent() extends Mod

val _Mod: String = "hydra/ext/scala/meta.Mod"
val _Mod_abstract: String = "abstract"
val _Mod_annot: String = "annot"
val _Mod_case: String = "case"
val _Mod_contravariant: String = "contravariant"
val _Mod_covariant: String = "covariant"
val _Mod_final: String = "final"
val _Mod_implicit: String = "implicit"
val _Mod_infix: String = "infix"
val _Mod_inline: String = "inline"
val _Mod_lazy: String = "lazy"
val _Mod_opaque: String = "opaque"
val _Mod_open: String = "open"
val _Mod_override: String = "override"
val _Mod_private: String = "private"
val _Mod_protected: String = "protected"
val _Mod_sealed: String = "sealed"
val _Mod_super: String = "super"
val _Mod_transparent: String = "transparent"
val _Mod_using: String = "using"
val _Mod_valParam: String = "valParam"
val _Mod_varParam: String = "varParam"
