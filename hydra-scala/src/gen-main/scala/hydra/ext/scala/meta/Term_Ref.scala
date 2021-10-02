package hydra.ext.scala.meta

enum Term_Ref:
    /**
     * @type hydra/ext/scala/meta.Term.This
     */
    case `this`(value: Term_This) extends Term_Ref
    /**
     * @type hydra/ext/scala/meta.Term.Super
     */
    case `super`(value: Term_Super) extends Term_Ref
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    case name(value: Term_Name) extends Term_Ref
    /**
     * @type hydra/ext/scala/meta.Term.Anonymous
     */
    case anonymous(value: Term_Anonymous) extends Term_Ref
    /**
     * @type hydra/ext/scala/meta.Term.Select
     */
    case select(value: Term_Select) extends Term_Ref
    /**
     * @type hydra/ext/scala/meta.Term.ApplyUnary
     */
    case applyUnary(value: Term_ApplyUnary) extends Term_Ref

val _Term_Ref: String = "hydra/ext/scala/meta.Term_Ref"
val _Term_Ref_anonymous: String = "anonymous"
val _Term_Ref_applyUnary: String = "applyUnary"
val _Term_Ref_name: String = "name"
val _Term_Ref_select: String = "select"
val _Term_Ref_super: String = "super"
val _Term_Ref_this: String = "this"
