package hydra.ext.scala.meta

enum Member:
    /**
     * @type hydra/ext/scala/meta.Member.Term
     */
    case term(value: Member_Term) extends Member
    /**
     * @type hydra/ext/scala/meta.Member.Type
     */
    case `type`(value: Member_Type) extends Member
    /**
     * @type hydra/ext/scala/meta.Term.Param
     */
    case termParam(value: Term_Param) extends Member
    /**
     * @type hydra/ext/scala/meta.Type.Param
     */
    case typeParam(value: Type_Param) extends Member
    /**
     * @type hydra/ext/scala/meta.Self
     */
    case self(value: Self) extends Member

val _Member: String = "hydra/ext/scala/meta.Member"
val _Member_self: String = "self"
val _Member_term: String = "term"
val _Member_termParam: String = "termParam"
val _Member_type: String = "type"
val _Member_typeParam: String = "typeParam"
