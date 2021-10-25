package hydra.ext.scala.meta

enum Member:
    /**
     * @type hydra/ext/scala/meta.Member.Term
     */
    case term(value: hydra.ext.scala.meta.Member.Term) extends Member
    /**
     * @type hydra/ext/scala/meta.Member.Type
     */
    case `type`(value: hydra.ext.scala.meta.Member.Type) extends Member
    /**
     * @type hydra/ext/scala/meta.Term.Param
     */
    case termParam(value: hydra.ext.scala.meta.Term.Param) extends Member
    /**
     * @type hydra/ext/scala/meta.Type.Param
     */
    case typeParam(value: hydra.ext.scala.meta.Type.Param) extends Member
    /**
     * @type hydra/ext/scala/meta.Self
     */
    case self(value: hydra.ext.scala.meta.Self) extends Member
object Member {
    enum Term:
        /**
         * @type hydra/ext/scala/meta.Pkg
         * 
         * @type hydra/ext/scala/meta.Pkg
         */
        case pkg(value: hydra.ext.scala.meta.Pkg) extends Term
        /**
         * @type hydra/ext/scala/meta.Pkg.Object
         * 
         * @type hydra/ext/scala/meta.Pkg.Object
         */
        case `object`(value: hydra.ext.scala.meta.Pkg.Object) extends Term
    
    case class Type (
        /**
         * @type hydra/ext/scala/meta.Type.Name
         * 
         * @type hydra/ext/scala/meta.Type.Name
         */
        name: hydra.ext.scala.meta.Type.Name
    )
}

val _Member: String = "hydra/ext/scala/meta.Member"
val _Member_Term: String = "hydra/ext/scala/meta.Member.Term"
val _Member_Term_object: String = "object"
val _Member_Term_pkg: String = "pkg"
val _Member_Type: String = "hydra/ext/scala/meta.Member.Type"
val _Member_Type_name: String = "name"
val _Member_self: String = "self"
val _Member_term: String = "term"
val _Member_termParam: String = "termParam"
val _Member_type: String = "type"
val _Member_typeParam: String = "typeParam"
