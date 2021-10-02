package hydra.ext.scala.meta

enum Member_Term:
    /**
     * @type hydra/ext/scala/meta.Pkg
     */
    case pkg(value: Pkg) extends Member_Term
    /**
     * @type hydra/ext/scala/meta.Pkg.Object
     */
    case `object`(value: Pkg_Object) extends Member_Term

val _Member_Term: String = "hydra/ext/scala/meta.Member_Term"
val _Member_Term_object: String = "object"
val _Member_Term_pkg: String = "pkg"
