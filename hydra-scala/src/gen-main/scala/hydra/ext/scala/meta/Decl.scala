package hydra.ext.scala.meta

enum Decl:
    /**
     * @type hydra/ext/scala/meta.Decl.Val
     */
    case `val`(value: Decl_Val) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Var
     */
    case `var`(value: Decl_Var) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Def
     */
    case `def`(value: Decl_Def) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Type
     */
    case `type`(value: Decl_Type) extends Decl
    /**
     * @type hydra/ext/scala/meta.Decl.Given
     */
    case `given`(value: Decl_Given) extends Decl

val _Decl: String = "hydra/ext/scala/meta.Decl"
val _Decl_def: String = "def"
val _Decl_given: String = "given"
val _Decl_type: String = "type"
val _Decl_val: String = "val"
val _Decl_var: String = "var"
