package hydra.ext.scala.meta

enum Tree:
    /**
     * @type hydra/ext/scala/meta.Ref
     */
    case ref(value: Ref) extends Tree
    /**
     * @type hydra/ext/scala/meta.Stat
     */
    case stat(value: Stat) extends Tree
    /**
     * @type hydra/ext/scala/meta.Type
     */
    case `type`(value: Type) extends Tree
    /**
     * @type hydra/ext/scala/meta.Type.Bounds
     */
    case bounds(value: Type_Bounds) extends Tree
    /**
     * @type hydra/ext/scala/meta.Pat
     */
    case pat(value: Pat) extends Tree
    /**
     * @type hydra/ext/scala/meta.Member
     */
    case member(value: Member) extends Tree
    /**
     * @type hydra/ext/scala/meta.Ctor
     */
    case ctor(value: Ctor) extends Tree
    /**
     * @type hydra/ext/scala/meta.Template
     */
    case template(value: Template) extends Tree
    /**
     * @type hydra/ext/scala/meta.Mod
     */
    case mod(value: Mod) extends Tree
    /**
     * @type hydra/ext/scala/meta.Enumerator
     */
    case enumerator(value: Enumerator) extends Tree
    /**
     * @type hydra/ext/scala/meta.Importer
     */
    case importer(value: Importer) extends Tree
    /**
     * @type hydra/ext/scala/meta.Importee
     */
    case importee(value: Importee) extends Tree
    /**
     * @type hydra/ext/scala/meta.CaseTree
     */
    case caseTree(value: CaseTree) extends Tree
    /**
     * @type hydra/ext/scala/meta.Source
     */
    case source(value: Source) extends Tree
    /**
     * @type hydra/ext/scala/meta.Quasi
     */
    case quasi(value: Quasi) extends Tree

val _Tree: String = "hydra/ext/scala/meta.Tree"
val _Tree_bounds: String = "bounds"
val _Tree_caseTree: String = "caseTree"
val _Tree_ctor: String = "ctor"
val _Tree_enumerator: String = "enumerator"
val _Tree_importee: String = "importee"
val _Tree_importer: String = "importer"
val _Tree_member: String = "member"
val _Tree_mod: String = "mod"
val _Tree_pat: String = "pat"
val _Tree_quasi: String = "quasi"
val _Tree_ref: String = "ref"
val _Tree_source: String = "source"
val _Tree_stat: String = "stat"
val _Tree_template: String = "template"
val _Tree_type: String = "type"
