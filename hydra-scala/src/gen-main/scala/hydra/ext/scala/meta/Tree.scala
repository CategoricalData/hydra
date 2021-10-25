package hydra.ext.scala.meta

enum Tree:
    /**
     * @type hydra/ext/scala/meta.Ref
     */
    case ref(value: hydra.ext.scala.meta.Ref) extends Tree
    /**
     * @type hydra/ext/scala/meta.Stat
     */
    case stat(value: hydra.ext.scala.meta.Stat) extends Tree
    /**
     * @type hydra/ext/scala/meta.Type
     */
    case `type`(value: hydra.ext.scala.meta.Type) extends Tree
    /**
     * @type hydra/ext/scala/meta.Type.Bounds
     */
    case bounds(value: hydra.ext.scala.meta.Type.Bounds) extends Tree
    /**
     * @type hydra/ext/scala/meta.Pat
     */
    case pat(value: hydra.ext.scala.meta.Pat) extends Tree
    /**
     * @type hydra/ext/scala/meta.Member
     */
    case member(value: hydra.ext.scala.meta.Member) extends Tree
    /**
     * @type hydra/ext/scala/meta.Ctor
     */
    case ctor(value: hydra.ext.scala.meta.Ctor) extends Tree
    /**
     * @type hydra/ext/scala/meta.Template
     */
    case template(value: hydra.ext.scala.meta.Template) extends Tree
    /**
     * @type hydra/ext/scala/meta.Mod
     */
    case mod(value: hydra.ext.scala.meta.Mod) extends Tree
    /**
     * @type hydra/ext/scala/meta.Enumerator
     */
    case enumerator(value: hydra.ext.scala.meta.Enumerator) extends Tree
    /**
     * @type hydra/ext/scala/meta.Importer
     */
    case importer(value: hydra.ext.scala.meta.Importer) extends Tree
    /**
     * @type hydra/ext/scala/meta.Importee
     */
    case importee(value: hydra.ext.scala.meta.Importee) extends Tree
    /**
     * @type hydra/ext/scala/meta.CaseTree
     */
    case caseTree(value: hydra.ext.scala.meta.CaseTree) extends Tree
    /**
     * @type hydra/ext/scala/meta.Source
     */
    case source(value: hydra.ext.scala.meta.Source) extends Tree
    /**
     * @type hydra/ext/scala/meta.Quasi
     */
    case quasi(value: hydra.ext.scala.meta.Quasi) extends Tree

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
