package hydra.ext.scala.meta

enum Importee:
    case wildcard() extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Given
     */
    case `given`(value: Importee_Given) extends Importee
    case givenAll() extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Name
     */
    case name(value: Importee_Name) extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Rename
     */
    case rename(value: Importee_Rename) extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Unimport
     */
    case unimport(value: Importee_Unimport) extends Importee

val _Importee: String = "hydra/ext/scala/meta.Importee"
val _Importee_given: String = "given"
val _Importee_givenAll: String = "givenAll"
val _Importee_name: String = "name"
val _Importee_rename: String = "rename"
val _Importee_unimport: String = "unimport"
val _Importee_wildcard: String = "wildcard"
