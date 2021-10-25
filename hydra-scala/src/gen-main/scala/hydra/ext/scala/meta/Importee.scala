package hydra.ext.scala.meta

enum Importee:
    case wildcard() extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Given
     */
    case `given`(value: hydra.ext.scala.meta.Importee.Given) extends Importee
    case givenAll() extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Name
     */
    case name(value: hydra.ext.scala.meta.Importee.Name) extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Rename
     */
    case rename(value: hydra.ext.scala.meta.Importee.Rename) extends Importee
    /**
     * @type hydra/ext/scala/meta.Importee.Unimport
     */
    case unimport(value: hydra.ext.scala.meta.Importee.Unimport) extends Importee
object Importee {
    case class Given (
        /**
         * @type hydra/ext/scala/meta.Type
         * 
         * @type hydra/ext/scala/meta.Type
         */
        tpe: hydra.ext.scala.meta.Type
    )
    
    case class Name (
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name
    )
    
    case class Rename (
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name,
        
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        rename: hydra.ext.scala.meta.Name
    )
    
    case class Unimport (
        /**
         * @type hydra/ext/scala/meta.Name
         * 
         * @type hydra/ext/scala/meta.Name
         */
        name: hydra.ext.scala.meta.Name
    )
}

val _Importee: String = "hydra/ext/scala/meta.Importee"
val _Importee_Given: String = "hydra/ext/scala/meta.Importee.Given"
val _Importee_Given_tpe: String = "tpe"
val _Importee_Name: String = "hydra/ext/scala/meta.Importee.Name"
val _Importee_Name_name: String = "name"
val _Importee_Rename: String = "hydra/ext/scala/meta.Importee.Rename"
val _Importee_Rename_name: String = "name"
val _Importee_Rename_rename: String = "rename"
val _Importee_Unimport: String = "hydra/ext/scala/meta.Importee.Unimport"
val _Importee_Unimport_name: String = "name"
val _Importee_given: String = "given"
val _Importee_givenAll: String = "givenAll"
val _Importee_name: String = "name"
val _Importee_rename: String = "rename"
val _Importee_unimport: String = "unimport"
val _Importee_wildcard: String = "wildcard"
