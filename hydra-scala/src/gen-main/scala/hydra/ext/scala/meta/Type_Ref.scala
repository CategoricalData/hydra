package hydra.ext.scala.meta

enum Type_Ref:
    /**
     * @type hydra/ext/scala/meta.Type.Name
     */
    case name(value: Type_Name) extends Type_Ref
    /**
     * @type hydra/ext/scala/meta.Type.Select
     */
    case select(value: Type_Select) extends Type_Ref
    /**
     * @type hydra/ext/scala/meta.Type.Project
     */
    case project(value: Type_Project) extends Type_Ref
    /**
     * @type hydra/ext/scala/meta.Type.Singleton
     */
    case singleton(value: Type_Singleton) extends Type_Ref

val _Type_Ref: String = "hydra/ext/scala/meta.Type_Ref"
val _Type_Ref_name: String = "name"
val _Type_Ref_project: String = "project"
val _Type_Ref_select: String = "select"
val _Type_Ref_singleton: String = "singleton"
