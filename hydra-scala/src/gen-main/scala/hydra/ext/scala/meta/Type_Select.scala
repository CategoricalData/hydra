package hydra.ext.scala.meta

case class Type_Select(
    /**
     * @type hydra/ext/scala/meta.Term.Ref
     */
    qual: Term_Ref,
    
    /**
     * @type hydra/ext/scala/meta.Type.Name
     */
    name: Type_Name
)

val _Type_Select: String = "hydra/ext/scala/meta.Type_Select"
val _Type_Select_name: String = "name"
val _Type_Select_qual: String = "qual"
