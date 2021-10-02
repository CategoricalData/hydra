package hydra.ext.scala.meta

case class Term_Select(
    /**
     * @type hydra/ext/scala/meta.Term
     */
    qual: Term,
    
    /**
     * @type hydra/ext/scala/meta.Term.Name
     */
    name: Term_Name
)

val _Term_Select: String = "hydra/ext/scala/meta.Term_Select"
val _Term_Select_name: String = "name"
val _Term_Select_qual: String = "qual"
