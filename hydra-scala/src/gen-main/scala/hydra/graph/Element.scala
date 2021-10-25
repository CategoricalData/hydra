/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
package hydra.graph

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
case class Element (
    /**
     * @type hydra/core.Name
     */
    name: hydra.core.Name,
    
    /**
     * @type hydra/core.Term
     */
    schema: hydra.core.Term,
    
    /**
     * @type hydra/core.Term
     */
    data: hydra.core.Term
)

val _Element: String = "hydra/graph.Element"
val _Element_data: String = "data"
val _Element_name: String = "name"
val _Element_schema: String = "schema"
