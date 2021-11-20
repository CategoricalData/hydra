/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
package hydra.graph

/**
 * A graph element, having a name, data term (value), and schema term (type)
 */
case class Element[a] (
    /**
     * @type hydra/core.Name
     */
    name: hydra.core.Name,
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    schema: hydra.core.Term[a],
    
    /**
     * @type parameterized:
     *         genericType: hydra/core.Term
     *         parameters:
     *         - type:
     *             variable: a
     *           variable: a
     */
    data: hydra.core.Term[a]
)

val _Element: String = "hydra/graph.Element"
val _Element_data: String = "data"
val _Element_name: String = "name"
val _Element_schema: String = "schema"
