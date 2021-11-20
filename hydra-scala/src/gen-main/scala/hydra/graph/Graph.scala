/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
package hydra.graph

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
case class Graph[a] (
    /**
     * @type hydra/graph.GraphName
     */
    name: hydra.graph.GraphName,
    
    /**
     * @type list:
     *         parameterized:
     *           genericType: hydra/graph.Element
     *           parameters:
     *           - type:
     *               variable: a
     *             variable: a
     */
    elements: Seq[hydra.graph.Element[a]],
    
    /**
     * @type function:
     *         from:
     *         - parameterized:
     *             genericType: hydra/core.Term
     *             parameters:
     *             - type:
     *                 variable: a
     *               variable: a
     *         to: boolean
     */
    dataTerms: hydra.core.Term[a] => Boolean,
    
    /**
     * A reference to this graph's schema graph within the provided graph set
     * 
     * @type hydra/graph.GraphName
     */
    schemaGraph: hydra.graph.GraphName
)

val _Graph: String = "hydra/graph.Graph"
val _Graph_dataTerms: String = "dataTerms"
val _Graph_elements: String = "elements"
val _Graph_name: String = "name"
val _Graph_schemaGraph: String = "schemaGraph"
