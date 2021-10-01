/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
package hydra.graph

import hydra.core.Term

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
case class Graph(
    /**
     * @type hydra/graph.GraphName
     */
    name: GraphName,
    
    /**
     * @type list: hydra/graph.Element
     */
    elements: Seq[Element],
    
    /**
     * @type function:
     *         from:
     *         - hydra/core.Term
     *         to: boolean
     */
    dataTerms: Term => Boolean,
    
    /**
     * A reference to this graph's schema graph within the provided graph set
     * 
     * @type hydra/graph.GraphName
     */
    schemaGraph: GraphName
)

val _Graph: String = "hydra/graph.Graph"
val _Graph_dataTerms: String = "dataTerms"
val _Graph_elements: String = "elements"
val _Graph_name: String = "name"
val _Graph_schemaGraph: String = "schemaGraph"
