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
     * @type hydra/graph.Graph
     */
    schemaGraph: Graph
)
