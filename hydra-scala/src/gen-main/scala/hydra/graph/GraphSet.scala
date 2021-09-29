/**
 * A collection of graphs with a distinguished root graph
 */
package hydra.graph

/**
 * A collection of graphs with a distinguished root graph
 */
case class GraphSet(
    /**
     * @type map:
     *         keys: hydra/graph.GraphName
     *         values: hydra/graph.Graph
     */
    graphs: Map[GraphName, Graph],
    
    /**
     * The focal graph of this set; 'the' graph. This root graph's schema graph, the second-degree schema graph, etc. are
     * also provided as non-root graphs.
     * 
     * @type hydra/graph.GraphName
     */
    root: GraphName
)

val _GraphSet: String = "hydra/graph.GraphSet"
val _GraphSet_graphs: String = "graphs"
val _GraphSet_root: String = "root"
