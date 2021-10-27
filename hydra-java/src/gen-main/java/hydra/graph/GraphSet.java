package hydra.graph;

/**
 * A collection of graphs with a distinguished root graph
 */
public class GraphSet {
  /**
   * @type map:
   *         keys: hydra/graph.GraphName
   *         values: hydra/graph.Graph
   */
  public final java.util.Map<GraphName, Graph> graphs;
  
  /**
   * The focal graph of this set; 'the' graph. This root graph's schema graph, the second-degree schema graph, etc. are
   * also provided as non-root graphs.
   * 
   * @type hydra/graph.GraphName
   */
  public final GraphName root;
  
  /**
   * Constructs an immutable GraphSet object
   */
  public GraphSet(java.util.Map<GraphName, Graph> graphs, GraphName root) {
    this.graphs = graphs;
    this.root = root;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphSet)) {
        return false;
    }
    GraphSet o = (GraphSet) other;
    return graphs.equals(o.graphs)
        && root.equals(o.root);
  }
  
  @Override
  public int hashCode() {
    return 2 * graphs.hashCode()
        + 3 * root.hashCode();
  }
  
  /**
   * Construct a new immutable GraphSet object in which graphs is overridden
   */
  public GraphSet withGraphs(java.util.Map<GraphName, Graph> graphs) {
    return new GraphSet(graphs, root);
  }
  
  /**
   * Construct a new immutable GraphSet object in which root is overridden
   */
  public GraphSet withRoot(GraphName root) {
    return new GraphSet(graphs, root);
  }
}
