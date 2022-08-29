package hydra.graph;

/**
 * A collection of graphs with a distinguished root graph
 */
public class GraphSet<M> {
  public final java.util.Map<hydra.graph.GraphName, hydra.graph.Graph<M>> graphs;
  
  public final hydra.graph.GraphName root;
  
  public GraphSet (java.util.Map<hydra.graph.GraphName, hydra.graph.Graph<M>> graphs, hydra.graph.GraphName root) {
    this.graphs = graphs;
    this.root = root;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GraphSet)) {
      return false;
    }
    GraphSet o = (GraphSet) (other);
    return graphs.equals(o.graphs) && root.equals(o.root);
  }
  
  @Override
  public int hashCode() {
    return 2 * graphs.hashCode() + 3 * root.hashCode();
  }
  
  public GraphSet withGraphs(java.util.Map<hydra.graph.GraphName, hydra.graph.Graph<M>> graphs) {
    return new GraphSet(graphs, root);
  }
  
  public GraphSet withRoot(hydra.graph.GraphName root) {
    return new GraphSet(graphs, root);
  }
}