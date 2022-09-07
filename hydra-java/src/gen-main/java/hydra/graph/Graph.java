package hydra.graph;

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the schema graph
 */
public class Graph<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/graph.Graph");
  
  /**
   * The unique (within a given graph set) name of the graph
   */
  public final hydra.graph.GraphName name;
  
  /**
   * All of the elements in the graph
   */
  public final java.util.List<hydra.graph.Element<M>> elements;
  
  /**
   * A reference to this graph's schema graph within the provided graph set
   */
  public final hydra.graph.GraphName schemaGraph;
  
  public Graph (hydra.graph.GraphName name, java.util.List<hydra.graph.Element<M>> elements, hydra.graph.GraphName schemaGraph) {
    this.name = name;
    this.elements = elements;
    this.schemaGraph = schemaGraph;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return name.equals(o.name) && elements.equals(o.elements) && schemaGraph.equals(o.schemaGraph);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * elements.hashCode() + 5 * schemaGraph.hashCode();
  }
  
  public Graph withName(hydra.graph.GraphName name) {
    return new Graph(name, elements, schemaGraph);
  }
  
  public Graph withElements(java.util.List<hydra.graph.Element<M>> elements) {
    return new Graph(name, elements, schemaGraph);
  }
  
  public Graph withSchemaGraph(hydra.graph.GraphName schemaGraph) {
    return new Graph(name, elements, schemaGraph);
  }
}