package hydra.graph;

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the schema graph
 */
public class Graph<M> {
  /**
   * The unique (within a given graph set) name of the graph
   */
  public final GraphName name;
  
  /**
   * All of the elements in the graph
   */
  public final java.util.List<Element<M>> elements;
  
  /**
   * A reference to this graph's schema graph within the provided graph set
   */
  public final GraphName schemaGraph;
  
  public Graph (GraphName name, java.util.List<Element<M>> elements, GraphName schemaGraph) {
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
  
  public Graph withName(GraphName name) {
    return new Graph(name, elements, schemaGraph);
  }
  
  public Graph withElements(java.util.List<Element<M>> elements) {
    return new Graph(name, elements, schemaGraph);
  }
  
  public Graph withSchemaGraph(GraphName schemaGraph) {
    return new Graph(name, elements, schemaGraph);
  }
}