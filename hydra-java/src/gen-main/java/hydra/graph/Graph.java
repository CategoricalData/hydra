package hydra.graph;

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the schema graph
 */
public class Graph<M> {
  public final GraphName name;
  
  public final java.util.List<Element<M>> elements;
  
  public final java.util.function.Function<hydra.core.Term<M>, Boolean> termExprs;
  
  /**
   * A reference to this graph's schema graph within the provided graph set
   */
  public final GraphName schemaGraph;
  
  public Graph (GraphName name, java.util.List<Element<M>> elements, java.util.function.Function<hydra.core.Term<M>, Boolean> termExprs, GraphName schemaGraph) {
    this.name = name;
    this.elements = elements;
    this.termExprs = termExprs;
    this.schemaGraph = schemaGraph;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return name.equals(o.name) && elements.equals(o.elements) && termExprs.equals(o.termExprs) && schemaGraph.equals(o.schemaGraph);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * elements.hashCode() + 5 * termExprs.hashCode() + 7 * schemaGraph.hashCode();
  }
  
  public Graph withName(GraphName name) {
    return new Graph(name, elements, termExprs, schemaGraph);
  }
  
  public Graph withElements(java.util.List<Element<M>> elements) {
    return new Graph(name, elements, termExprs, schemaGraph);
  }
  
  public Graph withTermExprs(java.util.function.Function<hydra.core.Term<M>, Boolean> termExprs) {
    return new Graph(name, elements, termExprs, schemaGraph);
  }
  
  public Graph withSchemaGraph(GraphName schemaGraph) {
    return new Graph(name, elements, termExprs, schemaGraph);
  }
}