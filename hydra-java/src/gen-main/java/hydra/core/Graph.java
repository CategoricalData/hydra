package hydra.core;

/**
 * A graph, or set of named terms, together with its schema graph
 */
public class Graph<M> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Graph");
  
  /**
   * All of the elements in the graph
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Element<M>> elements;
  
  /**
   * The schema graph to this graph. If omitted, the graph is its own schema graph.
   */
  public final java.util.Optional<hydra.core.Graph<M>> schema;
  
  public Graph (java.util.Map<hydra.core.Name, hydra.core.Element<M>> elements, java.util.Optional<hydra.core.Graph<M>> schema) {
    this.elements = elements;
    this.schema = schema;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
      return false;
    }
    Graph o = (Graph) (other);
    return elements.equals(o.elements) && schema.equals(o.schema);
  }
  
  @Override
  public int hashCode() {
    return 2 * elements.hashCode() + 3 * schema.hashCode();
  }
  
  public Graph withElements(java.util.Map<hydra.core.Name, hydra.core.Element<M>> elements) {
    return new Graph(elements, schema);
  }
  
  public Graph withSchema(java.util.Optional<hydra.core.Graph<M>> schema) {
    return new Graph(elements, schema);
  }
}