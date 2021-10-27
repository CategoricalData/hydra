package hydra.graph;

import hydra.core.Term;

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
public class Graph {
  public final hydra.graph.GraphName name;
  
  public final java.util.List<hydra.graph.Element> elements;
  
  public final java.util.function.Function<hydra.core.Term, Boolean> dataTerms;
  
  /**
   * A reference to this graph's schema graph within the provided graph set
   */
  public final hydra.graph.GraphName schemaGraph;
  
  /**
   * Constructs an immutable Graph object
   */
  public Graph(hydra.graph.GraphName name, java.util.List<hydra.graph.Element> elements, java.util.function.Function<hydra.core.Term, Boolean> dataTerms, hydra.graph.GraphName schemaGraph) {
    this.name = name;
    this.elements = elements;
    this.dataTerms = dataTerms;
    this.schemaGraph = schemaGraph;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Graph)) {
        return false;
    }
    Graph o = (Graph) other;
    return name.equals(o.name)
        && elements.equals(o.elements)
        && dataTerms.equals(o.dataTerms)
        && schemaGraph.equals(o.schemaGraph);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode()
        + 3 * elements.hashCode()
        + 5 * dataTerms.hashCode()
        + 7 * schemaGraph.hashCode();
  }
  
  /**
   * Construct a new immutable Graph object in which name is overridden
   */
  public Graph withName(hydra.graph.GraphName name) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which elements is overridden
   */
  public Graph withElements(java.util.List<hydra.graph.Element> elements) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which dataTerms is overridden
   */
  public Graph withDataTerms(java.util.function.Function<hydra.core.Term, Boolean> dataTerms) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which schemaGraph is overridden
   */
  public Graph withSchemaGraph(hydra.graph.GraphName schemaGraph) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
}
