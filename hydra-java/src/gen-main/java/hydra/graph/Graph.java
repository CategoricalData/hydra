package hydra.graph;

import hydra.core.Term;

/**
 * A graph, or set of legal terms combined with a set of elements over those terms, as well as another graph, called the
 * schema graph
 */
public class Graph {
  public final GraphName name;
  
  public final java.util.List<Element> elements;
  
  public final java.util.function.Function<Term, Boolean> dataTerms;
  
  /**
   * A reference to this graph's schema graph within the provided graph set
   */
  public final GraphName schemaGraph;
  
  /**
   * Constructs an immutable Graph object
   */
  public Graph(GraphName name, java.util.List<Element> elements, java.util.function.Function<Term, Boolean> dataTerms, GraphName schemaGraph) {
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
  public Graph withName(GraphName name) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which elements is overridden
   */
  public Graph withElements(java.util.List<Element> elements) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which dataTerms is overridden
   */
  public Graph withDataTerms(java.util.function.Function<Term, Boolean> dataTerms) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
  
  /**
   * Construct a new immutable Graph object in which schemaGraph is overridden
   */
  public Graph withSchemaGraph(GraphName schemaGraph) {
    return new Graph(name, elements, dataTerms, schemaGraph);
  }
}
