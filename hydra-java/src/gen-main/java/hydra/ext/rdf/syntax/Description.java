package hydra.ext.rdf.syntax;

/**
 * A graph of RDF statements together with a distinguished subject and/or object node
 */
public class Description {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/rdf/syntax.Description");
  
  public final hydra.ext.rdf.syntax.Node subject;
  
  public final hydra.ext.rdf.syntax.Graph graph;
  
  public Description (hydra.ext.rdf.syntax.Node subject, hydra.ext.rdf.syntax.Graph graph) {
    this.subject = subject;
    this.graph = graph;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Description)) {
      return false;
    }
    Description o = (Description) (other);
    return subject.equals(o.subject) && graph.equals(o.graph);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * graph.hashCode();
  }
  
  public Description withSubject(hydra.ext.rdf.syntax.Node subject) {
    return new Description(subject, graph);
  }
  
  public Description withGraph(hydra.ext.rdf.syntax.Graph graph) {
    return new Description(subject, graph);
  }
}