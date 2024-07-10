// Note: this is an automatically generated file. Do not edit.

package hydra.langs.rdf.syntax;

import java.io.Serializable;

/**
 * A graph of RDF statements together with a distinguished subject and/or object node
 */
public class Description implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/rdf/syntax.Description");
  
  public final hydra.langs.rdf.syntax.Node subject;
  
  public final hydra.langs.rdf.syntax.Graph graph;
  
  public Description (hydra.langs.rdf.syntax.Node subject, hydra.langs.rdf.syntax.Graph graph) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
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
  
  public Description withSubject(hydra.langs.rdf.syntax.Node subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new Description(subject, graph);
  }
  
  public Description withGraph(hydra.langs.rdf.syntax.Graph graph) {
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
    return new Description(subject, graph);
  }
}