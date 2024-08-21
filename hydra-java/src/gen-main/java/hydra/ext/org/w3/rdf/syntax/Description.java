// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A graph of RDF statements together with a distinguished subject and/or object node
 */
public class Description implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/rdf/syntax.Description");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_GRAPH = new hydra.core.Name("graph");
  
  public final hydra.ext.org.w3.rdf.syntax.Node subject;
  
  public final hydra.ext.org.w3.rdf.syntax.Graph graph;
  
  public Description (hydra.ext.org.w3.rdf.syntax.Node subject, hydra.ext.org.w3.rdf.syntax.Graph graph) {
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((graph));
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
  
  public Description withSubject(hydra.ext.org.w3.rdf.syntax.Node subject) {
    java.util.Objects.requireNonNull((subject));
    return new Description(subject, graph);
  }
  
  public Description withGraph(hydra.ext.org.w3.rdf.syntax.Graph graph) {
    java.util.Objects.requireNonNull((graph));
    return new Description(subject, graph);
  }
}