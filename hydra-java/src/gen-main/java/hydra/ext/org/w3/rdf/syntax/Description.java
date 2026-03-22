// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A graph of RDF statements together with a distinguished subject and/or object node
 */
public class Description implements Serializable, Comparable<Description> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Description");

  public static final hydra.core.Name SUBJECT = new hydra.core.Name("subject");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public final hydra.ext.org.w3.rdf.syntax.Node subject;

  public final hydra.ext.org.w3.rdf.syntax.Graph graph;

  public Description (hydra.ext.org.w3.rdf.syntax.Node subject, hydra.ext.org.w3.rdf.syntax.Graph graph) {
    this.subject = subject;
    this.graph = graph;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Description)) {
      return false;
    }
    Description o = (Description) other;
    return java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.graph,
      o.graph);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subject) + 3 * java.util.Objects.hashCode(graph);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Description other) {
    int cmp = 0;
    cmp = ((Comparable) subject).compareTo(other.subject);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) graph).compareTo(other.graph);
  }

  public Description withSubject(hydra.ext.org.w3.rdf.syntax.Node subject) {
    return new Description(subject, graph);
  }

  public Description withGraph(hydra.ext.org.w3.rdf.syntax.Graph graph) {
    return new Description(subject, graph);
  }
}
