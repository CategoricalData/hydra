// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * An RDF triple with an optional named graph component
 */
public class Quad implements Serializable, Comparable<Quad> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Quad");

  public static final hydra.core.Name SUBJECT = new hydra.core.Name("subject");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name GRAPH = new hydra.core.Name("graph");

  public final hydra.ext.org.w3.rdf.syntax.Resource subject;

  public final hydra.ext.org.w3.rdf.syntax.Iri predicate;

  public final hydra.ext.org.w3.rdf.syntax.Node object;

  public final hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Iri> graph;

  public Quad (hydra.ext.org.w3.rdf.syntax.Resource subject, hydra.ext.org.w3.rdf.syntax.Iri predicate, hydra.ext.org.w3.rdf.syntax.Node object, hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Iri> graph) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
    this.graph = graph;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Quad)) {
      return false;
    }
    Quad o = (Quad) other;
    return java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.object,
      o.object) && java.util.Objects.equals(
      this.graph,
      o.graph);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subject) + 3 * java.util.Objects.hashCode(predicate) + 5 * java.util.Objects.hashCode(object) + 7 * java.util.Objects.hashCode(graph);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Quad other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      subject,
      other.subject);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      predicate,
      other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      object,
      other.object);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      graph,
      other.graph);
  }

  public Quad withSubject(hydra.ext.org.w3.rdf.syntax.Resource subject) {
    return new Quad(subject, predicate, object, graph);
  }

  public Quad withPredicate(hydra.ext.org.w3.rdf.syntax.Iri predicate) {
    return new Quad(subject, predicate, object, graph);
  }

  public Quad withObject(hydra.ext.org.w3.rdf.syntax.Node object) {
    return new Quad(subject, predicate, object, graph);
  }

  public Quad withGraph(hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.Iri> graph) {
    return new Quad(subject, predicate, object, graph);
  }
}
