// Note: this is an automatically generated file. Do not edit.

package hydra.langs.rdf.syntax;

import java.io.Serializable;

/**
 * An RDF triple with an optional named graph component
 */
public class Quad implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/rdf/syntax.Quad");
  
  public final hydra.langs.rdf.syntax.Resource subject;
  
  public final hydra.langs.rdf.syntax.Iri predicate;
  
  public final hydra.langs.rdf.syntax.Node object;
  
  public final hydra.util.Opt<hydra.langs.rdf.syntax.Iri> graph;
  
  public Quad (hydra.langs.rdf.syntax.Resource subject, hydra.langs.rdf.syntax.Iri predicate, hydra.langs.rdf.syntax.Node object, hydra.util.Opt<hydra.langs.rdf.syntax.Iri> graph) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
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
    Quad o = (Quad) (other);
    return subject.equals(o.subject) && predicate.equals(o.predicate) && object.equals(o.object) && graph.equals(o.graph);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * predicate.hashCode() + 5 * object.hashCode() + 7 * graph.hashCode();
  }
  
  public Quad withSubject(hydra.langs.rdf.syntax.Resource subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withPredicate(hydra.langs.rdf.syntax.Iri predicate) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withObject(hydra.langs.rdf.syntax.Node object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withGraph(hydra.util.Opt<hydra.langs.rdf.syntax.Iri> graph) {
    if (graph == null) {
      throw new IllegalArgumentException("null value for 'graph' argument");
    }
    return new Quad(subject, predicate, object, graph);
  }
}