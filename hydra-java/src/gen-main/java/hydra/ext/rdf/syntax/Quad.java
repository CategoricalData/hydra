package hydra.ext.rdf.syntax;

/**
 * An RDF triple with an optional named graph component
 */
public class Quad {
  public final hydra.ext.rdf.syntax.Resource subject;
  
  public final hydra.ext.rdf.syntax.Iri predicate;
  
  public final hydra.ext.rdf.syntax.Node object;
  
  public final java.util.Optional<hydra.ext.rdf.syntax.Iri> graph;
  
  public Quad (hydra.ext.rdf.syntax.Resource subject, hydra.ext.rdf.syntax.Iri predicate, hydra.ext.rdf.syntax.Node object, java.util.Optional<hydra.ext.rdf.syntax.Iri> graph) {
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
  
  public Quad withSubject(hydra.ext.rdf.syntax.Resource subject) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withPredicate(hydra.ext.rdf.syntax.Iri predicate) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withObject(hydra.ext.rdf.syntax.Node object) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withGraph(java.util.Optional<hydra.ext.rdf.syntax.Iri> graph) {
    return new Quad(subject, predicate, object, graph);
  }
}