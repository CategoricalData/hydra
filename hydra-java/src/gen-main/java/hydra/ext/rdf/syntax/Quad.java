package hydra.ext.rdf.syntax;

/**
 * An RDF triple with an optional named graph component
 */
public class Quad {
  public final Resource subject;
  
  public final Iri predicate;
  
  public final Node object;
  
  public final java.util.Optional<Iri> graph;
  
  public Quad (Resource subject, Iri predicate, Node object, java.util.Optional<Iri> graph) {
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
  
  public Quad withSubject(Resource subject) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withPredicate(Iri predicate) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withObject(Node object) {
    return new Quad(subject, predicate, object, graph);
  }
  
  public Quad withGraph(java.util.Optional<Iri> graph) {
    return new Quad(subject, predicate, object, graph);
  }
}