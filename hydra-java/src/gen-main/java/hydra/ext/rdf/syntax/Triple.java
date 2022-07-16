package hydra.ext.rdf.syntax;

/**
 * An RDF triple defined by a subject, predicate, and object
 */
public class Triple {
  public final Resource subject;
  
  public final Iri predicate;
  
  public final Node object;
  
  public Triple (Resource subject, Iri predicate, Node object) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Triple)) {
      return false;
    }
    Triple o = (Triple) (other);
    return subject.equals(o.subject) && predicate.equals(o.predicate) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * predicate.hashCode() + 5 * object.hashCode();
  }
  
  public Triple withSubject(Resource subject) {
    return new Triple(subject, predicate, object);
  }
  
  public Triple withPredicate(Iri predicate) {
    return new Triple(subject, predicate, object);
  }
  
  public Triple withObject(Node object) {
    return new Triple(subject, predicate, object);
  }
}