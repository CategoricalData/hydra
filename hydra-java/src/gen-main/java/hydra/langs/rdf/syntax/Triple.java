// Note: this is an automatically generated file. Do not edit.

package hydra.langs.rdf.syntax;

import java.io.Serializable;

/**
 * An RDF triple defined by a subject, predicate, and object
 */
public class Triple implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/rdf/syntax.Triple");
  
  public final hydra.langs.rdf.syntax.Resource subject;
  
  public final hydra.langs.rdf.syntax.Iri predicate;
  
  public final hydra.langs.rdf.syntax.Node object;
  
  public Triple (hydra.langs.rdf.syntax.Resource subject, hydra.langs.rdf.syntax.Iri predicate, hydra.langs.rdf.syntax.Node object) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
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
  
  public Triple withSubject(hydra.langs.rdf.syntax.Resource subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new Triple(subject, predicate, object);
  }
  
  public Triple withPredicate(hydra.langs.rdf.syntax.Iri predicate) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    return new Triple(subject, predicate, object);
  }
  
  public Triple withObject(hydra.langs.rdf.syntax.Node object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new Triple(subject, predicate, object);
  }
}