// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * An RDF triple defined by a subject, predicate, and object
 */
public class Triple implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/rdf/syntax.Triple");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public final hydra.ext.org.w3.rdf.syntax.Resource subject;
  
  public final hydra.ext.org.w3.rdf.syntax.Iri predicate;
  
  public final hydra.ext.org.w3.rdf.syntax.Node object;
  
  public Triple (hydra.ext.org.w3.rdf.syntax.Resource subject, hydra.ext.org.w3.rdf.syntax.Iri predicate, hydra.ext.org.w3.rdf.syntax.Node object) {
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((object));
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
  
  public Triple withSubject(hydra.ext.org.w3.rdf.syntax.Resource subject) {
    java.util.Objects.requireNonNull((subject));
    return new Triple(subject, predicate, object);
  }
  
  public Triple withPredicate(hydra.ext.org.w3.rdf.syntax.Iri predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new Triple(subject, predicate, object);
  }
  
  public Triple withObject(hydra.ext.org.w3.rdf.syntax.Node object) {
    java.util.Objects.requireNonNull((object));
    return new Triple(subject, predicate, object);
  }
}