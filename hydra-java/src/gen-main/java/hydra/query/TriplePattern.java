// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A subject/predicate/object pattern
 */
public class TriplePattern implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.TriplePattern");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  public final hydra.query.Node subject;
  
  public final hydra.query.Path predicate;
  
  public final hydra.query.Node object;
  
  public TriplePattern (hydra.query.Node subject, hydra.query.Path predicate, hydra.query.Node object) {
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((predicate));
    java.util.Objects.requireNonNull((object));
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TriplePattern)) {
      return false;
    }
    TriplePattern o = (TriplePattern) (other);
    return subject.equals(o.subject) && predicate.equals(o.predicate) && object.equals(o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * predicate.hashCode() + 5 * object.hashCode();
  }
  
  public TriplePattern withSubject(hydra.query.Node subject) {
    java.util.Objects.requireNonNull((subject));
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withPredicate(hydra.query.Path predicate) {
    java.util.Objects.requireNonNull((predicate));
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withObject(hydra.query.Node object) {
    java.util.Objects.requireNonNull((object));
    return new TriplePattern(subject, predicate, object);
  }
}