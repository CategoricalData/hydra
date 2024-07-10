// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A subject/predicate/object pattern
 */
public class TriplePattern<A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/query.TriplePattern");
  
  public final hydra.query.Node<A> subject;
  
  public final hydra.query.Path predicate;
  
  public final hydra.query.Node<A> object;
  
  public TriplePattern (hydra.query.Node<A> subject, hydra.query.Path predicate, hydra.query.Node<A> object) {
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
  
  public TriplePattern withSubject(hydra.query.Node<A> subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withPredicate(hydra.query.Path predicate) {
    if (predicate == null) {
      throw new IllegalArgumentException("null value for 'predicate' argument");
    }
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withObject(hydra.query.Node<A> object) {
    if (object == null) {
      throw new IllegalArgumentException("null value for 'object' argument");
    }
    return new TriplePattern(subject, predicate, object);
  }
}