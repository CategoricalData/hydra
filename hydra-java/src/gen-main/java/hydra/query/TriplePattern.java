// Note: this is an automatically generated file. Do not edit.

package hydra.query;

import java.io.Serializable;

/**
 * A subject/predicate/object pattern
 */
public class TriplePattern implements Serializable, Comparable<TriplePattern> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.query.TriplePattern");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_PREDICATE = new hydra.core.Name("predicate");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT = new hydra.core.Name("object");
  
  /**
   * The subject of the pattern
   */
  public final hydra.query.Node subject;
  
  /**
   * The predicate (property) of the pattern
   */
  public final hydra.query.Path predicate;
  
  /**
   * The object of the pattern
   */
  public final hydra.query.Node object;
  
  public TriplePattern (hydra.query.Node subject, hydra.query.Path predicate, hydra.query.Node object) {
    this.subject = subject;
    this.predicate = predicate;
    this.object = object;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TriplePattern)) {
      return false;
    }
    TriplePattern o = (TriplePattern) other;
    return java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.predicate,
      o.predicate) && java.util.Objects.equals(
      this.object,
      o.object);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(subject) + 3 * java.util.Objects.hashCode(predicate) + 5 * java.util.Objects.hashCode(object);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(TriplePattern other) {
    int cmp = 0;
    cmp = ((Comparable) subject).compareTo(other.subject);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) predicate).compareTo(other.predicate);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) object).compareTo(other.object);
  }
  
  public TriplePattern withSubject(hydra.query.Node subject) {
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withPredicate(hydra.query.Path predicate) {
    return new TriplePattern(subject, predicate, object);
  }
  
  public TriplePattern withObject(hydra.query.Node object) {
    return new TriplePattern(subject, predicate, object);
  }
}
