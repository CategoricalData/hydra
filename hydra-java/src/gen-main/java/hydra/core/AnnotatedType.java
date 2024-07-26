// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type together with an annotation
 */
public class AnnotatedType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.AnnotatedType");
  
  public final hydra.core.Type subject;
  
  public final hydra.core.Kv annotation;
  
  public AnnotatedType (hydra.core.Type subject, hydra.core.Kv annotation) {
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((annotation));
    this.subject = subject;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedType)) {
      return false;
    }
    AnnotatedType o = (AnnotatedType) (other);
    return subject.equals(o.subject) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * annotation.hashCode();
  }
  
  public AnnotatedType withSubject(hydra.core.Type subject) {
    java.util.Objects.requireNonNull((subject));
    return new AnnotatedType(subject, annotation);
  }
  
  public AnnotatedType withAnnotation(hydra.core.Kv annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new AnnotatedType(subject, annotation);
  }
}