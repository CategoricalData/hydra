// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type together with an annotation
 */
public class AnnotatedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/core.AnnotatedType");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  public final hydra.core.Type subject;
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term> annotation;
  
  public AnnotatedType (hydra.core.Type subject, java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
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
  
  public AnnotatedType withAnnotation(java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new AnnotatedType(subject, annotation);
  }
}