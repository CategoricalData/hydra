// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term together with an annotation
 */
public class AnnotatedTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.AnnotatedTerm");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  public final hydra.core.Term subject;
  
  public final java.util.Map<hydra.core.Name, hydra.core.Term> annotation;
  
  public AnnotatedTerm (hydra.core.Term subject, java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((annotation));
    this.subject = subject;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedTerm)) {
      return false;
    }
    AnnotatedTerm o = (AnnotatedTerm) (other);
    return subject.equals(o.subject) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * annotation.hashCode();
  }
  
  public AnnotatedTerm withSubject(hydra.core.Term subject) {
    java.util.Objects.requireNonNull((subject));
    return new AnnotatedTerm(subject, annotation);
  }
  
  public AnnotatedTerm withAnnotation(java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new AnnotatedTerm(subject, annotation);
  }
}