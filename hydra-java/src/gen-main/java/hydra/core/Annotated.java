// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * An object, such as a type or term, together with an annotation
 */
public class Annotated<X, A> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Annotated");
  
  public final X subject;
  
  public final A annotation;
  
  public Annotated (X subject, A annotation) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    if (annotation == null) {
      throw new IllegalArgumentException("null value for 'annotation' argument");
    }
    this.subject = subject;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Annotated)) {
      return false;
    }
    Annotated o = (Annotated) (other);
    return subject.equals(o.subject) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * subject.hashCode() + 3 * annotation.hashCode();
  }
  
  public Annotated withSubject(X subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new Annotated(subject, annotation);
  }
  
  public Annotated withAnnotation(A annotation) {
    if (annotation == null) {
      throw new IllegalArgumentException("null value for 'annotation' argument");
    }
    return new Annotated(subject, annotation);
  }
}