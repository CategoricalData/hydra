// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term together with an annotation
 */
public class AnnotatedTerm implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.AnnotatedTerm");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  /**
   * The term being annotated
   */
  public final hydra.core.Term body;
  
  /**
   * The annotation as a map from keys to values
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> annotation;
  
  public AnnotatedTerm (hydra.core.Term body, java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((annotation));
    this.body = body;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedTerm)) {
      return false;
    }
    AnnotatedTerm o = (AnnotatedTerm) (other);
    return body.equals(o.body) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * annotation.hashCode();
  }
  
  public AnnotatedTerm withBody(hydra.core.Term body) {
    java.util.Objects.requireNonNull((body));
    return new AnnotatedTerm(body, annotation);
  }
  
  public AnnotatedTerm withAnnotation(java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new AnnotatedTerm(body, annotation);
  }
}
