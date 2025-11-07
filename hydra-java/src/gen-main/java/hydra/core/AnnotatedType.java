// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type together with an annotation
 */
public class AnnotatedType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.AnnotatedType");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATION = new hydra.core.Name("annotation");
  
  /**
   * The type being annotated
   */
  public final hydra.core.Type body;
  
  /**
   * The annotation as a map from keys to values
   */
  public final java.util.Map<hydra.core.Name, hydra.core.Term> annotation;
  
  public AnnotatedType (hydra.core.Type body, java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((body));
    java.util.Objects.requireNonNull((annotation));
    this.body = body;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedType)) {
      return false;
    }
    AnnotatedType o = (AnnotatedType) (other);
    return body.equals(o.body) && annotation.equals(o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode() + 3 * annotation.hashCode();
  }
  
  public AnnotatedType withBody(hydra.core.Type body) {
    java.util.Objects.requireNonNull((body));
    return new AnnotatedType(body, annotation);
  }
  
  public AnnotatedType withAnnotation(java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    java.util.Objects.requireNonNull((annotation));
    return new AnnotatedType(body, annotation);
  }
}
