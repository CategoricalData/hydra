// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A term together with an annotation
 */
public class AnnotatedTerm implements Serializable, Comparable<AnnotatedTerm> {
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
    this.body = body;
    this.annotation = annotation;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedTerm)) {
      return false;
    }
    AnnotatedTerm o = (AnnotatedTerm) (other);
    return java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.annotation,
      o.annotation);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(body) + 3 * java.util.Objects.hashCode(annotation);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotatedTerm other) {
    int cmp = 0;
    cmp = ((Comparable) (body)).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      annotation.hashCode(),
      other.annotation.hashCode());
  }
  
  public AnnotatedTerm withBody(hydra.core.Term body) {
    return new AnnotatedTerm(body, annotation);
  }
  
  public AnnotatedTerm withAnnotation(java.util.Map<hydra.core.Name, hydra.core.Term> annotation) {
    return new AnnotatedTerm(body, annotation);
  }
}
