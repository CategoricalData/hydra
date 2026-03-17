// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A type together with an annotation
 */
public class AnnotatedType implements Serializable, Comparable<AnnotatedType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.core.AnnotatedType");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name ANNOTATION = new hydra.core.Name("annotation");

  /**
   * The type being annotated
   */
  public final hydra.core.Type body;

  /**
   * The annotation as a map from keys to values
   */
  public final hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> annotation;

  public AnnotatedType (hydra.core.Type body, hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> annotation) {
    this.body = body;
    this.annotation = annotation;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotatedType)) {
      return false;
    }
    AnnotatedType o = (AnnotatedType) other;
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
  public int compareTo(AnnotatedType other) {
    int cmp = 0;
    cmp = ((Comparable) body).compareTo(other.body);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) annotation).compareTo(other.annotation);
  }

  public AnnotatedType withBody(hydra.core.Type body) {
    return new AnnotatedType(body, annotation);
  }

  public AnnotatedType withAnnotation(hydra.util.PersistentMap<hydra.core.Name, hydra.core.Term> annotation) {
    return new AnnotatedType(body, annotation);
  }
}
