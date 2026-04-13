// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class SubAnnotationPropertyOf implements Serializable, Comparable<SubAnnotationPropertyOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.SubAnnotationPropertyOf");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name SUB_PROPERTY = new hydra.core.Name("subProperty");

  public static final hydra.core.Name SUPER_PROPERTY = new hydra.core.Name("superProperty");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.AnnotationProperty subProperty;

  public final hydra.owl.syntax.AnnotationProperty superProperty;

  public SubAnnotationPropertyOf (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.AnnotationProperty subProperty, hydra.owl.syntax.AnnotationProperty superProperty) {
    this.annotations = annotations;
    this.subProperty = subProperty;
    this.superProperty = superProperty;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubAnnotationPropertyOf)) {
      return false;
    }
    SubAnnotationPropertyOf o = (SubAnnotationPropertyOf) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.subProperty,
      o.subProperty) && java.util.Objects.equals(
      this.superProperty,
      o.superProperty);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(subProperty) + 5 * java.util.Objects.hashCode(superProperty);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SubAnnotationPropertyOf other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      subProperty,
      other.subProperty);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      superProperty,
      other.superProperty);
  }

  public SubAnnotationPropertyOf withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }

  public SubAnnotationPropertyOf withSubProperty(hydra.owl.syntax.AnnotationProperty subProperty) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }

  public SubAnnotationPropertyOf withSuperProperty(hydra.owl.syntax.AnnotationProperty superProperty) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
}
