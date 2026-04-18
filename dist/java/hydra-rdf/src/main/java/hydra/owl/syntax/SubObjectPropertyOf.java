// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public class SubObjectPropertyOf implements Serializable, Comparable<SubObjectPropertyOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.SubObjectPropertyOf");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name SUB_PROPERTY = new hydra.core.Name("subProperty");

  public static final hydra.core.Name SUPER_PROPERTY = new hydra.core.Name("superProperty");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final java.util.List<hydra.owl.syntax.ObjectPropertyExpression> subProperty;

  public final hydra.owl.syntax.ObjectPropertyExpression superProperty;

  public SubObjectPropertyOf (java.util.List<hydra.owl.syntax.Annotation> annotations, java.util.List<hydra.owl.syntax.ObjectPropertyExpression> subProperty, hydra.owl.syntax.ObjectPropertyExpression superProperty) {
    this.annotations = annotations;
    this.subProperty = subProperty;
    this.superProperty = superProperty;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubObjectPropertyOf)) {
      return false;
    }
    SubObjectPropertyOf o = (SubObjectPropertyOf) other;
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
  public int compareTo(SubObjectPropertyOf other) {
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

  public SubObjectPropertyOf withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }

  public SubObjectPropertyOf withSubProperty(java.util.List<hydra.owl.syntax.ObjectPropertyExpression> subProperty) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }

  public SubObjectPropertyOf withSuperProperty(hydra.owl.syntax.ObjectPropertyExpression superProperty) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
}
