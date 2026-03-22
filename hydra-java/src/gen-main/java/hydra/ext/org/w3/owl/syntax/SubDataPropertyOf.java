// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class SubDataPropertyOf implements Serializable, Comparable<SubDataPropertyOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubDataPropertyOf");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name SUB_PROPERTY = new hydra.core.Name("subProperty");

  public static final hydra.core.Name SUPER_PROPERTY = new hydra.core.Name("superProperty");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression subProperty;

  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression superProperty;

  public SubDataPropertyOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression subProperty, hydra.ext.org.w3.owl.syntax.DataPropertyExpression superProperty) {
    this.annotations = annotations;
    this.subProperty = subProperty;
    this.superProperty = superProperty;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubDataPropertyOf)) {
      return false;
    }
    SubDataPropertyOf o = (SubDataPropertyOf) other;
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
  public int compareTo(SubDataPropertyOf other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) subProperty).compareTo(other.subProperty);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) superProperty).compareTo(other.superProperty);
  }

  public SubDataPropertyOf withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }

  public SubDataPropertyOf withSubProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression subProperty) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }

  public SubDataPropertyOf withSuperProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression superProperty) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }
}
