// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class SubObjectPropertyOf implements Serializable, Comparable<SubObjectPropertyOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name SUB_PROPERTY = new hydra.core.Name("subProperty");
  
  public static final hydra.core.Name SUPER_PROPERTY = new hydra.core.Name("superProperty");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> subProperty;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression superProperty;
  
  public SubObjectPropertyOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> subProperty, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression superProperty) {
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
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      subProperty.hashCode(),
      other.subProperty.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) superProperty).compareTo(other.superProperty);
  }
  
  public SubObjectPropertyOf withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSubProperty(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> subProperty) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSuperProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression superProperty) {
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
}
