// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class SubObjectPropertyOf implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.SubObjectPropertyOf");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty;
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression superProperty;
  
  public SubObjectPropertyOf (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty, hydra.langs.owl.syntax.ObjectPropertyExpression superProperty) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (subProperty == null) {
      throw new IllegalArgumentException("null value for 'subProperty' argument");
    }
    if (superProperty == null) {
      throw new IllegalArgumentException("null value for 'superProperty' argument");
    }
    this.annotations = annotations;
    this.subProperty = subProperty;
    this.superProperty = superProperty;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubObjectPropertyOf)) {
      return false;
    }
    SubObjectPropertyOf o = (SubObjectPropertyOf) (other);
    return annotations.equals(o.annotations) && subProperty.equals(o.subProperty) && superProperty.equals(o.superProperty);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * subProperty.hashCode() + 5 * superProperty.hashCode();
  }
  
  public SubObjectPropertyOf withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSubProperty(java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty) {
    if (subProperty == null) {
      throw new IllegalArgumentException("null value for 'subProperty' argument");
    }
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSuperProperty(hydra.langs.owl.syntax.ObjectPropertyExpression superProperty) {
    if (superProperty == null) {
      throw new IllegalArgumentException("null value for 'superProperty' argument");
    }
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
}