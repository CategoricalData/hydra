// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class SubObjectPropertyOf implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.SubObjectPropertyOf");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_SUB_PROPERTY = new hydra.core.Name("subProperty");
  
  public static final hydra.core.Name FIELD_NAME_SUPER_PROPERTY = new hydra.core.Name("superProperty");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty;
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression superProperty;
  
  public SubObjectPropertyOf (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty, hydra.langs.owl.syntax.ObjectPropertyExpression superProperty) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((subProperty));
    java.util.Objects.requireNonNull((superProperty));
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
    java.util.Objects.requireNonNull((annotations));
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSubProperty(java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> subProperty) {
    java.util.Objects.requireNonNull((subProperty));
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubObjectPropertyOf withSuperProperty(hydra.langs.owl.syntax.ObjectPropertyExpression superProperty) {
    java.util.Objects.requireNonNull((superProperty));
    return new SubObjectPropertyOf(annotations, subProperty, superProperty);
  }
}