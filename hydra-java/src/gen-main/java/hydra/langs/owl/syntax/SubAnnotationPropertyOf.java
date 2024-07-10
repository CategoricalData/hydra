// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class SubAnnotationPropertyOf implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.SubAnnotationPropertyOf");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.AnnotationProperty subProperty;
  
  public final hydra.langs.owl.syntax.AnnotationProperty superProperty;
  
  public SubAnnotationPropertyOf (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.AnnotationProperty subProperty, hydra.langs.owl.syntax.AnnotationProperty superProperty) {
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
    if (!(other instanceof SubAnnotationPropertyOf)) {
      return false;
    }
    SubAnnotationPropertyOf o = (SubAnnotationPropertyOf) (other);
    return annotations.equals(o.annotations) && subProperty.equals(o.subProperty) && superProperty.equals(o.superProperty);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * subProperty.hashCode() + 5 * superProperty.hashCode();
  }
  
  public SubAnnotationPropertyOf withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubAnnotationPropertyOf withSubProperty(hydra.langs.owl.syntax.AnnotationProperty subProperty) {
    if (subProperty == null) {
      throw new IllegalArgumentException("null value for 'subProperty' argument");
    }
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubAnnotationPropertyOf withSuperProperty(hydra.langs.owl.syntax.AnnotationProperty superProperty) {
    if (superProperty == null) {
      throw new IllegalArgumentException("null value for 'superProperty' argument");
    }
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
}