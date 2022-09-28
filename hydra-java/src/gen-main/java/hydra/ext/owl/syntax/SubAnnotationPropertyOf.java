package hydra.ext.owl.syntax;

public class SubAnnotationPropertyOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.SubAnnotationPropertyOf");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.AnnotationProperty subProperty;
  
  public final hydra.ext.owl.syntax.AnnotationProperty superProperty;
  
  public SubAnnotationPropertyOf (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.AnnotationProperty subProperty, hydra.ext.owl.syntax.AnnotationProperty superProperty) {
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
  
  public SubAnnotationPropertyOf withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubAnnotationPropertyOf withSubProperty(hydra.ext.owl.syntax.AnnotationProperty subProperty) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubAnnotationPropertyOf withSuperProperty(hydra.ext.owl.syntax.AnnotationProperty superProperty) {
    return new SubAnnotationPropertyOf(annotations, subProperty, superProperty);
  }
}