package hydra.ext.owl.syntax;

public class SubDataPropertyOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.SubDataPropertyOf");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression subProperty;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression superProperty;
  
  public SubDataPropertyOf (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.DataPropertyExpression subProperty, hydra.ext.owl.syntax.DataPropertyExpression superProperty) {
    this.annotations = annotations;
    this.subProperty = subProperty;
    this.superProperty = superProperty;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubDataPropertyOf)) {
      return false;
    }
    SubDataPropertyOf o = (SubDataPropertyOf) (other);
    return annotations.equals(o.annotations) && subProperty.equals(o.subProperty) && superProperty.equals(o.superProperty);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * subProperty.hashCode() + 5 * superProperty.hashCode();
  }
  
  public SubDataPropertyOf withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubDataPropertyOf withSubProperty(hydra.ext.owl.syntax.DataPropertyExpression subProperty) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }
  
  public SubDataPropertyOf withSuperProperty(hydra.ext.owl.syntax.DataPropertyExpression superProperty) {
    return new SubDataPropertyOf(annotations, subProperty, superProperty);
  }
}