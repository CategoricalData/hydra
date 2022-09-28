package hydra.ext.owl.syntax;

public class FunctionalDataProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.FunctionalDataProperty");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression property;
  
  public FunctionalDataProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.DataPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionalDataProperty)) {
      return false;
    }
    FunctionalDataProperty o = (FunctionalDataProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public FunctionalDataProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new FunctionalDataProperty(annotations, property);
  }
  
  public FunctionalDataProperty withProperty(hydra.ext.owl.syntax.DataPropertyExpression property) {
    return new FunctionalDataProperty(annotations, property);
  }
}