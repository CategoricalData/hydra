package hydra.ext.owl.syntax;

public class SymmetricObjectProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.SymmetricObjectProperty");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public SymmetricObjectProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SymmetricObjectProperty)) {
      return false;
    }
    SymmetricObjectProperty o = (SymmetricObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public SymmetricObjectProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new SymmetricObjectProperty(annotations, property);
  }
  
  public SymmetricObjectProperty withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    return new SymmetricObjectProperty(annotations, property);
  }
}