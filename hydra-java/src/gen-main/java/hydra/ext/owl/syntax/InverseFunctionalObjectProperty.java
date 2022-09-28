package hydra.ext.owl.syntax;

public class InverseFunctionalObjectProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.InverseFunctionalObjectProperty");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public InverseFunctionalObjectProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseFunctionalObjectProperty)) {
      return false;
    }
    InverseFunctionalObjectProperty o = (InverseFunctionalObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public InverseFunctionalObjectProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new InverseFunctionalObjectProperty(annotations, property);
  }
  
  public InverseFunctionalObjectProperty withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    return new InverseFunctionalObjectProperty(annotations, property);
  }
}