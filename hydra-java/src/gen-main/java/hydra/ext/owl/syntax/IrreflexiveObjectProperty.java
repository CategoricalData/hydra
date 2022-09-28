package hydra.ext.owl.syntax;

public class IrreflexiveObjectProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.IrreflexiveObjectProperty");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public IrreflexiveObjectProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IrreflexiveObjectProperty)) {
      return false;
    }
    IrreflexiveObjectProperty o = (IrreflexiveObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public IrreflexiveObjectProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new IrreflexiveObjectProperty(annotations, property);
  }
  
  public IrreflexiveObjectProperty withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    return new IrreflexiveObjectProperty(annotations, property);
  }
}