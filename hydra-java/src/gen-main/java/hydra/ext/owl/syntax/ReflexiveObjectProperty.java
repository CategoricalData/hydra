package hydra.ext.owl.syntax;

public class ReflexiveObjectProperty {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.ReflexiveObjectProperty");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public ReflexiveObjectProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ReflexiveObjectProperty)) {
      return false;
    }
    ReflexiveObjectProperty o = (ReflexiveObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public ReflexiveObjectProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new ReflexiveObjectProperty(annotations, property);
  }
  
  public ReflexiveObjectProperty withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    return new ReflexiveObjectProperty(annotations, property);
  }
}