package hydra.ext.owl.syntax;

public class InverseObjectProperties {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.InverseObjectProperties");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property1;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property2;
  
  public InverseObjectProperties (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property1, hydra.ext.owl.syntax.ObjectPropertyExpression property2) {
    this.annotations = annotations;
    this.property1 = property1;
    this.property2 = property2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseObjectProperties)) {
      return false;
    }
    InverseObjectProperties o = (InverseObjectProperties) (other);
    return annotations.equals(o.annotations) && property1.equals(o.property1) && property2.equals(o.property2);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property1.hashCode() + 5 * property2.hashCode();
  }
  
  public InverseObjectProperties withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty1(hydra.ext.owl.syntax.ObjectPropertyExpression property1) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty2(hydra.ext.owl.syntax.ObjectPropertyExpression property2) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
}