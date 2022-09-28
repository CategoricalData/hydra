package hydra.ext.owl.syntax;

public class DisjointObjectProperties {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.DisjointObjectProperties");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> properties;
  
  public DisjointObjectProperties (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> properties) {
    this.annotations = annotations;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DisjointObjectProperties)) {
      return false;
    }
    DisjointObjectProperties o = (DisjointObjectProperties) (other);
    return annotations.equals(o.annotations) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * properties.hashCode();
  }
  
  public DisjointObjectProperties withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new DisjointObjectProperties(annotations, properties);
  }
  
  public DisjointObjectProperties withProperties(java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> properties) {
    return new DisjointObjectProperties(annotations, properties);
  }
}