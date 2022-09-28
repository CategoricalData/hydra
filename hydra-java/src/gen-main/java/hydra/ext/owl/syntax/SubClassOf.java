package hydra.ext.owl.syntax;

public class SubClassOf {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.SubClassOf");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ClassExpression subClass;
  
  public final hydra.ext.owl.syntax.ClassExpression superClass;
  
  public SubClassOf (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ClassExpression subClass, hydra.ext.owl.syntax.ClassExpression superClass) {
    this.annotations = annotations;
    this.subClass = subClass;
    this.superClass = superClass;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SubClassOf)) {
      return false;
    }
    SubClassOf o = (SubClassOf) (other);
    return annotations.equals(o.annotations) && subClass.equals(o.subClass) && superClass.equals(o.superClass);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * subClass.hashCode() + 5 * superClass.hashCode();
  }
  
  public SubClassOf withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSubClass(hydra.ext.owl.syntax.ClassExpression subClass) {
    return new SubClassOf(annotations, subClass, superClass);
  }
  
  public SubClassOf withSuperClass(hydra.ext.owl.syntax.ClassExpression superClass) {
    return new SubClassOf(annotations, subClass, superClass);
  }
}