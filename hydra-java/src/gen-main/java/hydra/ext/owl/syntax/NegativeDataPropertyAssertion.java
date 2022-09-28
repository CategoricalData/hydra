package hydra.ext.owl.syntax;

public class NegativeDataPropertyAssertion {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/owl/syntax.NegativeDataPropertyAssertion");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.owl.syntax.Individual source;
  
  public final hydra.ext.owl.syntax.Individual target;
  
  public NegativeDataPropertyAssertion (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.DataPropertyExpression property, hydra.ext.owl.syntax.Individual source, hydra.ext.owl.syntax.Individual target) {
    this.annotations = annotations;
    this.property = property;
    this.source = source;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NegativeDataPropertyAssertion)) {
      return false;
    }
    NegativeDataPropertyAssertion o = (NegativeDataPropertyAssertion) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && source.equals(o.source) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * source.hashCode() + 7 * target.hashCode();
  }
  
  public NegativeDataPropertyAssertion withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeDataPropertyAssertion withProperty(hydra.ext.owl.syntax.DataPropertyExpression property) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeDataPropertyAssertion withSource(hydra.ext.owl.syntax.Individual source) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeDataPropertyAssertion withTarget(hydra.ext.owl.syntax.Individual target) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }
}