// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class NegativeObjectPropertyAssertion implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.NegativeObjectPropertyAssertion");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.langs.owl.syntax.Individual source;
  
  public final hydra.langs.owl.syntax.Individual target;
  
  public NegativeObjectPropertyAssertion (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.ObjectPropertyExpression property, hydra.langs.owl.syntax.Individual source, hydra.langs.owl.syntax.Individual target) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    this.annotations = annotations;
    this.property = property;
    this.source = source;
    this.target = target;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NegativeObjectPropertyAssertion)) {
      return false;
    }
    NegativeObjectPropertyAssertion o = (NegativeObjectPropertyAssertion) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && source.equals(o.source) && target.equals(o.target);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * source.hashCode() + 7 * target.hashCode();
  }
  
  public NegativeObjectPropertyAssertion withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withProperty(hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withSource(hydra.langs.owl.syntax.Individual source) {
    if (source == null) {
      throw new IllegalArgumentException("null value for 'source' argument");
    }
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withTarget(hydra.langs.owl.syntax.Individual target) {
    if (target == null) {
      throw new IllegalArgumentException("null value for 'target' argument");
    }
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
}