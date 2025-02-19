// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class NegativeObjectPropertyAssertion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_SOURCE = new hydra.core.Name("source");
  
  public static final hydra.core.Name FIELD_NAME_TARGET = new hydra.core.Name("target");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.Individual source;
  
  public final hydra.ext.org.w3.owl.syntax.Individual target;
  
  public NegativeObjectPropertyAssertion (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.Individual source, hydra.ext.org.w3.owl.syntax.Individual target) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((source));
    java.util.Objects.requireNonNull((target));
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
  
  public NegativeObjectPropertyAssertion withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withSource(hydra.ext.org.w3.owl.syntax.Individual source) {
    java.util.Objects.requireNonNull((source));
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
  
  public NegativeObjectPropertyAssertion withTarget(hydra.ext.org.w3.owl.syntax.Individual target) {
    java.util.Objects.requireNonNull((target));
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
}