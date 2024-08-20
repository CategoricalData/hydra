// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ClassAssertion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/w3/owl/syntax.ClassAssertion");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_INDIVIDUAL = new hydra.core.Name("individual");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression class_;
  
  public final hydra.ext.org.w3.owl.syntax.Individual individual;
  
  public ClassAssertion (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ClassExpression class_, hydra.ext.org.w3.owl.syntax.Individual individual) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((class_));
    java.util.Objects.requireNonNull((individual));
    this.annotations = annotations;
    this.class_ = class_;
    this.individual = individual;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassAssertion)) {
      return false;
    }
    ClassAssertion o = (ClassAssertion) (other);
    return annotations.equals(o.annotations) && class_.equals(o.class_) && individual.equals(o.individual);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * class_.hashCode() + 5 * individual.hashCode();
  }
  
  public ClassAssertion withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new ClassAssertion(annotations, class_, individual);
  }
  
  public ClassAssertion withClass(hydra.ext.org.w3.owl.syntax.ClassExpression class_) {
    java.util.Objects.requireNonNull((class_));
    return new ClassAssertion(annotations, class_, individual);
  }
  
  public ClassAssertion withIndividual(hydra.ext.org.w3.owl.syntax.Individual individual) {
    java.util.Objects.requireNonNull((individual));
    return new ClassAssertion(annotations, class_, individual);
  }
}