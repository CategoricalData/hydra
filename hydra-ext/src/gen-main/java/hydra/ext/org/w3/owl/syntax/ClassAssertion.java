// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class ClassAssertion implements Serializable, Comparable<ClassAssertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.ClassAssertion");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name INDIVIDUAL = new hydra.core.Name("individual");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.ClassExpression class_;

  public final hydra.ext.org.w3.owl.syntax.Individual individual;

  public ClassAssertion (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ClassExpression class_, hydra.ext.org.w3.owl.syntax.Individual individual) {
    this.annotations = annotations;
    this.class_ = class_;
    this.individual = individual;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ClassAssertion)) {
      return false;
    }
    ClassAssertion o = (ClassAssertion) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.class_,
      o.class_) && java.util.Objects.equals(
      this.individual,
      o.individual);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(class_) + 5 * java.util.Objects.hashCode(individual);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ClassAssertion other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) class_).compareTo(other.class_);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) individual).compareTo(other.individual);
  }

  public ClassAssertion withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new ClassAssertion(annotations, class_, individual);
  }

  public ClassAssertion withClass(hydra.ext.org.w3.owl.syntax.ClassExpression class_) {
    return new ClassAssertion(annotations, class_, individual);
  }

  public ClassAssertion withIndividual(hydra.ext.org.w3.owl.syntax.Individual individual) {
    return new ClassAssertion(annotations, class_, individual);
  }
}
