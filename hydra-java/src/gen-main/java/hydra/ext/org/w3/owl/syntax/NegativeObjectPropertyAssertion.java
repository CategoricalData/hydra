// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class NegativeObjectPropertyAssertion implements Serializable, Comparable<NegativeObjectPropertyAssertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;

  public final hydra.ext.org.w3.owl.syntax.Individual source;

  public final hydra.ext.org.w3.owl.syntax.Individual target;

  public NegativeObjectPropertyAssertion (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property, hydra.ext.org.w3.owl.syntax.Individual source, hydra.ext.org.w3.owl.syntax.Individual target) {
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
    NegativeObjectPropertyAssertion o = (NegativeObjectPropertyAssertion) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.source,
      o.source) && java.util.Objects.equals(
      this.target,
      o.target);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(source) + 7 * java.util.Objects.hashCode(target);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NegativeObjectPropertyAssertion other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) source).compareTo(other.source);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) target).compareTo(other.target);
  }

  public NegativeObjectPropertyAssertion withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }

  public NegativeObjectPropertyAssertion withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }

  public NegativeObjectPropertyAssertion withSource(hydra.ext.org.w3.owl.syntax.Individual source) {
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }

  public NegativeObjectPropertyAssertion withTarget(hydra.ext.org.w3.owl.syntax.Individual target) {
    return new NegativeObjectPropertyAssertion(annotations, property, source, target);
  }
}
