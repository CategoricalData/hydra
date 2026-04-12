// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class NegativeDataPropertyAssertion implements Serializable, Comparable<NegativeDataPropertyAssertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name TARGET = new hydra.core.Name("target");

  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;

  public final hydra.ext.org.w3.owl.syntax.Individual source;

  public final hydra.ext.org.w3.owl.syntax.Individual target;

  public NegativeDataPropertyAssertion (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.owl.syntax.Individual source, hydra.ext.org.w3.owl.syntax.Individual target) {
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
    NegativeDataPropertyAssertion o = (NegativeDataPropertyAssertion) other;
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
  public int compareTo(NegativeDataPropertyAssertion other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      property,
      other.property);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      source,
      other.source);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      target,
      other.target);
  }

  public NegativeDataPropertyAssertion withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }

  public NegativeDataPropertyAssertion withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }

  public NegativeDataPropertyAssertion withSource(hydra.ext.org.w3.owl.syntax.Individual source) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }

  public NegativeDataPropertyAssertion withTarget(hydra.ext.org.w3.owl.syntax.Individual target) {
    return new NegativeDataPropertyAssertion(annotations, property, source, target);
  }
}
