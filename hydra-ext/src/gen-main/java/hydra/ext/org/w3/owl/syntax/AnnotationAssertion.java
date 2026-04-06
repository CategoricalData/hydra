// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class AnnotationAssertion implements Serializable, Comparable<AnnotationAssertion> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationAssertion");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name SUBJECT = new hydra.core.Name("subject");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;

  public final hydra.ext.org.w3.owl.syntax.AnnotationProperty property;

  public final hydra.ext.org.w3.owl.syntax.AnnotationSubject subject;

  public final hydra.ext.org.w3.owl.syntax.AnnotationValue value;

  public AnnotationAssertion (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.AnnotationProperty property, hydra.ext.org.w3.owl.syntax.AnnotationSubject subject, hydra.ext.org.w3.owl.syntax.AnnotationValue value) {
    this.annotations = annotations;
    this.property = property;
    this.subject = subject;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AnnotationAssertion)) {
      return false;
    }
    AnnotationAssertion o = (AnnotationAssertion) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.subject,
      o.subject) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(subject) + 7 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AnnotationAssertion other) {
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
      subject,
      other.subject);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }

  public AnnotationAssertion withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new AnnotationAssertion(annotations, property, subject, value);
  }

  public AnnotationAssertion withProperty(hydra.ext.org.w3.owl.syntax.AnnotationProperty property) {
    return new AnnotationAssertion(annotations, property, subject, value);
  }

  public AnnotationAssertion withSubject(hydra.ext.org.w3.owl.syntax.AnnotationSubject subject) {
    return new AnnotationAssertion(annotations, property, subject, value);
  }

  public AnnotationAssertion withValue(hydra.ext.org.w3.owl.syntax.AnnotationValue value) {
    return new AnnotationAssertion(annotations, property, subject, value);
  }
}
