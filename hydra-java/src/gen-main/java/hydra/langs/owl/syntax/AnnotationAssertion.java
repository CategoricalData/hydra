// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class AnnotationAssertion implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.AnnotationAssertion");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.AnnotationProperty property;
  
  public final hydra.langs.owl.syntax.AnnotationSubject subject;
  
  public final hydra.langs.owl.syntax.AnnotationValue value;
  
  public AnnotationAssertion (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.AnnotationProperty property, hydra.langs.owl.syntax.AnnotationSubject subject, hydra.langs.owl.syntax.AnnotationValue value) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
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
    AnnotationAssertion o = (AnnotationAssertion) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && subject.equals(o.subject) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * subject.hashCode() + 7 * value.hashCode();
  }
  
  public AnnotationAssertion withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withProperty(hydra.langs.owl.syntax.AnnotationProperty property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withSubject(hydra.langs.owl.syntax.AnnotationSubject subject) {
    if (subject == null) {
      throw new IllegalArgumentException("null value for 'subject' argument");
    }
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withValue(hydra.langs.owl.syntax.AnnotationValue value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new AnnotationAssertion(annotations, property, subject, value);
  }
}