// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class AnnotationAssertion implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.AnnotationAssertion");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_SUBJECT = new hydra.core.Name("subject");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.AnnotationProperty property;
  
  public final hydra.ext.owl.syntax.AnnotationSubject subject;
  
  public final hydra.ext.owl.syntax.AnnotationValue value;
  
  public AnnotationAssertion (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.AnnotationProperty property, hydra.ext.owl.syntax.AnnotationSubject subject, hydra.ext.owl.syntax.AnnotationValue value) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((subject));
    java.util.Objects.requireNonNull((value));
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
  
  public AnnotationAssertion withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withProperty(hydra.ext.owl.syntax.AnnotationProperty property) {
    java.util.Objects.requireNonNull((property));
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withSubject(hydra.ext.owl.syntax.AnnotationSubject subject) {
    java.util.Objects.requireNonNull((subject));
    return new AnnotationAssertion(annotations, property, subject, value);
  }
  
  public AnnotationAssertion withValue(hydra.ext.owl.syntax.AnnotationValue value) {
    java.util.Objects.requireNonNull((value));
    return new AnnotationAssertion(annotations, property, subject, value);
  }
}