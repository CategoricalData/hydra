// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Keys
 */
public class HasKey implements Serializable, Comparable<HasKey> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.HasKey");

  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name OBJECT_PROPERTIES = new hydra.core.Name("objectProperties");

  public static final hydra.core.Name DATA_PROPERTIES = new hydra.core.Name("dataProperties");

  public final java.util.List<hydra.owl.syntax.Annotation> annotations;

  public final hydra.owl.syntax.ClassExpression class_;

  public final java.util.List<hydra.owl.syntax.ObjectPropertyExpression> objectProperties;

  public final java.util.List<hydra.owl.syntax.DataPropertyExpression> dataProperties;

  public HasKey (java.util.List<hydra.owl.syntax.Annotation> annotations, hydra.owl.syntax.ClassExpression class_, java.util.List<hydra.owl.syntax.ObjectPropertyExpression> objectProperties, java.util.List<hydra.owl.syntax.DataPropertyExpression> dataProperties) {
    this.annotations = annotations;
    this.class_ = class_;
    this.objectProperties = objectProperties;
    this.dataProperties = dataProperties;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HasKey)) {
      return false;
    }
    HasKey o = (HasKey) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.class_,
      o.class_) && java.util.Objects.equals(
      this.objectProperties,
      o.objectProperties) && java.util.Objects.equals(
      this.dataProperties,
      o.dataProperties);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(class_) + 5 * java.util.Objects.hashCode(objectProperties) + 7 * java.util.Objects.hashCode(dataProperties);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HasKey other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      annotations,
      other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      class_,
      other.class_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      objectProperties,
      other.objectProperties);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      dataProperties,
      other.dataProperties);
  }

  public HasKey withAnnotations(java.util.List<hydra.owl.syntax.Annotation> annotations) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }

  public HasKey withClass(hydra.owl.syntax.ClassExpression class_) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }

  public HasKey withObjectProperties(java.util.List<hydra.owl.syntax.ObjectPropertyExpression> objectProperties) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }

  public HasKey withDataProperties(java.util.List<hydra.owl.syntax.DataPropertyExpression> dataProperties) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
}
