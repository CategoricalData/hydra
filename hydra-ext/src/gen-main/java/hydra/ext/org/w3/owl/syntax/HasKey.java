// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Keys
 */
public class HasKey implements Serializable, Comparable<HasKey> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.HasKey");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name OBJECT_PROPERTIES = new hydra.core.Name("objectProperties");
  
  public static final hydra.core.Name DATA_PROPERTIES = new hydra.core.Name("dataProperties");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression class_;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> objectProperties;
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> dataProperties;
  
  public HasKey (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ClassExpression class_, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> objectProperties, hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> dataProperties) {
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
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) class_).compareTo(other.class_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      objectProperties.hashCode(),
      other.objectProperties.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      dataProperties.hashCode(),
      other.dataProperties.hashCode());
  }
  
  public HasKey withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withClass(hydra.ext.org.w3.owl.syntax.ClassExpression class_) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withObjectProperties(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> objectProperties) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withDataProperties(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> dataProperties) {
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
}
