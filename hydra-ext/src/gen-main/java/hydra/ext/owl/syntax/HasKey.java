// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Keys
 */
public class HasKey implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.HasKey");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_OBJECT_PROPERTIES = new hydra.core.Name("objectProperties");
  
  public static final hydra.core.Name FIELD_NAME_DATA_PROPERTIES = new hydra.core.Name("dataProperties");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ClassExpression class_;
  
  public final java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> objectProperties;
  
  public final java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> dataProperties;
  
  public HasKey (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ClassExpression class_, java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> objectProperties, java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> dataProperties) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((class_));
    java.util.Objects.requireNonNull((objectProperties));
    java.util.Objects.requireNonNull((dataProperties));
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
    HasKey o = (HasKey) (other);
    return annotations.equals(o.annotations) && class_.equals(o.class_) && objectProperties.equals(o.objectProperties) && dataProperties.equals(o.dataProperties);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * class_.hashCode() + 5 * objectProperties.hashCode() + 7 * dataProperties.hashCode();
  }
  
  public HasKey withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withClass(hydra.ext.owl.syntax.ClassExpression class_) {
    java.util.Objects.requireNonNull((class_));
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withObjectProperties(java.util.List<hydra.ext.owl.syntax.ObjectPropertyExpression> objectProperties) {
    java.util.Objects.requireNonNull((objectProperties));
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withDataProperties(java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> dataProperties) {
    java.util.Objects.requireNonNull((dataProperties));
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
}