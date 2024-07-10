// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Keys
 */
public class HasKey implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.HasKey");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.ClassExpression class_;
  
  public final java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> objectProperties;
  
  public final java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> dataProperties;
  
  public HasKey (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.ClassExpression class_, java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> objectProperties, java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> dataProperties) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (class_ == null) {
      throw new IllegalArgumentException("null value for 'class' argument");
    }
    if (objectProperties == null) {
      throw new IllegalArgumentException("null value for 'objectProperties' argument");
    }
    if (dataProperties == null) {
      throw new IllegalArgumentException("null value for 'dataProperties' argument");
    }
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
  
  public HasKey withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withClass(hydra.langs.owl.syntax.ClassExpression class_) {
    if (class_ == null) {
      throw new IllegalArgumentException("null value for 'class' argument");
    }
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withObjectProperties(java.util.List<hydra.langs.owl.syntax.ObjectPropertyExpression> objectProperties) {
    if (objectProperties == null) {
      throw new IllegalArgumentException("null value for 'objectProperties' argument");
    }
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
  
  public HasKey withDataProperties(java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> dataProperties) {
    if (dataProperties == null) {
      throw new IllegalArgumentException("null value for 'dataProperties' argument");
    }
    return new HasKey(annotations, class_, objectProperties, dataProperties);
  }
}