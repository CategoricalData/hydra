// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class EquivalentObjectProperties implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties;
  
  public EquivalentObjectProperties (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((properties));
    this.annotations = annotations;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentObjectProperties)) {
      return false;
    }
    EquivalentObjectProperties o = (EquivalentObjectProperties) (other);
    return annotations.equals(o.annotations) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * properties.hashCode();
  }
  
  public EquivalentObjectProperties withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new EquivalentObjectProperties(annotations, properties);
  }
  
  public EquivalentObjectProperties withProperties(java.util.List<hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression> properties) {
    java.util.Objects.requireNonNull((properties));
    return new EquivalentObjectProperties(annotations, properties);
  }
}