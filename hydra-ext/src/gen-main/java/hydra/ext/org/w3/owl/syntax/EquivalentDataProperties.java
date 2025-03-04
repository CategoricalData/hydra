// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class EquivalentDataProperties implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentDataProperties");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTIES = new hydra.core.Name("properties");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> properties;
  
  public EquivalentDataProperties (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> properties) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((properties));
    this.annotations = annotations;
    this.properties = properties;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EquivalentDataProperties)) {
      return false;
    }
    EquivalentDataProperties o = (EquivalentDataProperties) (other);
    return annotations.equals(o.annotations) && properties.equals(o.properties);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * properties.hashCode();
  }
  
  public EquivalentDataProperties withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new EquivalentDataProperties(annotations, properties);
  }
  
  public EquivalentDataProperties withProperties(java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> properties) {
    java.util.Objects.requireNonNull((properties));
    return new EquivalentDataProperties(annotations, properties);
  }
}