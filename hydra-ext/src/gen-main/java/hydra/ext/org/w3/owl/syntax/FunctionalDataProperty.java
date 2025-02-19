// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class FunctionalDataProperty implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.FunctionalDataProperty");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public FunctionalDataProperty (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionalDataProperty)) {
      return false;
    }
    FunctionalDataProperty o = (FunctionalDataProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public FunctionalDataProperty withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new FunctionalDataProperty(annotations, property);
  }
  
  public FunctionalDataProperty withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new FunctionalDataProperty(annotations, property);
  }
}