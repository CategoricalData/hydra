// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class IrreflexiveObjectProperty implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.IrreflexiveObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public final java.util.List<hydra.ext.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.owl.syntax.ObjectPropertyExpression property;
  
  public IrreflexiveObjectProperty (java.util.List<hydra.ext.owl.syntax.Annotation> annotations, hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IrreflexiveObjectProperty)) {
      return false;
    }
    IrreflexiveObjectProperty o = (IrreflexiveObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public IrreflexiveObjectProperty withAnnotations(java.util.List<hydra.ext.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new IrreflexiveObjectProperty(annotations, property);
  }
  
  public IrreflexiveObjectProperty withProperty(hydra.ext.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new IrreflexiveObjectProperty(annotations, property);
  }
}
