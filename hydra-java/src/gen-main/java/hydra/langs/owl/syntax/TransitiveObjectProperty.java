// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class TransitiveObjectProperty implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.TransitiveObjectProperty");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression property;
  
  public TransitiveObjectProperty (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof TransitiveObjectProperty)) {
      return false;
    }
    TransitiveObjectProperty o = (TransitiveObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public TransitiveObjectProperty withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new TransitiveObjectProperty(annotations, property);
  }
  
  public TransitiveObjectProperty withProperty(hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new TransitiveObjectProperty(annotations, property);
  }
}