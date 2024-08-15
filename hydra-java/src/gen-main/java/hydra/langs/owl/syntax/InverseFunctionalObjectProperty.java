// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class InverseFunctionalObjectProperty implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/owl/syntax.InverseFunctionalObjectProperty");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.ObjectPropertyExpression property;
  
  public InverseFunctionalObjectProperty (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property));
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseFunctionalObjectProperty)) {
      return false;
    }
    InverseFunctionalObjectProperty o = (InverseFunctionalObjectProperty) (other);
    return annotations.equals(o.annotations) && property.equals(o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode();
  }
  
  public InverseFunctionalObjectProperty withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new InverseFunctionalObjectProperty(annotations, property);
  }
  
  public InverseFunctionalObjectProperty withProperty(hydra.langs.owl.syntax.ObjectPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new InverseFunctionalObjectProperty(annotations, property);
  }
}