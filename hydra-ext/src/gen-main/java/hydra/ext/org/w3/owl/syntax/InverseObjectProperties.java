// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class InverseObjectProperties implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.InverseObjectProperties");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY1 = new hydra.core.Name("property1");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY2 = new hydra.core.Name("property2");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2;
  
  public InverseObjectProperties (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2) {
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((property1));
    java.util.Objects.requireNonNull((property2));
    this.annotations = annotations;
    this.property1 = property1;
    this.property2 = property2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseObjectProperties)) {
      return false;
    }
    InverseObjectProperties o = (InverseObjectProperties) (other);
    return annotations.equals(o.annotations) && property1.equals(o.property1) && property2.equals(o.property2);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property1.hashCode() + 5 * property2.hashCode();
  }
  
  public InverseObjectProperties withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty1(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1) {
    java.util.Objects.requireNonNull((property1));
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty2(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2) {
    java.util.Objects.requireNonNull((property2));
    return new InverseObjectProperties(annotations, property1, property2);
  }
}