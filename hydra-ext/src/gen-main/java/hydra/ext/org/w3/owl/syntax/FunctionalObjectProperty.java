// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class FunctionalObjectProperty implements Serializable, Comparable<FunctionalObjectProperty> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property;
  
  public FunctionalObjectProperty (java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionalObjectProperty)) {
      return false;
    }
    FunctionalObjectProperty o = (FunctionalObjectProperty) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionalObjectProperty other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) property).compareTo(other.property);
  }
  
  public FunctionalObjectProperty withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new FunctionalObjectProperty(annotations, property);
  }
  
  public FunctionalObjectProperty withProperty(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property) {
    return new FunctionalObjectProperty(annotations, property);
  }
}
