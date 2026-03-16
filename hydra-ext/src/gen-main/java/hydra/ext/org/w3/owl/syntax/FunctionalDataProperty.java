// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class FunctionalDataProperty implements Serializable, Comparable<FunctionalDataProperty> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.FunctionalDataProperty");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public FunctionalDataProperty (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    this.annotations = annotations;
    this.property = property;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionalDataProperty)) {
      return false;
    }
    FunctionalDataProperty o = (FunctionalDataProperty) other;
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
  public int compareTo(FunctionalDataProperty other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) property).compareTo(other.property);
  }
  
  public FunctionalDataProperty withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new FunctionalDataProperty(annotations, property);
  }
  
  public FunctionalDataProperty withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    return new FunctionalDataProperty(annotations, property);
  }
}
