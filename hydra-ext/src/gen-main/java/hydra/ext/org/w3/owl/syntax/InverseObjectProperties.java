// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class InverseObjectProperties implements Serializable, Comparable<InverseObjectProperties> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.InverseObjectProperties");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY1 = new hydra.core.Name("property1");
  
  public static final hydra.core.Name PROPERTY2 = new hydra.core.Name("property2");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1;
  
  public final hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2;
  
  public InverseObjectProperties (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1, hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2) {
    this.annotations = annotations;
    this.property1 = property1;
    this.property2 = property2;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InverseObjectProperties)) {
      return false;
    }
    InverseObjectProperties o = (InverseObjectProperties) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property1,
      o.property1) && java.util.Objects.equals(
      this.property2,
      o.property2);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property1) + 5 * java.util.Objects.hashCode(property2);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(InverseObjectProperties other) {
    int cmp = 0;
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property1).compareTo(other.property1);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) property2).compareTo(other.property2);
  }
  
  public InverseObjectProperties withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty1(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property1) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
  
  public InverseObjectProperties withProperty2(hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression property2) {
    return new InverseObjectProperties(annotations, property1, property2);
  }
}
