// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataPropertyRange implements Serializable, Comparable<DataPropertyRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyRange");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.org.w3.owl.syntax.ClassExpression range;
  
  public DataPropertyRange (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations, hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    this.annotations = annotations;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyRange)) {
      return false;
    }
    DataPropertyRange o = (DataPropertyRange) other;
    return java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.range,
      o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(annotations) + 3 * java.util.Objects.hashCode(property) + 5 * java.util.Objects.hashCode(range);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataPropertyRange other) {
    int cmp = 0;
    cmp = ((Comparable) annotations).compareTo(other.annotations);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) range).compareTo(other.range);
  }
  
  public DataPropertyRange withAnnotations(hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withRange(hydra.ext.org.w3.owl.syntax.ClassExpression range) {
    return new DataPropertyRange(annotations, property, range);
  }
}
