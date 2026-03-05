// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataAllValuesFrom implements Serializable, Comparable<DataAllValuesFrom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataAllValuesFrom");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property;
  
  public final hydra.ext.org.w3.owl.syntax.DataRange range;
  
  public DataAllValuesFrom (java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property, hydra.ext.org.w3.owl.syntax.DataRange range) {
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataAllValuesFrom)) {
      return false;
    }
    DataAllValuesFrom o = (DataAllValuesFrom) other;
    return java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.range,
      o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(property) + 3 * java.util.Objects.hashCode(range);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataAllValuesFrom other) {
    int cmp = 0;
    cmp = Integer.compare(
      property.hashCode(),
      other.property.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) range).compareTo(other.range);
  }
  
  public DataAllValuesFrom withProperty(java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property) {
    return new DataAllValuesFrom(property, range);
  }
  
  public DataAllValuesFrom withRange(hydra.ext.org.w3.owl.syntax.DataRange range) {
    return new DataAllValuesFrom(property, range);
  }
}
