// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataSomeValuesFrom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property;
  
  public final hydra.ext.org.w3.owl.syntax.DataRange range;
  
  public DataSomeValuesFrom (java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property, hydra.ext.org.w3.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataSomeValuesFrom)) {
      return false;
    }
    DataSomeValuesFrom o = (DataSomeValuesFrom) (other);
    return property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * range.hashCode();
  }
  
  public DataSomeValuesFrom withProperty(java.util.List<hydra.ext.org.w3.owl.syntax.DataPropertyExpression> property) {
    java.util.Objects.requireNonNull((property));
    return new DataSomeValuesFrom(property, range);
  }
  
  public DataSomeValuesFrom withRange(hydra.ext.org.w3.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((range));
    return new DataSomeValuesFrom(property, range);
  }
}