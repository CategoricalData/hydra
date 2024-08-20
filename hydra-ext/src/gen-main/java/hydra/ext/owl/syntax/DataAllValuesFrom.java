// Note: this is an automatically generated file. Do not edit.

package hydra.ext.owl.syntax;

import java.io.Serializable;

public class DataAllValuesFrom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/owl/syntax.DataAllValuesFrom");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_RANGE = new hydra.core.Name("range");
  
  public final java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property;
  
  public final hydra.ext.owl.syntax.DataRange range;
  
  public DataAllValuesFrom (java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property, hydra.ext.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((range));
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataAllValuesFrom)) {
      return false;
    }
    DataAllValuesFrom o = (DataAllValuesFrom) (other);
    return property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * range.hashCode();
  }
  
  public DataAllValuesFrom withProperty(java.util.List<hydra.ext.owl.syntax.DataPropertyExpression> property) {
    java.util.Objects.requireNonNull((property));
    return new DataAllValuesFrom(property, range);
  }
  
  public DataAllValuesFrom withRange(hydra.ext.owl.syntax.DataRange range) {
    java.util.Objects.requireNonNull((range));
    return new DataAllValuesFrom(property, range);
  }
}
