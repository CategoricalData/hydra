// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DataSomeValuesFrom implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataSomeValuesFrom");
  
  public final java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> property;
  
  public final hydra.langs.owl.syntax.DataRange range;
  
  public DataSomeValuesFrom (java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> property, hydra.langs.owl.syntax.DataRange range) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
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
  
  public DataSomeValuesFrom withProperty(java.util.List<hydra.langs.owl.syntax.DataPropertyExpression> property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new DataSomeValuesFrom(property, range);
  }
  
  public DataSomeValuesFrom withRange(hydra.langs.owl.syntax.DataRange range) {
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    return new DataSomeValuesFrom(property, range);
  }
}