// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DataHasValue implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataHasValue");
  
  public final hydra.langs.owl.syntax.DataPropertyExpression property;
  
  public final hydra.langs.rdf.syntax.Literal value;
  
  public DataHasValue (hydra.langs.owl.syntax.DataPropertyExpression property, hydra.langs.rdf.syntax.Literal value) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    this.property = property;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataHasValue)) {
      return false;
    }
    DataHasValue o = (DataHasValue) (other);
    return property.equals(o.property) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * property.hashCode() + 3 * value.hashCode();
  }
  
  public DataHasValue withProperty(hydra.langs.owl.syntax.DataPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new DataHasValue(property, value);
  }
  
  public DataHasValue withValue(hydra.langs.rdf.syntax.Literal value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new DataHasValue(property, value);
  }
}