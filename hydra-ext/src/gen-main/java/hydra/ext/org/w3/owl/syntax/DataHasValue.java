// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataHasValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataHasValue");
  
  public static final hydra.core.Name FIELD_NAME_PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;
  
  public final hydra.ext.org.w3.rdf.syntax.Literal value;
  
  public DataHasValue (hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.rdf.syntax.Literal value) {
    java.util.Objects.requireNonNull((property));
    java.util.Objects.requireNonNull((value));
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
  
  public DataHasValue withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    java.util.Objects.requireNonNull((property));
    return new DataHasValue(property, value);
  }
  
  public DataHasValue withValue(hydra.ext.org.w3.rdf.syntax.Literal value) {
    java.util.Objects.requireNonNull((value));
    return new DataHasValue(property, value);
  }
}