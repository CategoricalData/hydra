// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class DataHasValue implements Serializable, Comparable<DataHasValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataHasValue");

  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.org.w3.owl.syntax.DataPropertyExpression property;

  public final hydra.ext.org.w3.rdf.syntax.Literal value;

  public DataHasValue (hydra.ext.org.w3.owl.syntax.DataPropertyExpression property, hydra.ext.org.w3.rdf.syntax.Literal value) {
    this.property = property;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataHasValue)) {
      return false;
    }
    DataHasValue o = (DataHasValue) other;
    return java.util.Objects.equals(
      this.property,
      o.property) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(property) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(DataHasValue other) {
    int cmp = 0;
    cmp = ((Comparable) property).compareTo(other.property);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }

  public DataHasValue withProperty(hydra.ext.org.w3.owl.syntax.DataPropertyExpression property) {
    return new DataHasValue(property, value);
  }

  public DataHasValue withValue(hydra.ext.org.w3.rdf.syntax.Literal value) {
    return new DataHasValue(property, value);
  }
}
