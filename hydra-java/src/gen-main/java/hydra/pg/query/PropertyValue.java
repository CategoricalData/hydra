// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class PropertyValue implements Serializable, Comparable<PropertyValue> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.PropertyValue");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public PropertyValue (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyValue)) {
      return false;
    }
    PropertyValue o = (PropertyValue) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyValue other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
