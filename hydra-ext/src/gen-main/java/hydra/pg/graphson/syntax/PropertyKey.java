// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class PropertyKey implements Serializable, Comparable<PropertyKey> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.graphson.syntax.PropertyKey");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public PropertyKey (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyKey)) {
      return false;
    }
    PropertyKey o = (PropertyKey) other;
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
  public int compareTo(PropertyKey other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
