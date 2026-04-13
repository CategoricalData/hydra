// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public class PropertyLookup implements Serializable, Comparable<PropertyLookup> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.PropertyLookup");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.cypher.openCypher.PropertyKeyName value;

  public PropertyLookup (hydra.cypher.openCypher.PropertyKeyName value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyLookup)) {
      return false;
    }
    PropertyLookup o = (PropertyLookup) other;
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
  public int compareTo(PropertyLookup other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
