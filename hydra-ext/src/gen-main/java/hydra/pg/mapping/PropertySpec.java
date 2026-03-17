// Note: this is an automatically generated file. Do not edit.

package hydra.pg.mapping;

import java.io.Serializable;

/**
 * A mapping specification producing properties of a specified key, and values of the appropriate type.
 */
public class PropertySpec implements Serializable, Comparable<PropertySpec> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.mapping.PropertySpec");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  /**
   * The key of the target properties
   */
  public final hydra.pg.model.PropertyKey key;

  /**
   * A specification of the value of each target property, which must conform to the type associated with the property key
   */
  public final hydra.pg.mapping.ValueSpec value;

  public PropertySpec (hydra.pg.model.PropertyKey key, hydra.pg.mapping.ValueSpec value) {
    this.key = key;
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertySpec)) {
      return false;
    }
    PropertySpec o = (PropertySpec) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertySpec other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }

  public PropertySpec withKey(hydra.pg.model.PropertyKey key) {
    return new PropertySpec(key, value);
  }

  public PropertySpec withValue(hydra.pg.mapping.ValueSpec value) {
    return new PropertySpec(key, value);
  }
}
