// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of a property
 */
public class PropertyType<T> implements Serializable, Comparable<PropertyType<T>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.PropertyType");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public static final hydra.core.Name REQUIRED = new hydra.core.Name("required");

  /**
   * A property's key
   */
  public final hydra.pg.model.PropertyKey key;

  /**
   * The type of a property's value
   */
  public final T value;

  /**
   * Whether the property is required; values may be omitted from a property map otherwise
   */
  public final Boolean required;

  public PropertyType (hydra.pg.model.PropertyKey key, T value, Boolean required) {
    this.key = key;
    this.value = value;
    this.required = required;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyType)) {
      return false;
    }
    PropertyType o = (PropertyType) other;
    return java.util.Objects.equals(
      this.key,
      o.key) && java.util.Objects.equals(
      this.value,
      o.value) && java.util.Objects.equals(
      this.required,
      o.required);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(key) + 3 * java.util.Objects.hashCode(value) + 5 * java.util.Objects.hashCode(required);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyType other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) value).compareTo(other.value);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) required).compareTo(other.required);
  }

  public PropertyType withKey(hydra.pg.model.PropertyKey key) {
    return new PropertyType(key, value, required);
  }

  public PropertyType withValue(T value) {
    return new PropertyType(key, value, required);
  }

  public PropertyType withRequired(Boolean required) {
    return new PropertyType(key, value, required);
  }
}
