// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * The type of a property
 */
public class PropertyType<T> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.model.PropertyType");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name FIELD_NAME_REQUIRED = new hydra.core.Name("required");
  
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
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    java.util.Objects.requireNonNull((required));
    this.key = key;
    this.value = value;
    this.required = required;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyType)) {
      return false;
    }
    PropertyType o = (PropertyType) (other);
    return key.equals(o.key) && value.equals(o.value) && required.equals(o.required);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode() + 5 * required.hashCode();
  }
  
  public PropertyType withKey(hydra.pg.model.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertyType(key, value, required);
  }
  
  public PropertyType withValue(T value) {
    java.util.Objects.requireNonNull((value));
    return new PropertyType(key, value, required);
  }
  
  public PropertyType withRequired(Boolean required) {
    java.util.Objects.requireNonNull((required));
    return new PropertyType(key, value, required);
  }
}