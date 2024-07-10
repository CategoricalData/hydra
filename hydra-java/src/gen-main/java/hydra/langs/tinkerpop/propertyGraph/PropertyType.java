// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * The type of a property
 */
public class PropertyType<T> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/propertyGraph.PropertyType");
  
  /**
   * A property's key
   */
  public final hydra.langs.tinkerpop.propertyGraph.PropertyKey key;
  
  /**
   * The type of a property's value
   */
  public final T value;
  
  /**
   * Whether the property is required; values may be omitted from a property map otherwise
   */
  public final Boolean required;
  
  public PropertyType (hydra.langs.tinkerpop.propertyGraph.PropertyKey key, T value, Boolean required) {
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    if (required == null) {
      throw new IllegalArgumentException("null value for 'required' argument");
    }
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
  
  public PropertyType withKey(hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    if (key == null) {
      throw new IllegalArgumentException("null value for 'key' argument");
    }
    return new PropertyType(key, value, required);
  }
  
  public PropertyType withValue(T value) {
    if (value == null) {
      throw new IllegalArgumentException("null value for 'value' argument");
    }
    return new PropertyType(key, value, required);
  }
  
  public PropertyType withRequired(Boolean required) {
    if (required == null) {
      throw new IllegalArgumentException("null value for 'required' argument");
    }
    return new PropertyType(key, value, required);
  }
}