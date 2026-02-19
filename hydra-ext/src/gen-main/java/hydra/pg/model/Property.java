// Note: this is an automatically generated file. Do not edit.

package hydra.pg.model;

import java.io.Serializable;

/**
 * A key/value property
 */
public class Property<V> implements Serializable, Comparable<Property<V>> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.model.Property");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  /**
   * They key of the property
   */
  public final hydra.pg.model.PropertyKey key;
  
  /**
   * The value of the property
   */
  public final V value;
  
  public Property (hydra.pg.model.PropertyKey key, V value) {
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) other;
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
  public int compareTo(Property other) {
    int cmp = 0;
    cmp = ((Comparable) key).compareTo(other.key);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) value).compareTo(other.value);
  }
  
  public Property withKey(hydra.pg.model.PropertyKey key) {
    return new Property(key, value);
  }
  
  public Property withValue(V value) {
    return new Property(key, value);
  }
}
