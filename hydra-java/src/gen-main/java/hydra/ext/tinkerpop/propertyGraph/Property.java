// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tinkerpop.propertyGraph;

import java.io.Serializable;

/**
 * A key/value property
 */
public class Property<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/tinkerpop/propertyGraph.Property");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  /**
   * They key of the property
   */
  public final hydra.ext.tinkerpop.propertyGraph.PropertyKey key;
  
  /**
   * The value of the property
   */
  public final V value;
  
  public Property (hydra.ext.tinkerpop.propertyGraph.PropertyKey key, V value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Property)) {
      return false;
    }
    Property o = (Property) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public Property withKey(hydra.ext.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new Property(key, value);
  }
  
  public Property withValue(V value) {
    java.util.Objects.requireNonNull((value));
    return new Property(key, value);
  }
}
