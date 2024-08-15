// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.mappings;

import java.io.Serializable;

/**
 * A mapping specification producing properties of a specified key, and values of the appropriate type.
 */
public class PropertySpec implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.PropertySpec");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  /**
   * The key of the target properties
   */
  public final hydra.langs.tinkerpop.propertyGraph.PropertyKey key;
  
  /**
   * A specification of the value of each target property, which must conform to the type associated with the property key
   */
  public final hydra.langs.tinkerpop.mappings.ValueSpec value;
  
  public PropertySpec (hydra.langs.tinkerpop.propertyGraph.PropertyKey key, hydra.langs.tinkerpop.mappings.ValueSpec value) {
    java.util.Objects.requireNonNull((key));
    java.util.Objects.requireNonNull((value));
    this.key = key;
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertySpec)) {
      return false;
    }
    PropertySpec o = (PropertySpec) (other);
    return key.equals(o.key) && value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * key.hashCode() + 3 * value.hashCode();
  }
  
  public PropertySpec withKey(hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertySpec(key, value);
  }
  
  public PropertySpec withValue(hydra.langs.tinkerpop.mappings.ValueSpec value) {
    java.util.Objects.requireNonNull((value));
    return new PropertySpec(key, value);
  }
}