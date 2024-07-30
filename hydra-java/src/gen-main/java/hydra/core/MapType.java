// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A map type
 */
public class MapType implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.MapType");
  
  public final hydra.core.Type keys;
  
  public final hydra.core.Type values;
  
  public MapType (hydra.core.Type keys, hydra.core.Type values) {
    java.util.Objects.requireNonNull((keys));
    java.util.Objects.requireNonNull((values));
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
      return false;
    }
    MapType o = (MapType) (other);
    return keys.equals(o.keys) && values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode() + 3 * values.hashCode();
  }
  
  public MapType withKeys(hydra.core.Type keys) {
    java.util.Objects.requireNonNull((keys));
    return new MapType(keys, values);
  }
  
  public MapType withValues(hydra.core.Type values) {
    java.util.Objects.requireNonNull((values));
    return new MapType(keys, values);
  }
}