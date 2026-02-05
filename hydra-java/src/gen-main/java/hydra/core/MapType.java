// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A map type
 */
public class MapType implements Serializable, Comparable<MapType> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.MapType");
  
  public static final hydra.core.Name FIELD_NAME_KEYS = new hydra.core.Name("keys");
  
  public static final hydra.core.Name FIELD_NAME_VALUES = new hydra.core.Name("values");
  
  /**
   * The type of keys in the map
   */
  public final hydra.core.Type keys;
  
  /**
   * The type of values in the map
   */
  public final hydra.core.Type values;
  
  public MapType (hydra.core.Type keys, hydra.core.Type values) {
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
      return false;
    }
    MapType o = (MapType) (other);
    return java.util.Objects.equals(
      this.keys,
      o.keys) && java.util.Objects.equals(
      this.values,
      o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(keys) + 3 * java.util.Objects.hashCode(values);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(MapType other) {
    int cmp = 0;
    cmp = ((Comparable) (keys)).compareTo(other.keys);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (values)).compareTo(other.values);
  }
  
  public MapType withKeys(hydra.core.Type keys) {
    return new MapType(keys, values);
  }
  
  public MapType withValues(hydra.core.Type values) {
    return new MapType(keys, values);
  }
}
