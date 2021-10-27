package hydra.core;

public class MapType {
  public final hydra.core.Type keys;
  
  public final hydra.core.Type values;
  
  /**
   * Constructs an immutable MapType object
   */
  public MapType(hydra.core.Type keys, hydra.core.Type values) {
    this.keys = keys;
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MapType)) {
        return false;
    }
    MapType o = (MapType) other;
    return keys.equals(o.keys)
        && values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * keys.hashCode()
        + 3 * values.hashCode();
  }
  
  /**
   * Construct a new immutable MapType object in which keys is overridden
   */
  public MapType withKeys(hydra.core.Type keys) {
    return new MapType(keys, values);
  }
  
  /**
   * Construct a new immutable MapType object in which values is overridden
   */
  public MapType withValues(hydra.core.Type values) {
    return new MapType(keys, values);
  }
}
