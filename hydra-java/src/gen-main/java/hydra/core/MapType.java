package hydra.core;

/**
 * A map type
 */
public class MapType<M> {
  public final hydra.core.Type<M> keys;
  
  public final hydra.core.Type<M> values;
  
  public MapType (hydra.core.Type<M> keys, hydra.core.Type<M> values) {
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
  
  public MapType withKeys(hydra.core.Type<M> keys) {
    return new MapType(keys, values);
  }
  
  public MapType withValues(hydra.core.Type<M> values) {
    return new MapType(keys, values);
  }
}