package hydra.core;

/**
 * A map type
 */
public class MapType<M> {
  public final Type<M> keys;
  
  public final Type<M> values;
  
  public MapType (Type<M> keys, Type<M> values) {
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
  
  public MapType withKeys(Type<M> keys) {
    return new MapType(keys, values);
  }
  
  public MapType withValues(Type<M> values) {
    return new MapType(keys, values);
  }
}