// Note: this is an automatically generated file. Do not edit.

package hydra.pg.query;

import java.io.Serializable;

public class PropertyProjection implements Serializable, Comparable<PropertyProjection> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.pg.query.PropertyProjection");
  
  public static final hydra.core.Name BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public final hydra.pg.query.Expression base;
  
  public final hydra.pg.model.PropertyKey key;
  
  public PropertyProjection (hydra.pg.query.Expression base, hydra.pg.model.PropertyKey key) {
    this.base = base;
    this.key = key;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyProjection)) {
      return false;
    }
    PropertyProjection o = (PropertyProjection) other;
    return java.util.Objects.equals(
      this.base,
      o.base) && java.util.Objects.equals(
      this.key,
      o.key);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(base) + 3 * java.util.Objects.hashCode(key);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PropertyProjection other) {
    int cmp = 0;
    cmp = ((Comparable) base).compareTo(other.base);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) key).compareTo(other.key);
  }
  
  public PropertyProjection withBase(hydra.pg.query.Expression base) {
    return new PropertyProjection(base, key);
  }
  
  public PropertyProjection withKey(hydra.pg.model.PropertyKey key) {
    return new PropertyProjection(base, key);
  }
}
