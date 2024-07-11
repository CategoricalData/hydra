// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.queries;

import java.io.Serializable;

public class PropertyProjection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/queries.PropertyProjection");
  
  public final hydra.langs.tinkerpop.queries.Expression base;
  
  public final hydra.langs.tinkerpop.propertyGraph.PropertyKey key;
  
  public PropertyProjection (hydra.langs.tinkerpop.queries.Expression base, hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((base));
    java.util.Objects.requireNonNull((key));
    this.base = base;
    this.key = key;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PropertyProjection)) {
      return false;
    }
    PropertyProjection o = (PropertyProjection) (other);
    return base.equals(o.base) && key.equals(o.key);
  }
  
  @Override
  public int hashCode() {
    return 2 * base.hashCode() + 3 * key.hashCode();
  }
  
  public PropertyProjection withBase(hydra.langs.tinkerpop.queries.Expression base) {
    java.util.Objects.requireNonNull((base));
    return new PropertyProjection(base, key);
  }
  
  public PropertyProjection withKey(hydra.langs.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertyProjection(base, key);
  }
}