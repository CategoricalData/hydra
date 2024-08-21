// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.queries;

import java.io.Serializable;

public class PropertyProjection implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/org/apache/tinkerpop/queries.PropertyProjection");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_KEY = new hydra.core.Name("key");
  
  public final hydra.ext.org.apache.tinkerpop.queries.Expression base;
  
  public final hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key;
  
  public PropertyProjection (hydra.ext.org.apache.tinkerpop.queries.Expression base, hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key) {
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
  
  public PropertyProjection withBase(hydra.ext.org.apache.tinkerpop.queries.Expression base) {
    java.util.Objects.requireNonNull((base));
    return new PropertyProjection(base, key);
  }
  
  public PropertyProjection withKey(hydra.ext.org.apache.tinkerpop.propertyGraph.PropertyKey key) {
    java.util.Objects.requireNonNull((key));
    return new PropertyProjection(base, key);
  }
}