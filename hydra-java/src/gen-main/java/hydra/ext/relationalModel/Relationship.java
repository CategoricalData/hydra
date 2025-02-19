// Note: this is an automatically generated file. Do not edit.

package hydra.ext.relationalModel;

import java.io.Serializable;

/**
 * A domain-unordered (string-indexed, rather than position-indexed) relation
 */
public class Relationship<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.relationalModel.Relationship");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.Set<java.util.Map<hydra.ext.relationalModel.ColumnName, V>> value;
  
  public Relationship (java.util.Set<java.util.Map<hydra.ext.relationalModel.ColumnName, V>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relationship)) {
      return false;
    }
    Relationship o = (Relationship) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}