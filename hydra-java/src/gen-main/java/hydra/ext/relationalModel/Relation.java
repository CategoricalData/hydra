// Note: this is an automatically generated file. Do not edit.

package hydra.ext.relationalModel;

import java.io.Serializable;

/**
 * A set of distinct n-tuples; a table
 */
public class Relation<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.relationalModel.Relation");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.ext.relationalModel.Row<V>> value;
  
  public Relation (java.util.List<hydra.ext.relationalModel.Row<V>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relation)) {
      return false;
    }
    Relation o = (Relation) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}