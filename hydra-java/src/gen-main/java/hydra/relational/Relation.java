// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * A set of distinct n-tuples; a table
 */
public class Relation<V> implements Serializable, Comparable<Relation<V>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.Relation");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.relational.Row<V>> value;
  
  public Relation (java.util.List<hydra.relational.Row<V>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Relation)) {
      return false;
    }
    Relation o = (Relation) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Relation other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
