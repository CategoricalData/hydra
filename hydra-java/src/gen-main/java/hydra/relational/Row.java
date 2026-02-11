// Note: this is an automatically generated file. Do not edit.

package hydra.relational;

import java.io.Serializable;

/**
 * An n-tuple which is an element of a given relation
 */
public class Row<V> implements Serializable, Comparable<Row<V>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.relational.Row");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<V> value;
  
  public Row (java.util.List<V> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Row)) {
      return false;
    }
    Row o = (Row) other;
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
  public int compareTo(Row other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
