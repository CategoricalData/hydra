// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A data row, containing optional-valued cells; one per column
 */
public class DataRow<V> implements Serializable, Comparable<DataRow<V>> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.DataRow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.util.Maybe<V>> value;
  
  public DataRow (java.util.List<hydra.util.Maybe<V>> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataRow)) {
      return false;
    }
    DataRow o = (DataRow) (other);
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
  public int compareTo(DataRow other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
