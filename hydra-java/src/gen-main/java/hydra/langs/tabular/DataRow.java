// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tabular;

import java.io.Serializable;

/**
 * A data row, containing optional-valued cells; one per column
 */
public class DataRow<V> implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/tabular.DataRow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<hydra.util.Opt<V>> value;
  
  public DataRow (java.util.List<hydra.util.Opt<V>> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataRow)) {
      return false;
    }
    DataRow o = (DataRow) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}