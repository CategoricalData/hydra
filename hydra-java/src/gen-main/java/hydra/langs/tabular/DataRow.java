package hydra.langs.tabular;

import java.io.Serializable;

/**
 * A data row, containing optional-valued cells; one per column
 */
public class DataRow<V> implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tabular.DataRow");
  
  public final java.util.List<java.util.Optional<V>> value;
  
  public DataRow (java.util.List<java.util.Optional<V>> value) {
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