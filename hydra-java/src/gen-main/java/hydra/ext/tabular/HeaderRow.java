// Note: this is an automatically generated file. Do not edit.

package hydra.ext.tabular;

import java.io.Serializable;

/**
 * A header row, containing column names (but no types or data)
 */
public class HeaderRow implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.tabular.HeaderRow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<String> value;
  
  public HeaderRow (java.util.List<String> value) {
    java.util.Objects.requireNonNull((value));
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HeaderRow)) {
      return false;
    }
    HeaderRow o = (HeaderRow) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}