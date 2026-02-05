// Note: this is an automatically generated file. Do not edit.

package hydra.tabular;

import java.io.Serializable;

/**
 * A header row, containing column names (but no types or data)
 */
public class HeaderRow implements Serializable, Comparable<HeaderRow> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.tabular.HeaderRow");
  
  public static final hydra.core.Name FIELD_NAME_VALUE = new hydra.core.Name("value");
  
  public final java.util.List<String> value;
  
  public HeaderRow (java.util.List<String> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HeaderRow)) {
      return false;
    }
    HeaderRow o = (HeaderRow) (other);
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
  public int compareTo(HeaderRow other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
