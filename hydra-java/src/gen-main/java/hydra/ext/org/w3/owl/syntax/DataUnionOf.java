// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges
 */
public class DataUnionOf implements Serializable, Comparable<DataUnionOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataUnionOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataRange> value;

  public DataUnionOf (hydra.util.ConsList<hydra.ext.org.w3.owl.syntax.DataRange> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataUnionOf)) {
      return false;
    }
    DataUnionOf o = (DataUnionOf) other;
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
  public int compareTo(DataUnionOf other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
