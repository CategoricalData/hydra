// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges
 */
public class DataComplementOf implements Serializable, Comparable<DataComplementOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataComplementOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.ext.org.w3.owl.syntax.DataRange value;

  public DataComplementOf (hydra.ext.org.w3.owl.syntax.DataRange value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataComplementOf)) {
      return false;
    }
    DataComplementOf o = (DataComplementOf) other;
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
  public int compareTo(DataComplementOf other) {
    return ((Comparable) value).compareTo(other.value);
  }
}
