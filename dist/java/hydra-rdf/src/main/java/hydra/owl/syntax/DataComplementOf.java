// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges
 */
public class DataComplementOf implements Serializable, Comparable<DataComplementOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DataComplementOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final hydra.owl.syntax.DataRange value;

  public DataComplementOf (hydra.owl.syntax.DataRange value) {
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
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
