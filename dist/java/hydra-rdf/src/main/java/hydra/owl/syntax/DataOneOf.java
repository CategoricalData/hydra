// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals
 */
public class DataOneOf implements Serializable, Comparable<DataOneOf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.DataOneOf");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.util.List<hydra.rdf.syntax.Literal> value;

  public DataOneOf (java.util.List<hydra.rdf.syntax.Literal> value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataOneOf)) {
      return false;
    }
    DataOneOf o = (DataOneOf) other;
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
  public int compareTo(DataOneOf other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
