// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class IDREF implements Serializable, Comparable<IDREF> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.IDREF");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public IDREF (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IDREF)) {
      return false;
    }
    IDREF o = (IDREF) other;
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
  public int compareTo(IDREF other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
