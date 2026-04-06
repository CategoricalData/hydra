// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.xml.schema;

import java.io.Serializable;

public class NOTATION implements Serializable, Comparable<NOTATION> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.xml.schema.NOTATION");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public NOTATION (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NOTATION)) {
      return false;
    }
    NOTATION o = (NOTATION) other;
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
  public int compareTo(NOTATION other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
