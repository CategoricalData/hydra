// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class GYear implements Serializable, Comparable<GYear> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.GYear");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public GYear (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof GYear)) {
      return false;
    }
    GYear o = (GYear) other;
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
  public int compareTo(GYear other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
