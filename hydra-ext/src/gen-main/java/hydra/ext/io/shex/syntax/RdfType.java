// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class RdfType implements Serializable, Comparable<RdfType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.RdfType");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public RdfType (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RdfType)) {
      return false;
    }
    RdfType o = (RdfType) other;
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
  public int compareTo(RdfType other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
