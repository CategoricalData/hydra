// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

/**
 * See https://www.w3.org/TR/xmlschema-2/#non-fundamental
 */
public class ConstrainingFacet implements Serializable, Comparable<ConstrainingFacet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.ConstrainingFacet");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final java.lang.Void value;

  public ConstrainingFacet (java.lang.Void value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ConstrainingFacet)) {
      return false;
    }
    ConstrainingFacet o = (ConstrainingFacet) other;
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
  public int compareTo(ConstrainingFacet other) {
    return Integer.compare(
      value.hashCode(),
      other.value.hashCode());
  }
}
