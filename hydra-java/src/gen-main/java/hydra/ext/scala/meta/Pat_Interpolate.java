// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Interpolate implements Serializable, Comparable<Pat_Interpolate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Interpolate");

  public static final hydra.core.Name PREFIX = new hydra.core.Name("prefix");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public final hydra.ext.scala.meta.Data_Name prefix;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts;

  public Pat_Interpolate (hydra.ext.scala.meta.Data_Name prefix, hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts) {
    this.prefix = prefix;
    this.parts = parts;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Interpolate)) {
      return false;
    }
    Pat_Interpolate o = (Pat_Interpolate) other;
    return java.util.Objects.equals(
      this.prefix,
      o.prefix) && java.util.Objects.equals(
      this.parts,
      o.parts);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(prefix) + 3 * java.util.Objects.hashCode(parts);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Interpolate other) {
    int cmp = 0;
    cmp = ((Comparable) prefix).compareTo(other.prefix);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) parts).compareTo(other.parts);
  }

  public Pat_Interpolate withPrefix(hydra.ext.scala.meta.Data_Name prefix) {
    return new Pat_Interpolate(prefix, parts);
  }

  public Pat_Interpolate withParts(hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts) {
    return new Pat_Interpolate(prefix, parts);
  }
}
