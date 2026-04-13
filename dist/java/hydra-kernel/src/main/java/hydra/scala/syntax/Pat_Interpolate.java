// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Interpolate implements Serializable, Comparable<Pat_Interpolate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Interpolate");

  public static final hydra.core.Name PREFIX = new hydra.core.Name("prefix");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public final hydra.scala.syntax.Data_Name prefix;

  public final java.util.List<hydra.scala.syntax.Lit> parts;

  public Pat_Interpolate (hydra.scala.syntax.Data_Name prefix, java.util.List<hydra.scala.syntax.Lit> parts) {
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
    cmp = hydra.util.Comparing.compare(
      prefix,
      other.prefix);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      parts,
      other.parts);
  }

  public Pat_Interpolate withPrefix(hydra.scala.syntax.Data_Name prefix) {
    return new Pat_Interpolate(prefix, parts);
  }

  public Pat_Interpolate withParts(java.util.List<hydra.scala.syntax.Lit> parts) {
    return new Pat_Interpolate(prefix, parts);
  }
}
