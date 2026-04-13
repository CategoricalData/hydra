// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Xml implements Serializable, Comparable<Pat_Xml> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Xml");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final java.util.List<hydra.scala.syntax.Lit> parts;

  public final java.util.List<hydra.scala.syntax.Pat> args;

  public Pat_Xml (java.util.List<hydra.scala.syntax.Lit> parts, java.util.List<hydra.scala.syntax.Pat> args) {
    this.parts = parts;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Xml)) {
      return false;
    }
    Pat_Xml o = (Pat_Xml) other;
    return java.util.Objects.equals(
      this.parts,
      o.parts) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(parts) + 3 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Xml other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      parts,
      other.parts);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      args,
      other.args);
  }

  public Pat_Xml withParts(java.util.List<hydra.scala.syntax.Lit> parts) {
    return new Pat_Xml(parts, args);
  }

  public Pat_Xml withArgs(java.util.List<hydra.scala.syntax.Pat> args) {
    return new Pat_Xml(parts, args);
  }
}
