// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Xml implements Serializable, Comparable<Pat_Xml> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Xml");

  public static final hydra.core.Name PARTS = new hydra.core.Name("parts");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Pat> args;

  public Pat_Xml (hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts, hydra.util.ConsList<hydra.ext.scala.meta.Pat> args) {
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
    cmp = ((Comparable) parts).compareTo(other.parts);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Pat_Xml withParts(hydra.util.ConsList<hydra.ext.scala.meta.Lit> parts) {
    return new Pat_Xml(parts, args);
  }

  public Pat_Xml withArgs(hydra.util.ConsList<hydra.ext.scala.meta.Pat> args) {
    return new Pat_Xml(parts, args);
  }
}
