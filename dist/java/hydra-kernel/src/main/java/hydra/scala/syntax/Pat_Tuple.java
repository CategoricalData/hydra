// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public class Pat_Tuple implements Serializable, Comparable<Pat_Tuple> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Pat_Tuple");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final java.util.List<hydra.scala.syntax.Pat> args;

  public Pat_Tuple (java.util.List<hydra.scala.syntax.Pat> args) {
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Tuple)) {
      return false;
    }
    Pat_Tuple o = (Pat_Tuple) other;
    return java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Tuple other) {
    return hydra.util.Comparing.compare(
      args,
      other.args);
  }
}
