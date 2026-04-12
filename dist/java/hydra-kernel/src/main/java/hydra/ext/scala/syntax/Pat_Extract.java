// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public class Pat_Extract implements Serializable, Comparable<Pat_Extract> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Pat_Extract");

  public static final hydra.core.Name FUN = new hydra.core.Name("fun");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.syntax.Data fun;

  public final java.util.List<hydra.ext.scala.syntax.Pat> args;

  public Pat_Extract (hydra.ext.scala.syntax.Data fun, java.util.List<hydra.ext.scala.syntax.Pat> args) {
    this.fun = fun;
    this.args = args;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Extract)) {
      return false;
    }
    Pat_Extract o = (Pat_Extract) other;
    return java.util.Objects.equals(
      this.fun,
      o.fun) && java.util.Objects.equals(
      this.args,
      o.args);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(fun) + 3 * java.util.Objects.hashCode(args);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Pat_Extract other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      fun,
      other.fun);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      args,
      other.args);
  }

  public Pat_Extract withFun(hydra.ext.scala.syntax.Data fun) {
    return new Pat_Extract(fun, args);
  }

  public Pat_Extract withArgs(java.util.List<hydra.ext.scala.syntax.Pat> args) {
    return new Pat_Extract(fun, args);
  }
}
