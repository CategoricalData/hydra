// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Extract implements Serializable, Comparable<Pat_Extract> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Pat_Extract");

  public static final hydra.core.Name FUN = new hydra.core.Name("fun");

  public static final hydra.core.Name ARGS = new hydra.core.Name("args");

  public final hydra.ext.scala.meta.Data fun;

  public final hydra.util.ConsList<hydra.ext.scala.meta.Pat> args;

  public Pat_Extract (hydra.ext.scala.meta.Data fun, hydra.util.ConsList<hydra.ext.scala.meta.Pat> args) {
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
    cmp = ((Comparable) fun).compareTo(other.fun);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) args).compareTo(other.args);
  }

  public Pat_Extract withFun(hydra.ext.scala.meta.Data fun) {
    return new Pat_Extract(fun, args);
  }

  public Pat_Extract withArgs(hydra.util.ConsList<hydra.ext.scala.meta.Pat> args) {
    return new Pat_Extract(fun, args);
  }
}
