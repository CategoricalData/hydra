// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Pat_Extract implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Pat_Extract");
  
  public static final hydra.core.Name FIELD_NAME_FUN = new hydra.core.Name("fun");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.scala.meta.Data fun;
  
  public final java.util.List<hydra.ext.scala.meta.Pat> args;
  
  public Pat_Extract (hydra.ext.scala.meta.Data fun, java.util.List<hydra.ext.scala.meta.Pat> args) {
    java.util.Objects.requireNonNull((fun));
    java.util.Objects.requireNonNull((args));
    this.fun = fun;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Pat_Extract)) {
      return false;
    }
    Pat_Extract o = (Pat_Extract) (other);
    return fun.equals(o.fun) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * fun.hashCode() + 3 * args.hashCode();
  }
  
  public Pat_Extract withFun(hydra.ext.scala.meta.Data fun) {
    java.util.Objects.requireNonNull((fun));
    return new Pat_Extract(fun, args);
  }
  
  public Pat_Extract withArgs(java.util.List<hydra.ext.scala.meta.Pat> args) {
    java.util.Objects.requireNonNull((args));
    return new Pat_Extract(fun, args);
  }
}