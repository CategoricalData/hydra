// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public class Data_Apply implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Apply");
  
  public static final hydra.core.Name FIELD_NAME_FUN = new hydra.core.Name("fun");
  
  public static final hydra.core.Name FIELD_NAME_ARGS = new hydra.core.Name("args");
  
  public final hydra.ext.scala.meta.Data fun;
  
  public final java.util.List<hydra.ext.scala.meta.Data> args;
  
  public Data_Apply (hydra.ext.scala.meta.Data fun, java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((fun));
    java.util.Objects.requireNonNull((args));
    this.fun = fun;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_Apply)) {
      return false;
    }
    Data_Apply o = (Data_Apply) (other);
    return fun.equals(o.fun) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * fun.hashCode() + 3 * args.hashCode();
  }
  
  public Data_Apply withFun(hydra.ext.scala.meta.Data fun) {
    java.util.Objects.requireNonNull((fun));
    return new Data_Apply(fun, args);
  }
  
  public Data_Apply withArgs(java.util.List<hydra.ext.scala.meta.Data> args) {
    java.util.Objects.requireNonNull((args));
    return new Data_Apply(fun, args);
  }
}