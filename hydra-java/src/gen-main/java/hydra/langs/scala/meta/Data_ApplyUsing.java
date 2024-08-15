// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public class Data_ApplyUsing implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Data.ApplyUsing");
  
  public static final hydra.core.Name FIELD_NAME_FUN = new hydra.core.Name("fun");
  
  public static final hydra.core.Name FIELD_NAME_TARGS = new hydra.core.Name("targs");
  
  public final hydra.langs.scala.meta.Data fun;
  
  public final java.util.List<hydra.langs.scala.meta.Data> targs;
  
  public Data_ApplyUsing (hydra.langs.scala.meta.Data fun, java.util.List<hydra.langs.scala.meta.Data> targs) {
    java.util.Objects.requireNonNull((fun));
    java.util.Objects.requireNonNull((targs));
    this.fun = fun;
    this.targs = targs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Data_ApplyUsing)) {
      return false;
    }
    Data_ApplyUsing o = (Data_ApplyUsing) (other);
    return fun.equals(o.fun) && targs.equals(o.targs);
  }
  
  @Override
  public int hashCode() {
    return 2 * fun.hashCode() + 3 * targs.hashCode();
  }
  
  public Data_ApplyUsing withFun(hydra.langs.scala.meta.Data fun) {
    java.util.Objects.requireNonNull((fun));
    return new Data_ApplyUsing(fun, targs);
  }
  
  public Data_ApplyUsing withTargs(java.util.List<hydra.langs.scala.meta.Data> targs) {
    java.util.Objects.requireNonNull((targs));
    return new Data_ApplyUsing(fun, targs);
  }
}