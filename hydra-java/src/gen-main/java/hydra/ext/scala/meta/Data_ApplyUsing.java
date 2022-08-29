package hydra.ext.scala.meta;

public class Data_ApplyUsing {
  public final hydra.ext.scala.meta.Data fun;
  
  public final java.util.List<hydra.ext.scala.meta.Data> targs;
  
  public Data_ApplyUsing (hydra.ext.scala.meta.Data fun, java.util.List<hydra.ext.scala.meta.Data> targs) {
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
  
  public Data_ApplyUsing withFun(hydra.ext.scala.meta.Data fun) {
    return new Data_ApplyUsing(fun, targs);
  }
  
  public Data_ApplyUsing withTargs(java.util.List<hydra.ext.scala.meta.Data> targs) {
    return new Data_ApplyUsing(fun, targs);
  }
}