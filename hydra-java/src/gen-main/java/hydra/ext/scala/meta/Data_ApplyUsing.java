package hydra.ext.scala.meta;

public class Data_ApplyUsing {
  public final Data fun;
  
  public final java.util.List<Data> targs;
  
  public Data_ApplyUsing (Data fun, java.util.List<Data> targs) {
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
  
  public Data_ApplyUsing withFun(Data fun) {
    return new Data_ApplyUsing(fun, targs);
  }
  
  public Data_ApplyUsing withTargs(java.util.List<Data> targs) {
    return new Data_ApplyUsing(fun, targs);
  }
}