package hydra.ext.scala.meta;

public class Data_Apply {
  public final Data fun;
  
  public final java.util.List<Data> args;
  
  public Data_Apply (Data fun, java.util.List<Data> args) {
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
  
  public Data_Apply withFun(Data fun) {
    return new Data_Apply(fun, args);
  }
  
  public Data_Apply withArgs(java.util.List<Data> args) {
    return new Data_Apply(fun, args);
  }
}