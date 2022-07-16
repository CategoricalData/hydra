package hydra.ext.scala.meta;

public class Pat_Extract {
  public final Data fun;
  
  public final java.util.List<Pat> args;
  
  public Pat_Extract (Data fun, java.util.List<Pat> args) {
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
  
  public Pat_Extract withFun(Data fun) {
    return new Pat_Extract(fun, args);
  }
  
  public Pat_Extract withArgs(java.util.List<Pat> args) {
    return new Pat_Extract(fun, args);
  }
}