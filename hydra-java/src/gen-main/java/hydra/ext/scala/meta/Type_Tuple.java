package hydra.ext.scala.meta;

public class Type_Tuple {
  public final java.util.List<Type> args;
  
  public Type_Tuple (java.util.List<Type> args) {
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Tuple)) {
      return false;
    }
    Type_Tuple o = (Type_Tuple) (other);
    return args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * args.hashCode();
  }
}