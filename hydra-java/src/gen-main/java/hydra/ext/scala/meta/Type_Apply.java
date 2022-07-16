package hydra.ext.scala.meta;

public class Type_Apply {
  public final Type tpe;
  
  public final java.util.List<Type> args;
  
  public Type_Apply (Type tpe, java.util.List<Type> args) {
    this.tpe = tpe;
    this.args = args;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Apply)) {
      return false;
    }
    Type_Apply o = (Type_Apply) (other);
    return tpe.equals(o.tpe) && args.equals(o.args);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode() + 3 * args.hashCode();
  }
  
  public Type_Apply withTpe(Type tpe) {
    return new Type_Apply(tpe, args);
  }
  
  public Type_Apply withArgs(java.util.List<Type> args) {
    return new Type_Apply(tpe, args);
  }
}