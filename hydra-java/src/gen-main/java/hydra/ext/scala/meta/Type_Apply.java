package hydra.ext.scala.meta;

public class Type_Apply {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Apply");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public final java.util.List<hydra.ext.scala.meta.Type> args;
  
  public Type_Apply (hydra.ext.scala.meta.Type tpe, java.util.List<hydra.ext.scala.meta.Type> args) {
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
  
  public Type_Apply withTpe(hydra.ext.scala.meta.Type tpe) {
    return new Type_Apply(tpe, args);
  }
  
  public Type_Apply withArgs(java.util.List<hydra.ext.scala.meta.Type> args) {
    return new Type_Apply(tpe, args);
  }
}