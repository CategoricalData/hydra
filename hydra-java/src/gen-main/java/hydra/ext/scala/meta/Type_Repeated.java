package hydra.ext.scala.meta;

public class Type_Repeated {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.Repeated");
  
  public final hydra.ext.scala.meta.Type tpe;
  
  public Type_Repeated (hydra.ext.scala.meta.Type tpe) {
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Repeated)) {
      return false;
    }
    Type_Repeated o = (Type_Repeated) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}