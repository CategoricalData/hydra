package hydra.ext.scala.meta;

public class Type_Repeated {
  public final Type tpe;
  
  public Type_Repeated (Type tpe) {
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