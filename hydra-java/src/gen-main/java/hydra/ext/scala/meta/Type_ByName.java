package hydra.ext.scala.meta;

public class Type_ByName {
  public final Type tpe;
  
  public Type_ByName (Type tpe) {
    this.tpe = tpe;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_ByName)) {
      return false;
    }
    Type_ByName o = (Type_ByName) (other);
    return tpe.equals(o.tpe);
  }
  
  @Override
  public int hashCode() {
    return 2 * tpe.hashCode();
  }
}