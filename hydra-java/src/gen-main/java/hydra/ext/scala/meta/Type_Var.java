package hydra.ext.scala.meta;

public class Type_Var {
  public final hydra.ext.scala.meta.Type_Name name;
  
  public Type_Var (hydra.ext.scala.meta.Type_Name name) {
    this.name = name;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Var)) {
      return false;
    }
    Type_Var o = (Type_Var) (other);
    return name.equals(o.name);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode();
  }
}