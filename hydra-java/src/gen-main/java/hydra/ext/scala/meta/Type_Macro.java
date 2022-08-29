package hydra.ext.scala.meta;

public class Type_Macro {
  public final hydra.ext.scala.meta.Data body;
  
  public Type_Macro (hydra.ext.scala.meta.Data body) {
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Macro)) {
      return false;
    }
    Type_Macro o = (Type_Macro) (other);
    return body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * body.hashCode();
  }
}