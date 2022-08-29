package hydra.ext.scala.meta;

public class Type_Or {
  public final hydra.ext.scala.meta.Type lhs;
  
  public final hydra.ext.scala.meta.Type rhs;
  
  public Type_Or (hydra.ext.scala.meta.Type lhs, hydra.ext.scala.meta.Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_Or)) {
      return false;
    }
    Type_Or o = (Type_Or) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Type_Or withLhs(hydra.ext.scala.meta.Type lhs) {
    return new Type_Or(lhs, rhs);
  }
  
  public Type_Or withRhs(hydra.ext.scala.meta.Type rhs) {
    return new Type_Or(lhs, rhs);
  }
}