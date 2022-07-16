package hydra.ext.scala.meta;

public class Type_And {
  public final Type lhs;
  
  public final Type rhs;
  
  public Type_And (Type lhs, Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_And)) {
      return false;
    }
    Type_And o = (Type_And) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Type_And withLhs(Type lhs) {
    return new Type_And(lhs, rhs);
  }
  
  public Type_And withRhs(Type rhs) {
    return new Type_And(lhs, rhs);
  }
}