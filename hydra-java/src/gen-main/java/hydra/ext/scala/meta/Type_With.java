package hydra.ext.scala.meta;

public class Type_With {
  public final Type lhs;
  
  public final Type rhs;
  
  public Type_With (Type lhs, Type rhs) {
    this.lhs = lhs;
    this.rhs = rhs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Type_With)) {
      return false;
    }
    Type_With o = (Type_With) (other);
    return lhs.equals(o.lhs) && rhs.equals(o.rhs);
  }
  
  @Override
  public int hashCode() {
    return 2 * lhs.hashCode() + 3 * rhs.hashCode();
  }
  
  public Type_With withLhs(Type lhs) {
    return new Type_With(lhs, rhs);
  }
  
  public Type_With withRhs(Type rhs) {
    return new Type_With(lhs, rhs);
  }
}