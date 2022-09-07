package hydra.ext.scala.meta;

public class Type_With {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/scala/meta.Type.With");
  
  public final hydra.ext.scala.meta.Type lhs;
  
  public final hydra.ext.scala.meta.Type rhs;
  
  public Type_With (hydra.ext.scala.meta.Type lhs, hydra.ext.scala.meta.Type rhs) {
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
  
  public Type_With withLhs(hydra.ext.scala.meta.Type lhs) {
    return new Type_With(lhs, rhs);
  }
  
  public Type_With withRhs(hydra.ext.scala.meta.Type rhs) {
    return new Type_With(lhs, rhs);
  }
}