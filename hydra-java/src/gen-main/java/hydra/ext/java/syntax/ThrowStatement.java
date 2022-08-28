package hydra.ext.java.syntax;

public class ThrowStatement {
  public final hydra.ext.java.syntax.Expression value;
  
  public ThrowStatement (hydra.ext.java.syntax.Expression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ThrowStatement)) {
      return false;
    }
    ThrowStatement o = (ThrowStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}