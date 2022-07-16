package hydra.ext.java.syntax;

public class BreakStatement {
  public final java.util.Optional<Identifier> value;
  
  public BreakStatement (java.util.Optional<Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BreakStatement)) {
      return false;
    }
    BreakStatement o = (BreakStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}