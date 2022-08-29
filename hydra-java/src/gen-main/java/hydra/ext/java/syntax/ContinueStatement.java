package hydra.ext.java.syntax;

public class ContinueStatement {
  public final java.util.Optional<hydra.ext.java.syntax.Identifier> value;
  
  public ContinueStatement (java.util.Optional<hydra.ext.java.syntax.Identifier> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ContinueStatement)) {
      return false;
    }
    ContinueStatement o = (ContinueStatement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}