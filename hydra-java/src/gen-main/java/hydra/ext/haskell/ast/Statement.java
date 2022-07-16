package hydra.ext.haskell.ast;

public class Statement {
  public final Expression value;
  
  public Statement (Expression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Statement)) {
      return false;
    }
    Statement o = (Statement) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}