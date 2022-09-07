package hydra.ext.haskell.ast;

public class Statement {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/haskell/ast.Statement");
  
  public final hydra.ext.haskell.ast.Expression value;
  
  public Statement (hydra.ext.haskell.ast.Expression value) {
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