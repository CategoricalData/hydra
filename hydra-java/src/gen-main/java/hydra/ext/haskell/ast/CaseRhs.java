package hydra.ext.haskell.ast;

/**
 * The right-hand side of a pattern-matching alternative
 */
public class CaseRhs {
  /**
   * The right-hand side of a pattern-matching alternative
   */
  public final hydra.ext.haskell.ast.Expression value;
  
  public CaseRhs (hydra.ext.haskell.ast.Expression value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CaseRhs)) {
      return false;
    }
    CaseRhs o = (CaseRhs) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}