package hydra.langs.haskell.ast;

/**
 * The right-hand side of a pattern-matching alternative
 */
public class CaseRhs {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/haskell/ast.CaseRhs");
  
  /**
   * The right-hand side of a pattern-matching alternative
   */
  public final hydra.langs.haskell.ast.Expression value;
  
  public CaseRhs (hydra.langs.haskell.ast.Expression value) {
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