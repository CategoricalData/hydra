package hydra.util.codetree.ast;

/**
 * An expression enclosed by brackets
 */
public class BracketExpr {
  public final Brackets brackets;
  
  public final Expr enclosed;
  
  public final BlockStyle style;
  
  public BracketExpr (Brackets brackets, Expr enclosed, BlockStyle style) {
    this.brackets = brackets;
    this.enclosed = enclosed;
    this.style = style;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BracketExpr)) {
      return false;
    }
    BracketExpr o = (BracketExpr) (other);
    return brackets.equals(o.brackets) && enclosed.equals(o.enclosed) && style.equals(o.style);
  }
  
  @Override
  public int hashCode() {
    return 2 * brackets.hashCode() + 3 * enclosed.hashCode() + 5 * style.hashCode();
  }
  
  public BracketExpr withBrackets(Brackets brackets) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withEnclosed(Expr enclosed) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withStyle(BlockStyle style) {
    return new BracketExpr(brackets, enclosed, style);
  }
}