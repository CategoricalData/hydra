package hydra.util.codetree.ast;

/**
 * An expression enclosed by brackets
 */
public class BracketExpr {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/util/codetree/ast.BracketExpr");
  
  public final hydra.util.codetree.ast.Brackets brackets;
  
  public final hydra.util.codetree.ast.Expr enclosed;
  
  public final hydra.util.codetree.ast.BlockStyle style;
  
  public BracketExpr (hydra.util.codetree.ast.Brackets brackets, hydra.util.codetree.ast.Expr enclosed, hydra.util.codetree.ast.BlockStyle style) {
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
  
  public BracketExpr withBrackets(hydra.util.codetree.ast.Brackets brackets) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withEnclosed(hydra.util.codetree.ast.Expr enclosed) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withStyle(hydra.util.codetree.ast.BlockStyle style) {
    return new BracketExpr(brackets, enclosed, style);
  }
}