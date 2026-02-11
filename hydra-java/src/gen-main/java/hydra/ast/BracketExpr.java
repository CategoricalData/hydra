// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An expression enclosed by brackets
 */
public class BracketExpr implements Serializable, Comparable<BracketExpr> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.BracketExpr");
  
  public static final hydra.core.Name FIELD_NAME_BRACKETS = new hydra.core.Name("brackets");
  
  public static final hydra.core.Name FIELD_NAME_ENCLOSED = new hydra.core.Name("enclosed");
  
  public static final hydra.core.Name FIELD_NAME_STYLE = new hydra.core.Name("style");
  
  /**
   * The bracket pair enclosing the expression
   */
  public final hydra.ast.Brackets brackets;
  
  /**
   * The expression within the brackets
   */
  public final hydra.ast.Expr enclosed;
  
  /**
   * The formatting style for the bracketed block
   */
  public final hydra.ast.BlockStyle style;
  
  public BracketExpr (hydra.ast.Brackets brackets, hydra.ast.Expr enclosed, hydra.ast.BlockStyle style) {
    this.brackets = brackets;
    this.enclosed = enclosed;
    this.style = style;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof BracketExpr)) {
      return false;
    }
    BracketExpr o = (BracketExpr) other;
    return java.util.Objects.equals(
      this.brackets,
      o.brackets) && java.util.Objects.equals(
      this.enclosed,
      o.enclosed) && java.util.Objects.equals(
      this.style,
      o.style);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(brackets) + 3 * java.util.Objects.hashCode(enclosed) + 5 * java.util.Objects.hashCode(style);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(BracketExpr other) {
    int cmp = 0;
    cmp = ((Comparable) brackets).compareTo(other.brackets);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) enclosed).compareTo(other.enclosed);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) style).compareTo(other.style);
  }
  
  public BracketExpr withBrackets(hydra.ast.Brackets brackets) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withEnclosed(hydra.ast.Expr enclosed) {
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withStyle(hydra.ast.BlockStyle style) {
    return new BracketExpr(brackets, enclosed, style);
  }
}
