// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An expression indented in a certain style
 */
public class IndentedExpression implements Serializable, Comparable<IndentedExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ast.IndentedExpression");
  
  public static final hydra.core.Name FIELD_NAME_STYLE = new hydra.core.Name("style");
  
  public static final hydra.core.Name FIELD_NAME_EXPR = new hydra.core.Name("expr");
  
  /**
   * The indentation style
   */
  public final hydra.ast.IndentStyle style;
  
  /**
   * The expression to be indented
   */
  public final hydra.ast.Expr expr;
  
  public IndentedExpression (hydra.ast.IndentStyle style, hydra.ast.Expr expr) {
    this.style = style;
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IndentedExpression)) {
      return false;
    }
    IndentedExpression o = (IndentedExpression) (other);
    return java.util.Objects.equals(
      this.style,
      o.style) && java.util.Objects.equals(
      this.expr,
      o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(style) + 3 * java.util.Objects.hashCode(expr);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(IndentedExpression other) {
    int cmp = 0;
    cmp = ((Comparable) (style)).compareTo(other.style);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (expr)).compareTo(other.expr);
  }
  
  public IndentedExpression withStyle(hydra.ast.IndentStyle style) {
    return new IndentedExpression(style, expr);
  }
  
  public IndentedExpression withExpr(hydra.ast.Expr expr) {
    return new IndentedExpression(style, expr);
  }
}
