// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An expression indented in a certain style
 */
public class IndentedExpression implements Serializable {
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
    java.util.Objects.requireNonNull((style));
    java.util.Objects.requireNonNull((expr));
    this.style = style;
    this.expr = expr;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IndentedExpression)) {
      return false;
    }
    IndentedExpression o = (IndentedExpression) (other);
    return style.equals(o.style) && expr.equals(o.expr);
  }
  
  @Override
  public int hashCode() {
    return 2 * style.hashCode() + 3 * expr.hashCode();
  }
  
  public IndentedExpression withStyle(hydra.ast.IndentStyle style) {
    java.util.Objects.requireNonNull((style));
    return new IndentedExpression(style, expr);
  }
  
  public IndentedExpression withExpr(hydra.ast.Expr expr) {
    java.util.Objects.requireNonNull((expr));
    return new IndentedExpression(style, expr);
  }
}
