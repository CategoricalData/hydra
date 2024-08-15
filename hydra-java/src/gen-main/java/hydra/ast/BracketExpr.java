// Note: this is an automatically generated file. Do not edit.

package hydra.ast;

import java.io.Serializable;

/**
 * An expression enclosed by brackets
 */
public class BracketExpr implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ast.BracketExpr");
  
  public static final hydra.core.Name FIELD_NAME_BRACKETS = new hydra.core.Name("brackets");
  
  public static final hydra.core.Name FIELD_NAME_ENCLOSED = new hydra.core.Name("enclosed");
  
  public static final hydra.core.Name FIELD_NAME_STYLE = new hydra.core.Name("style");
  
  public final hydra.ast.Brackets brackets;
  
  public final hydra.ast.Expr enclosed;
  
  public final hydra.ast.BlockStyle style;
  
  public BracketExpr (hydra.ast.Brackets brackets, hydra.ast.Expr enclosed, hydra.ast.BlockStyle style) {
    java.util.Objects.requireNonNull((brackets));
    java.util.Objects.requireNonNull((enclosed));
    java.util.Objects.requireNonNull((style));
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
  
  public BracketExpr withBrackets(hydra.ast.Brackets brackets) {
    java.util.Objects.requireNonNull((brackets));
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withEnclosed(hydra.ast.Expr enclosed) {
    java.util.Objects.requireNonNull((enclosed));
    return new BracketExpr(brackets, enclosed, style);
  }
  
  public BracketExpr withStyle(hydra.ast.BlockStyle style) {
    java.util.Objects.requireNonNull((style));
    return new BracketExpr(brackets, enclosed, style);
  }
}