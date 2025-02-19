// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptBlock implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ExceptBlock");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.util.Opt<hydra.ext.python.syntax.ExceptExpression> expression;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ExceptBlock (hydra.util.Opt<hydra.ext.python.syntax.ExceptExpression> expression, hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((body));
    this.expression = expression;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptBlock)) {
      return false;
    }
    ExceptBlock o = (ExceptBlock) (other);
    return expression.equals(o.expression) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * body.hashCode();
  }
  
  public ExceptBlock withExpression(hydra.util.Opt<hydra.ext.python.syntax.ExceptExpression> expression) {
    java.util.Objects.requireNonNull((expression));
    return new ExceptBlock(expression, body);
  }
  
  public ExceptBlock withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new ExceptBlock(expression, body);
  }
}