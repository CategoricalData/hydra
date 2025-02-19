// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class ExceptStarBlock implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.ExceptStarBlock");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.python.syntax.Expression expression;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Name> as;
  
  public final hydra.ext.python.syntax.Block body;
  
  public ExceptStarBlock (hydra.ext.python.syntax.Expression expression, hydra.util.Opt<hydra.ext.python.syntax.Name> as, hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((as));
    java.util.Objects.requireNonNull((body));
    this.expression = expression;
    this.as = as;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ExceptStarBlock)) {
      return false;
    }
    ExceptStarBlock o = (ExceptStarBlock) (other);
    return expression.equals(o.expression) && as.equals(o.as) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * as.hashCode() + 5 * body.hashCode();
  }
  
  public ExceptStarBlock withExpression(hydra.ext.python.syntax.Expression expression) {
    java.util.Objects.requireNonNull((expression));
    return new ExceptStarBlock(expression, as, body);
  }
  
  public ExceptStarBlock withAs(hydra.util.Opt<hydra.ext.python.syntax.Name> as) {
    java.util.Objects.requireNonNull((as));
    return new ExceptStarBlock(expression, as, body);
  }
  
  public ExceptStarBlock withBody(hydra.ext.python.syntax.Block body) {
    java.util.Objects.requireNonNull((body));
    return new ExceptStarBlock(expression, as, body);
  }
}