// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class IsPatternExpression implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.IsPatternExpression");
  
  public static final hydra.core.Name FIELD_NAME_EXPRESSION = new hydra.core.Name("expression");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN = new hydra.core.Name("pattern");
  
  public final hydra.ext.csharp.syntax.RelationalExpression expression;
  
  public final hydra.ext.csharp.syntax.Pattern pattern;
  
  public IsPatternExpression (hydra.ext.csharp.syntax.RelationalExpression expression, hydra.ext.csharp.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((expression));
    java.util.Objects.requireNonNull((pattern));
    this.expression = expression;
    this.pattern = pattern;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof IsPatternExpression)) {
      return false;
    }
    IsPatternExpression o = (IsPatternExpression) (other);
    return expression.equals(o.expression) && pattern.equals(o.pattern);
  }
  
  @Override
  public int hashCode() {
    return 2 * expression.hashCode() + 3 * pattern.hashCode();
  }
  
  public IsPatternExpression withExpression(hydra.ext.csharp.syntax.RelationalExpression expression) {
    java.util.Objects.requireNonNull((expression));
    return new IsPatternExpression(expression, pattern);
  }
  
  public IsPatternExpression withPattern(hydra.ext.csharp.syntax.Pattern pattern) {
    java.util.Objects.requireNonNull((pattern));
    return new IsPatternExpression(expression, pattern);
  }
}