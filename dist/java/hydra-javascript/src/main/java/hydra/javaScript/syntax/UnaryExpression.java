// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A unary operation expression
 */
public class UnaryExpression implements Serializable, Comparable<UnaryExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.UnaryExpression");

  public static final hydra.core.Name OPERATOR = new hydra.core.Name("operator");

  public static final hydra.core.Name ARGUMENT = new hydra.core.Name("argument");

  public static final hydra.core.Name PREFIX = new hydra.core.Name("prefix");

  public final hydra.javaScript.syntax.UnaryOperator operator;

  public final hydra.javaScript.syntax.Expression argument;

  /**
   * Whether the operator is prefix (true) or postfix (false)
   */
  public final Boolean prefix;

  public UnaryExpression (hydra.javaScript.syntax.UnaryOperator operator, hydra.javaScript.syntax.Expression argument, Boolean prefix) {
    this.operator = operator;
    this.argument = argument;
    this.prefix = prefix;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnaryExpression)) {
      return false;
    }
    UnaryExpression o = (UnaryExpression) other;
    return java.util.Objects.equals(
      this.operator,
      o.operator) && java.util.Objects.equals(
      this.argument,
      o.argument) && java.util.Objects.equals(
      this.prefix,
      o.prefix);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(operator) + 3 * java.util.Objects.hashCode(argument) + 5 * java.util.Objects.hashCode(prefix);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(UnaryExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      operator,
      other.operator);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      argument,
      other.argument);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      prefix,
      other.prefix);
  }

  public UnaryExpression withOperator(hydra.javaScript.syntax.UnaryOperator operator) {
    return new UnaryExpression(operator, argument, prefix);
  }

  public UnaryExpression withArgument(hydra.javaScript.syntax.Expression argument) {
    return new UnaryExpression(operator, argument, prefix);
  }

  public UnaryExpression withPrefix(Boolean prefix) {
    return new UnaryExpression(operator, argument, prefix);
  }
}
