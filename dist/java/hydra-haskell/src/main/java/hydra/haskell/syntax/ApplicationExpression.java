// Note: this is an automatically generated file. Do not edit.

package hydra.haskell.syntax;

import java.io.Serializable;

/**
 * An application expression
 */
public class ApplicationExpression implements Serializable, Comparable<ApplicationExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.haskell.syntax.ApplicationExpression");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name ARGUMENT = new hydra.core.Name("argument");

  /**
   * The function being applied
   */
  public final hydra.haskell.syntax.Expression function;

  /**
   * The argument
   */
  public final hydra.haskell.syntax.Expression argument;

  public ApplicationExpression (hydra.haskell.syntax.Expression function, hydra.haskell.syntax.Expression argument) {
    this.function = function;
    this.argument = argument;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationExpression)) {
      return false;
    }
    ApplicationExpression o = (ApplicationExpression) other;
    return java.util.Objects.equals(
      this.function,
      o.function) && java.util.Objects.equals(
      this.argument,
      o.argument);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(function) + 3 * java.util.Objects.hashCode(argument);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ApplicationExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      function,
      other.function);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      argument,
      other.argument);
  }

  public ApplicationExpression withFunction(hydra.haskell.syntax.Expression function) {
    return new ApplicationExpression(function, argument);
  }

  public ApplicationExpression withArgument(hydra.haskell.syntax.Expression argument) {
    return new ApplicationExpression(function, argument);
  }
}
