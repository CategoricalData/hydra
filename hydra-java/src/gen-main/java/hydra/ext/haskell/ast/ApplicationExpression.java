// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * An application expression
 */
public class ApplicationExpression implements Serializable, Comparable<ApplicationExpression> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.ApplicationExpression");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENT = new hydra.core.Name("argument");
  
  /**
   * The function being applied
   */
  public final hydra.ext.haskell.ast.Expression function;
  
  /**
   * The argument
   */
  public final hydra.ext.haskell.ast.Expression argument;
  
  public ApplicationExpression (hydra.ext.haskell.ast.Expression function, hydra.ext.haskell.ast.Expression argument) {
    this.function = function;
    this.argument = argument;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ApplicationExpression)) {
      return false;
    }
    ApplicationExpression o = (ApplicationExpression) (other);
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
    cmp = ((Comparable) (function)).compareTo(other.function);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) (argument)).compareTo(other.argument);
  }
  
  public ApplicationExpression withFunction(hydra.ext.haskell.ast.Expression function) {
    return new ApplicationExpression(function, argument);
  }
  
  public ApplicationExpression withArgument(hydra.ext.haskell.ast.Expression argument) {
    return new ApplicationExpression(function, argument);
  }
}
