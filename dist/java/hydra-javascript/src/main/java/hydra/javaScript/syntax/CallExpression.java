// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A function call expression
 */
public class CallExpression implements Serializable, Comparable<CallExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.CallExpression");

  public static final hydra.core.Name CALLEE = new hydra.core.Name("callee");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");

  /**
   * The function being called
   */
  public final hydra.javaScript.syntax.Expression callee;

  /**
   * The arguments
   */
  public final java.util.List<hydra.javaScript.syntax.Expression> arguments;

  /**
   * Whether using optional chaining (?.)
   */
  public final Boolean optional;

  public CallExpression (hydra.javaScript.syntax.Expression callee, java.util.List<hydra.javaScript.syntax.Expression> arguments, Boolean optional) {
    this.callee = callee;
    this.arguments = arguments;
    this.optional = optional;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CallExpression)) {
      return false;
    }
    CallExpression o = (CallExpression) other;
    return java.util.Objects.equals(
      this.callee,
      o.callee) && java.util.Objects.equals(
      this.arguments,
      o.arguments) && java.util.Objects.equals(
      this.optional,
      o.optional);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(callee) + 3 * java.util.Objects.hashCode(arguments) + 5 * java.util.Objects.hashCode(optional);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CallExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      callee,
      other.callee);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      arguments,
      other.arguments);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      optional,
      other.optional);
  }

  public CallExpression withCallee(hydra.javaScript.syntax.Expression callee) {
    return new CallExpression(callee, arguments, optional);
  }

  public CallExpression withArguments(java.util.List<hydra.javaScript.syntax.Expression> arguments) {
    return new CallExpression(callee, arguments, optional);
  }

  public CallExpression withOptional(Boolean optional) {
    return new CallExpression(callee, arguments, optional);
  }
}
