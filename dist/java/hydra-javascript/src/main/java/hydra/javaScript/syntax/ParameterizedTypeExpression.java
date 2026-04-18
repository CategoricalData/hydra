// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A parameterized type (e.g., Array&lt;T&gt;, Map&lt;K, V&gt;)
 */
public class ParameterizedTypeExpression implements Serializable, Comparable<ParameterizedTypeExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ParameterizedTypeExpression");

  public static final hydra.core.Name BASE = new hydra.core.Name("base");

  public static final hydra.core.Name ARGUMENTS = new hydra.core.Name("arguments");

  public final hydra.javaScript.syntax.TypeExpression base;

  public final java.util.List<hydra.javaScript.syntax.TypeExpression> arguments;

  public ParameterizedTypeExpression (hydra.javaScript.syntax.TypeExpression base, java.util.List<hydra.javaScript.syntax.TypeExpression> arguments) {
    this.base = base;
    this.arguments = arguments;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ParameterizedTypeExpression)) {
      return false;
    }
    ParameterizedTypeExpression o = (ParameterizedTypeExpression) other;
    return java.util.Objects.equals(
      this.base,
      o.base) && java.util.Objects.equals(
      this.arguments,
      o.arguments);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(base) + 3 * java.util.Objects.hashCode(arguments);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ParameterizedTypeExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      base,
      other.base);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      arguments,
      other.arguments);
  }

  public ParameterizedTypeExpression withBase(hydra.javaScript.syntax.TypeExpression base) {
    return new ParameterizedTypeExpression(base, arguments);
  }

  public ParameterizedTypeExpression withArguments(java.util.List<hydra.javaScript.syntax.TypeExpression> arguments) {
    return new ParameterizedTypeExpression(base, arguments);
  }
}
