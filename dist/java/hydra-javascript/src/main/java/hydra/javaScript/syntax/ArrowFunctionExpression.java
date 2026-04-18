// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An arrow function expression
 */
public class ArrowFunctionExpression implements Serializable, Comparable<ArrowFunctionExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.ArrowFunctionExpression");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public static final hydra.core.Name ASYNC = new hydra.core.Name("async");

  /**
   * Function parameters
   */
  public final java.util.List<hydra.javaScript.syntax.Pattern> params;

  /**
   * Function body (expression or block)
   */
  public final hydra.javaScript.syntax.ArrowFunctionBody body;

  /**
   * Whether the function is async
   */
  public final Boolean async;

  public ArrowFunctionExpression (java.util.List<hydra.javaScript.syntax.Pattern> params, hydra.javaScript.syntax.ArrowFunctionBody body, Boolean async) {
    this.params = params;
    this.body = body;
    this.async = async;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ArrowFunctionExpression)) {
      return false;
    }
    ArrowFunctionExpression o = (ArrowFunctionExpression) other;
    return java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.body,
      o.body) && java.util.Objects.equals(
      this.async,
      o.async);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(params) + 3 * java.util.Objects.hashCode(body) + 5 * java.util.Objects.hashCode(async);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(ArrowFunctionExpression other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      params,
      other.params);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      body,
      other.body);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      async,
      other.async);
  }

  public ArrowFunctionExpression withParams(java.util.List<hydra.javaScript.syntax.Pattern> params) {
    return new ArrowFunctionExpression(params, body, async);
  }

  public ArrowFunctionExpression withBody(hydra.javaScript.syntax.ArrowFunctionBody body) {
    return new ArrowFunctionExpression(params, body, async);
  }

  public ArrowFunctionExpression withAsync(Boolean async) {
    return new ArrowFunctionExpression(params, body, async);
  }
}
