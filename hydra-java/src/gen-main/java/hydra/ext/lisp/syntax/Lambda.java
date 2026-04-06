// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * An anonymous function. Serializes as (fn [params] body) in Clojure, (lambda (params) body) in Emacs Lisp, Common Lisp, and Scheme. If name is provided, emits (fn name [params] body) in Clojure for self-reference.
 */
public class Lambda implements Serializable, Comparable<Lambda> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.Lambda");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name REST_PARAM = new hydra.core.Name("restParam");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * Optional name for self-referential lambdas (Clojure named fn)
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> name;

  /**
   * The parameter list
   */
  public final java.util.List<hydra.ext.lisp.syntax.Symbol> params;

  /**
   * Optional rest parameter
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam;

  /**
   * The lambda body
   */
  public final java.util.List<hydra.ext.lisp.syntax.Expression> body;

  public Lambda (hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> name, java.util.List<hydra.ext.lisp.syntax.Symbol> params, hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam, java.util.List<hydra.ext.lisp.syntax.Expression> body) {
    this.name = name;
    this.params = params;
    this.restParam = restParam;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Lambda)) {
      return false;
    }
    Lambda o = (Lambda) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.restParam,
      o.restParam) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(params) + 5 * java.util.Objects.hashCode(restParam) + 7 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Lambda other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      params,
      other.params);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      restParam,
      other.restParam);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public Lambda withName(hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> name) {
    return new Lambda(name, params, restParam, body);
  }

  public Lambda withParams(java.util.List<hydra.ext.lisp.syntax.Symbol> params) {
    return new Lambda(name, params, restParam, body);
  }

  public Lambda withRestParam(hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam) {
    return new Lambda(name, params, restParam, body);
  }

  public Lambda withBody(java.util.List<hydra.ext.lisp.syntax.Expression> body) {
    return new Lambda(name, params, restParam, body);
  }
}
