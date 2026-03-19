// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A macro definition. Serializes as (defmacro name [params] body) in Clojure, (defmacro name (params) body) in Emacs Lisp and Common Lisp, (define-syntax name ...) in Scheme
 */
public class MacroDefinition implements Serializable, Comparable<MacroDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.MacroDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name REST_PARAM = new hydra.core.Name("restParam");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The macro name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The parameter list
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params;

  /**
   * Optional rest parameter
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam;

  /**
   * The macro body
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body;

  public MacroDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params, hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam, hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    this.name = name;
    this.params = params;
    this.restParam = restParam;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof MacroDefinition)) {
      return false;
    }
    MacroDefinition o = (MacroDefinition) other;
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
  public int compareTo(MacroDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) params).compareTo(other.params);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) restParam).compareTo(other.restParam);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public MacroDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new MacroDefinition(name, params, restParam, body);
  }

  public MacroDefinition withParams(hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params) {
    return new MacroDefinition(name, params, restParam, body);
  }

  public MacroDefinition withRestParam(hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam) {
    return new MacroDefinition(name, params, restParam, body);
  }

  public MacroDefinition withBody(hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    return new MacroDefinition(name, params, restParam, body);
  }
}
