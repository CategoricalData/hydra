// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A named function definition. Serializes as (defn name [params] body) in Clojure, (defun name (params) body) in Emacs Lisp and Common Lisp, (define (name params) body) in Scheme
 */
public class FunctionDefinition implements Serializable, Comparable<FunctionDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.FunctionDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name REST_PARAM = new hydra.core.Name("restParam");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  public static final hydra.core.Name TYPE_HINTS = new hydra.core.Name("typeHints");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The function name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The parameter list
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params;

  /**
   * Optional rest/variadic parameter
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam;

  /**
   * Optional docstring
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc;

  /**
   * Optional type hints for parameters and return type
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.TypeHint> typeHints;

  /**
   * The function body (one or more expressions)
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body;

  public FunctionDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params, hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam, hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc, hydra.util.ConsList<hydra.ext.lisp.syntax.TypeHint> typeHints, hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    this.name = name;
    this.params = params;
    this.restParam = restParam;
    this.doc = doc;
    this.typeHints = typeHints;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDefinition)) {
      return false;
    }
    FunctionDefinition o = (FunctionDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.restParam,
      o.restParam) && java.util.Objects.equals(
      this.doc,
      o.doc) && java.util.Objects.equals(
      this.typeHints,
      o.typeHints) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(params) + 5 * java.util.Objects.hashCode(restParam) + 7 * java.util.Objects.hashCode(doc) + 11 * java.util.Objects.hashCode(typeHints) + 13 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionDefinition other) {
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
    cmp = ((Comparable) doc).compareTo(other.doc);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) typeHints).compareTo(other.typeHints);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public FunctionDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }

  public FunctionDefinition withParams(hydra.util.ConsList<hydra.ext.lisp.syntax.Symbol> params) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }

  public FunctionDefinition withRestParam(hydra.util.Maybe<hydra.ext.lisp.syntax.Symbol> restParam) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }

  public FunctionDefinition withDoc(hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }

  public FunctionDefinition withTypeHints(hydra.util.ConsList<hydra.ext.lisp.syntax.TypeHint> typeHints) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }

  public FunctionDefinition withBody(hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    return new FunctionDefinition(name, params, restParam, doc, typeHints, body);
  }
}
