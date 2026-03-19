// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * Local variable bindings. Serializes as (let [x 1 y 2] body) in Clojure (always sequential), (let ((x 1) (y 2)) body) or (let* ...) in other dialects
 */
public class LetExpression implements Serializable, Comparable<LetExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.LetExpression");

  public static final hydra.core.Name KIND = new hydra.core.Name("kind");

  public static final hydra.core.Name BINDINGS = new hydra.core.Name("bindings");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The kind of let (parallel or sequential)
   */
  public final hydra.ext.lisp.syntax.LetKind kind;

  /**
   * The variable bindings
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.LetBinding> bindings;

  /**
   * The body expressions
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body;

  public LetExpression (hydra.ext.lisp.syntax.LetKind kind, hydra.util.ConsList<hydra.ext.lisp.syntax.LetBinding> bindings, hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    this.kind = kind;
    this.bindings = bindings;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof LetExpression)) {
      return false;
    }
    LetExpression o = (LetExpression) other;
    return java.util.Objects.equals(
      this.kind,
      o.kind) && java.util.Objects.equals(
      this.bindings,
      o.bindings) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(kind) + 3 * java.util.Objects.hashCode(bindings) + 5 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(LetExpression other) {
    int cmp = 0;
    cmp = ((Comparable) kind).compareTo(other.kind);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) bindings).compareTo(other.bindings);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) body).compareTo(other.body);
  }

  public LetExpression withKind(hydra.ext.lisp.syntax.LetKind kind) {
    return new LetExpression(kind, bindings, body);
  }

  public LetExpression withBindings(hydra.util.ConsList<hydra.ext.lisp.syntax.LetBinding> bindings) {
    return new LetExpression(kind, bindings, body);
  }

  public LetExpression withBody(hydra.util.ConsList<hydra.ext.lisp.syntax.Expression> body) {
    return new LetExpression(kind, bindings, body);
  }
}
