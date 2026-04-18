// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A catch clause
 */
public class CatchClause implements Serializable, Comparable<CatchClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.CatchClause");

  public static final hydra.core.Name PARAM = new hydra.core.Name("param");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  /**
   * The catch parameter (can be omitted in ES2019+)
   */
  public final hydra.util.Maybe<hydra.javaScript.syntax.Pattern> param;

  public final java.util.List<hydra.javaScript.syntax.Statement> body;

  public CatchClause (hydra.util.Maybe<hydra.javaScript.syntax.Pattern> param, java.util.List<hydra.javaScript.syntax.Statement> body) {
    this.param = param;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CatchClause)) {
      return false;
    }
    CatchClause o = (CatchClause) other;
    return java.util.Objects.equals(
      this.param,
      o.param) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(param) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CatchClause other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      param,
      other.param);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public CatchClause withParam(hydra.util.Maybe<hydra.javaScript.syntax.Pattern> param) {
    return new CatchClause(param, body);
  }

  public CatchClause withBody(java.util.List<hydra.javaScript.syntax.Statement> body) {
    return new CatchClause(param, body);
  }
}
