// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class WhileStatementNoShortIf implements Serializable, Comparable<WhileStatementNoShortIf> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.WhileStatementNoShortIf");

  public static final hydra.core.Name COND = new hydra.core.Name("cond");

  public static final hydra.core.Name BODY = new hydra.core.Name("body");

  public final hydra.util.Maybe<hydra.java.syntax.Expression> cond;

  public final hydra.java.syntax.StatementNoShortIf body;

  public WhileStatementNoShortIf (hydra.util.Maybe<hydra.java.syntax.Expression> cond, hydra.java.syntax.StatementNoShortIf body) {
    this.cond = cond;
    this.body = body;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof WhileStatementNoShortIf)) {
      return false;
    }
    WhileStatementNoShortIf o = (WhileStatementNoShortIf) other;
    return java.util.Objects.equals(
      this.cond,
      o.cond) && java.util.Objects.equals(
      this.body,
      o.body);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(cond) + 3 * java.util.Objects.hashCode(body);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(WhileStatementNoShortIf other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      cond,
      other.cond);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      body,
      other.body);
  }

  public WhileStatementNoShortIf withCond(hydra.util.Maybe<hydra.java.syntax.Expression> cond) {
    return new WhileStatementNoShortIf(cond, body);
  }

  public WhileStatementNoShortIf withBody(hydra.java.syntax.StatementNoShortIf body) {
    return new WhileStatementNoShortIf(cond, body);
  }
}
