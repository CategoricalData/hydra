// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class NextStatement implements Serializable, Comparable<NextStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NextStatement");

  public static final hydra.core.Name YIELD_CLAUSE = new hydra.core.Name("yieldClause");

  public static final hydra.core.Name STATEMENT = new hydra.core.Name("statement");

  public final hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yieldClause;

  public final openGql.grammar.Statement statement;

  public NextStatement (hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yieldClause, openGql.grammar.Statement statement) {
    this.yieldClause = yieldClause;
    this.statement = statement;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NextStatement)) {
      return false;
    }
    NextStatement o = (NextStatement) other;
    return java.util.Objects.equals(
      this.yieldClause,
      o.yieldClause) && java.util.Objects.equals(
      this.statement,
      o.statement);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(yieldClause) + 3 * java.util.Objects.hashCode(statement);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NextStatement other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      yieldClause,
      other.yieldClause);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      statement,
      other.statement);
  }

  public NextStatement withYieldClause(hydra.util.Maybe<java.util.List<openGql.grammar.YieldItem>> yieldClause) {
    return new NextStatement(yieldClause, statement);
  }

  public NextStatement withStatement(openGql.grammar.Statement statement) {
    return new NextStatement(yieldClause, statement);
  }
}
