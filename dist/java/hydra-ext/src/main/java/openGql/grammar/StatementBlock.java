// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class StatementBlock implements Serializable, Comparable<StatementBlock> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.StatementBlock");

  public static final hydra.core.Name STATEMENT = new hydra.core.Name("statement");

  public static final hydra.core.Name NEXT_STATEMENTS = new hydra.core.Name("nextStatements");

  public final openGql.grammar.Statement statement;

  public final java.util.List<openGql.grammar.NextStatement> nextStatements;

  public StatementBlock (openGql.grammar.Statement statement, java.util.List<openGql.grammar.NextStatement> nextStatements) {
    this.statement = statement;
    this.nextStatements = nextStatements;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StatementBlock)) {
      return false;
    }
    StatementBlock o = (StatementBlock) other;
    return java.util.Objects.equals(
      this.statement,
      o.statement) && java.util.Objects.equals(
      this.nextStatements,
      o.nextStatements);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(statement) + 3 * java.util.Objects.hashCode(nextStatements);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StatementBlock other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      statement,
      other.statement);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      nextStatements,
      other.nextStatements);
  }

  public StatementBlock withStatement(openGql.grammar.Statement statement) {
    return new StatementBlock(statement, nextStatements);
  }

  public StatementBlock withNextStatements(java.util.List<openGql.grammar.NextStatement> nextStatements) {
    return new StatementBlock(statement, nextStatements);
  }
}
